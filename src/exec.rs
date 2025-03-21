use std::collections::HashMap;
use std::fmt;
use std::fs::OpenOptions;
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::pin::Pin;
use std::rc::Rc;

use tokio::process::Command as OsCommand;

use crate::syntax::{RedirectDest, RedirectMode, RedirectPort, Stmt, Word};

pub mod builtins;
mod utils;

pub use utils::*;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type ExecResult<T = ()> = Result<T>;

pub type Bytes = Vec<u8>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("command exited with {0}")]
    ExitCode(i32),

    #[error("{0}")]
    Custom(String),

    #[error("invalid identifier: {0:?}")]
    InvalidateIdentifier(String),
    #[error("expecting an identifier, found a list with {0} elements")]
    InvalidateIdentifierWords(usize),
    #[error("empty command")]
    EmptyCommand,
    #[error("command not found: {0:?}")]
    CommandNotFound(String),
    #[error("invalid redirection target")]
    InvalidRedirectionTarget,
    #[error("failed open redirection file {0:?}: {1}")]
    OpenRedirectionFile(PathBuf, std::io::Error),
    #[error("failed to spawn process {0:?}: {1}")]
    SpawnProcess(PathBuf, std::io::Error),

    #[error("failed to create pipe: {0}")]
    CreatePipe(std::io::Error),
    #[error("failed to clone fd: {0}")]
    CloneHandle(std::io::Error),
    #[error("failed to wait process: {0}")]
    WaitProcess(std::io::Error),
}

#[derive(Default, Debug, Clone)]
pub struct Io {
    pub stdin: Stdio,
    pub stdout: Stdio,
    pub stderr: Stdio,
}

#[derive(Default, Debug, Clone)]
pub enum Stdio {
    #[default]
    Inherit,
    Close,
    Raw(Rc<OwnedFd>),
}

#[derive(Debug)]
pub struct Executor {
    builtin_funcs: HashMap<String, Command>,
}

impl Default for Executor {
    fn default() -> Self {
        Self {
            // TODO: Avoid string allocations.
            builtin_funcs: builtins::ALL_BUILTINS
                .into_iter()
                .map(|(name, cmd)| (name.to_owned(), cmd))
                .collect(),
        }
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            builtin_funcs: HashMap::new(),
        }
    }

    pub fn add_builtin_func(&mut self, name: String, cmd: Command) {
        self.builtin_funcs.insert(name, cmd);
    }
}

#[derive(Debug)]
pub struct ExecContext<'a, 'b> {
    funcs: HashMap<String, Option<Command>>,
    vars: HashMap<String, Option<Vec<String>>>,
    root: &'b Executor,
    outer: Option<&'a ExecContext<'a, 'b>>,
}

impl<'b> ExecContext<'_, 'b> {
    pub fn new(root: &'b Executor) -> Self {
        Self {
            funcs: HashMap::new(),
            vars: HashMap::new(),
            root,
            outer: None,
        }
    }

    pub fn fork(&self) -> ExecContext<'_, 'b> {
        ExecContext {
            funcs: HashMap::new(),
            vars: HashMap::new(),
            root: self.root,
            outer: Some(self),
        }
    }

    pub fn cut<'c>(self) -> ExecContext<'c, 'b> {
        ExecContext {
            funcs: self.funcs,
            vars: self.vars,
            root: self.root,
            outer: None,
        }
    }

    async fn fork_exec(&self, stmt: &Stmt, io: Io) -> (ExecContext<'_, 'b>, ExecResult) {
        let mut subctx = self.fork();
        let ret = subctx.exec_stmt(stmt, io).await;
        (subctx, ret)
    }

    pub fn merge(&mut self, rhs: ExecContext<'_, '_>) {
        for (name, cmd) in rhs.funcs {
            match cmd {
                Some(cmd) => self.set_func(name, cmd),
                None => self.remove_func(&name),
            }
        }
        for (name, vals) in rhs.vars {
            match vals {
                Some(vals) => self.set_var(name, vals),
                None => self.remove_var(&name),
            }
        }
    }

    pub fn get_func(&self, name: &str) -> Option<&Command> {
        match (self.funcs.get(name), self.outer) {
            // Defined locally.
            (Some(Some(cmd)), _) => Some(cmd),
            // Inherited.
            (None, Some(outer)) => outer.get_func(name),
            // Unset locally, or not set at all.
            (Some(None), _) | (None, None) => self.root.builtin_funcs.get(name),
        }
    }

    pub fn set_func(&mut self, name: impl Into<String>, cmd: Command) {
        self.funcs.insert(name.into(), Some(cmd));
    }

    pub fn remove_func(&mut self, name: &str) {
        if self.outer.is_none() {
            self.funcs.remove(name);
        } else {
            self.funcs.insert(name.to_owned(), None);
        }
    }

    pub fn get_var(&self, name: &str) -> Option<&[String]> {
        match (self.vars.get(name), self.outer) {
            // Defined locally.
            (Some(Some(vals)), _) => Some(vals),
            // Inherited.
            (None, Some(outer)) => outer.get_var(name),
            // Unset locally, or not set at all.
            // TODO: Builtin variables.
            (Some(None), _) | (None, None) => None,
        }
    }

    pub fn set_var(&mut self, name: impl Into<String>, values: impl IntoIterator<Item = String>) {
        self.vars
            .insert(name.into(), Some(values.into_iter().collect()));
    }

    pub fn remove_var(&mut self, name: &str) {
        if self.outer.is_none() {
            self.vars.remove(name);
        } else {
            self.vars.insert(name.to_owned(), None);
        }
    }

    fn exec_stmt_boxed(&mut self, stmt: &Stmt, io: Io) -> impl Future<Output = ExecResult> {
        Box::pin(self.exec_stmt(stmt, io))
    }

    pub async fn exec_stmts(&mut self, stmts: &[Stmt], io: Io) -> ExecResult {
        let Some((last, init)) = stmts.split_last() else {
            return Ok(());
        };
        for s in init {
            let _: Result<_> = self.exec_stmt_boxed(s, io.clone()).await;
        }
        self.exec_stmt(last, io).await
    }

    pub async fn exec_stmt(&mut self, stmt: &Stmt, mut io: Io) -> ExecResult {
        match stmt {
            Stmt::Command(words) => {
                let words = self.expand_words(words).await?;
                self.exec_cmd(&words, io).await
            }
            Stmt::Block(stmts) => Box::pin(self.exec_stmts(stmts, io)).await,
            Stmt::If(cond, then, else_) => {
                let cond = self.exec_stmt_boxed(cond, io.clone()).await.is_ok();
                if cond {
                    self.exec_stmt_boxed(then, io).await
                } else if let Some(else_) = else_ {
                    self.exec_stmt_boxed(else_, io).await
                } else {
                    Ok(())
                }
            }
            Stmt::While(cond, body) => loop {
                self.exec_stmt_boxed(cond, io.clone()).await?;
                let _ret = self.exec_stmt_boxed(body, io.clone()).await;
            },
            Stmt::For(var, elem_ws, body) => {
                let var = self.expand_words(std::slice::from_ref(var)).await?;
                let var = validate_variable_words(&var)?;
                let elems = self.expand_words(elem_ws).await?;
                for elem in elems {
                    self.set_var(var, [elem]);
                    self.exec_stmt_boxed(body, io.clone()).await?;
                }
                Ok(())
            }
            Stmt::Function(..) => todo!(),
            Stmt::Redirect(stmt, redirects) => {
                for redir in redirects {
                    let (RedirectDest::File(file_word) | RedirectDest::Fd(file_word)) = &redir.dest;
                    let expanded = self.expand_words(std::slice::from_ref(file_word)).await?;
                    let [file_path] = &*expanded else {
                        return Err(Error::InvalidRedirectionTarget);
                    };
                    match redir.dest {
                        RedirectDest::File(_) => {}
                        RedirectDest::Fd(_) => todo!(),
                    }
                    let mut opt = OpenOptions::new();
                    match redir.mode {
                        RedirectMode::Read | RedirectMode::ReadOrNull => opt.read(true),
                        RedirectMode::Write => opt.write(true).create(true),
                        RedirectMode::WriteNoClobber => opt.write(true).create_new(true),
                        RedirectMode::Append => opt.append(true).create(true),
                    };
                    let f = opt
                        .open(file_path)
                        .map_err(|err| Error::OpenRedirectionFile(file_path.into(), err))?;
                    let f = Stdio::Raw(Rc::new(OwnedFd::from(f)));

                    match redir.port {
                        RedirectPort::STDIN => io.stdin = f,
                        RedirectPort::STDOUT => io.stdout = f,
                        RedirectPort::STDOUT_STDERR => (io.stdout, io.stderr) = (f.clone(), f),
                        _ => todo!(),
                    }
                }

                self.exec_stmt_boxed(stmt, io).await
            }
            Stmt::Pipe(port, lhs, rhs) => {
                let (pipe_r, pipe_w) = os_pipe::pipe().map_err(Error::CreatePipe)?;
                let pipe_r = Stdio::Raw(Rc::new(pipe_r.into()));
                let pipe_w = Stdio::Raw(Rc::new(pipe_w.into()));

                let mut lhs_io = io.clone();
                match *port {
                    RedirectPort::STDOUT => lhs_io.stdout = pipe_w,
                    RedirectPort::STDERR => lhs_io.stderr = pipe_w,
                    RedirectPort::STDOUT_STDERR => {
                        (lhs_io.stdout, lhs_io.stderr) = (pipe_w.clone(), pipe_w)
                    }
                    _ => todo!(),
                }
                let rhs_io = Io {
                    stdin: pipe_r,
                    ..io.clone()
                };

                let ((ctx1, _ret1), (ctx2, ret2)) = tokio::join!(
                    Box::pin(self.fork_exec(lhs, lhs_io)),
                    Box::pin(self.fork_exec(rhs, rhs_io)),
                );
                let (ctx1, ctx2) = (ctx1.cut(), ctx2.cut());
                self.merge(ctx1);
                self.merge(ctx2);
                ret2
            }
            Stmt::Not(stmt) => match self.exec_stmt_boxed(stmt, io).await {
                Ok(()) => Err(Error::ExitCode(0)),
                Err(_) => Ok(()),
            },
            Stmt::And(lhs, rhs) => {
                self.exec_stmt_boxed(lhs, io.clone()).await?;
                self.exec_stmt_boxed(rhs, io).await
            }
            Stmt::Or(lhs, rhs) => match self.exec_stmt_boxed(lhs, io.clone()).await {
                Ok(()) => Ok(()),
                Err(_) => self.exec_stmt_boxed(rhs, io).await,
            },
        }
    }

    pub async fn exec_cmd(&mut self, words: &[String], io: Io) -> ExecResult {
        let cmd = words.first().ok_or(Error::EmptyCommand)?;

        // NB. The command borrows `self` but may modify the `self` during execution.
        // We need to clone it first.
        if let Some(cmd) = self.get_func(cmd).cloned() {
            return cmd.exec(self, words, io).await;
        }

        self.exec_external_cmd(words, io).await
    }

    pub async fn exec_external_cmd(&self, words: &[String], io: Io) -> ExecResult {
        use std::process::Stdio as ProcessStdio;

        let (cmd, args) = words.split_first().ok_or(Error::EmptyCommand)?;

        let cvt_stdio = |s: Stdio| {
            Ok(match s {
                Stdio::Inherit => ProcessStdio::inherit(),
                Stdio::Close => todo!(),
                Stdio::Raw(raw) => Rc::try_unwrap(raw)
                    .or_else(|raw| raw.try_clone().map_err(Error::CloneHandle))?
                    .into(),
            })
        };

        let mut child = OsCommand::new(cmd)
            .args(args)
            .stdin(cvt_stdio(io.stdin)?)
            .stdout(cvt_stdio(io.stdout)?)
            .stderr(cvt_stdio(io.stderr)?)
            .spawn()
            .map_err(|err| Error::SpawnProcess(cmd.into(), err))?;
        let status = child.wait().await.map_err(Error::WaitProcess)?;

        if status.success() {
            Ok(())
        } else {
            #[cfg(unix)]
            let code = {
                use std::os::unix::process::ExitStatusExt;
                status.into_raw()
            };

            Err(Error::ExitCode(code))
        }
    }

    async fn expand_words(&self, words: &[Word]) -> Result<Vec<String>> {
        let mut ret = Vec::new();

        for w in words {
            match w {
                Word::Simple(w) => ret.push(w.clone()),
                Word::Complex(_) => todo!(),
            }
        }

        Ok(ret)
    }
}

type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

pub trait DynCommand:
    'static
    + dyn_clone::DynClone
    + for<'e, 'a, 'b> Fn(&'e mut ExecContext<'a, 'b>, &'e [String], Io) -> BoxFuture<'e, ExecResult>
{
}
impl<F> DynCommand for F where
    F: ?Sized
        + 'static
        + dyn_clone::DynClone
        + for<'e, 'a, 'b> Fn(
            &'e mut ExecContext<'a, 'b>,
            &'e [String],
            Io,
        ) -> BoxFuture<'e, ExecResult>
{
}

dyn_clone::clone_trait_object!(DynCommand);

#[derive(Clone)]
pub struct Command {
    func: Box<dyn DynCommand>,
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Command").finish_non_exhaustive()
    }
}

impl Command {
    pub const fn new(func: Box<dyn DynCommand>) -> Self {
        Self { func }
    }

    const fn new_zst_fn<F: DynCommand + Copy>(_func: F) -> Self {
        const { assert!(size_of::<F>() == 0) };
        // SAFETY: `Box<ZST>` can be constructed via a aligned non-null dangling pointer.
        // Ref: <https://doc.rust-lang.org/stable/std/boxed/index.html#memory-layout>
        let b = unsafe { std::mem::transmute::<*mut F, Box<F>>(std::ptr::dangling_mut()) };
        Self::new(b)
    }

    pub fn exec<'e, 'a, 'b>(
        &self,
        ctx: &'e mut ExecContext<'a, 'b>,
        args: &'e [String],
        io: Io,
    ) -> impl Future<Output = ExecResult> + use<'e, 'a, 'b> {
        (self.func)(ctx, args, io)
    }
}
