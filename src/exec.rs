use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::pin::Pin;
use std::rc::Rc;
use std::{fmt, slice};

use crate::syntax::{RedirectDest, RedirectMode, RedirectPort, Stmt, Word, WordFrag};

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
    #[error("io error: {0}")]
    Io(std::io::Error),

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
    OpenRedirectionFile(PathBuf, io::Error),
    #[error("failed to spawn process {0:?}: {1}")]
    SpawnProcess(PathBuf, io::Error),
    #[error("variable not found: {0:?}")]
    VariableNotFound(String),
    #[error("pipe closed")]
    PipeClosed,

    #[error("failed to create pipe: {0}")]
    CreatePipe(io::Error),
    #[error("failed to clone fd: {0}")]
    CloneHandle(io::Error),
    #[error("failed to wait process: {0}")]
    WaitProcess(io::Error),
}

#[derive(Default, Debug, Clone)]
pub struct Io {
    pub stdin: Stdio,
    pub stdout: Stdio,
    pub stderr: Stdio,
}

pub type StdioCollectSink = Rc<dyn Fn(&[u8]) -> ExecResult>;

#[derive(Default, Clone)]
pub enum Stdio {
    #[default]
    Inherit,
    Close,
    Collect(StdioCollectSink),
    Raw(Rc<OwnedFd>),
}

impl fmt::Debug for Stdio {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inherit => write!(f, "Inherit"),
            Self::Close => write!(f, "Close"),
            Self::Collect(_) => f.debug_tuple("Collect").finish_non_exhaustive(),
            Self::Raw(fd) => f.debug_tuple("Raw").field(fd).finish(),
        }
    }
}

impl Io {
    pub fn write_stdout(&self, s: impl fmt::Display) -> ExecResult {
        match &self.stdout {
            // FIXME: how to async write stdout?
            Stdio::Inherit => std::io::stdout()
                .lock()
                .write_fmt(format_args!("{s}"))
                .map_err(Error::Io),
            Stdio::Close => Err(Error::PipeClosed),
            Stdio::Collect(sink) => sink(s.to_string().as_bytes()),
            Stdio::Raw(_) => todo!(),
        }
    }
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

    pub fn builtin_funcs(&self) -> impl ExactSizeIterator<Item = (&str, &Command)> {
        self.root
            .builtin_funcs
            .iter()
            .map(|(name, cmd)| (&**name, cmd))
    }

    pub fn get_builtin_func(&self, name: &str) -> Option<&Command> {
        self.root.builtin_funcs.get(name)
    }

    pub fn get_func(&self, name: &str) -> Option<&Command> {
        match (self.funcs.get(name), self.outer) {
            // Defined locally.
            (Some(Some(cmd)), _) => Some(cmd),
            // Inherited.
            (None, Some(outer)) => outer.get_func(name),
            // Unset locally, or not set at all.
            (Some(None), _) | (None, None) => self.get_builtin_func(name),
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
                let var = self.expand_words(slice::from_ref(var)).await?;
                let var = validate_variable_words(&var)?;
                let elems = self.expand_words(elem_ws).await?;
                for elem in elems {
                    self.set_var(var, [elem]);
                    self.exec_stmt_boxed(body, io.clone()).await?;
                }
                Ok(())
            }
            Stmt::Function(words, stmt) => {
                let words = &*self.expand_words(words).await?;
                let [name] = words else {
                    return Err(Error::InvalidateIdentifierWords(words.len()));
                };
                validate_function_name(name)?;
                let func_cmd = Command::new_function((**stmt).clone());
                self.set_func(name, func_cmd);
                Ok(())
            }
            Stmt::Redirect(stmt, redirects) => {
                for redir in redirects {
                    let (RedirectDest::File(file_word) | RedirectDest::Fd(file_word)) = &redir.dest;
                    let expanded = self.expand_words(slice::from_ref(file_word)).await?;
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
        let (cmd, args) = words.split_first().ok_or(Error::EmptyCommand)?;

        // NB. The command borrows `self` but may modify the `self` during execution.
        // We need to clone it first.
        if let Some(cmd) = self.get_func(cmd).cloned() {
            return cmd.exec(self, args, io).await;
        }

        builtins::command(self, words, io).await
    }

    // TODO: Word and size limit.
    async fn expand_words(&mut self, words: &[Word]) -> Result<Vec<String>> {
        fn dfs(
            ret: &mut Vec<String>,
            stack: &mut String,
            frags: &[WordFrag],
            expanded: &[Vec<String>],
        ) {
            let Some((frag, rest_frags)) = frags.split_first() else {
                ret.push(stack.clone());
                return;
            };
            let prev_len = stack.len();
            match frag {
                WordFrag::Literal(s) => {
                    stack.push_str(s);
                    dfs(ret, stack, rest_frags, expanded);
                    stack.truncate(prev_len);
                }
                WordFrag::Variable(_)
                | WordFrag::VariableNoSplit(_)
                | WordFrag::Command(_)
                | WordFrag::CommandNoSplit(_)
                | WordFrag::Brace(_) => {
                    let (words, rest_computed) = expanded.split_first().unwrap();
                    for w in words {
                        stack.push_str(w);
                        dfs(ret, stack, rest_frags, rest_computed);
                        stack.truncate(prev_len);
                    }
                }
                WordFrag::TildeSegment | WordFrag::Wildcard | WordFrag::WildcardRecursive => {
                    todo!()
                }
            }
        }

        let mut ret = Vec::new();

        for w in words {
            let frags = match w {
                Word::Simple(w) => {
                    ret.push(w.clone());
                    continue;
                }
                Word::Complex(frags) => frags,
            };

            let mut expanded = Vec::with_capacity(frags.len());
            for frag in frags {
                match frag {
                    WordFrag::Literal(_) => {}
                    WordFrag::Variable(var) | WordFrag::VariableNoSplit(var) => {
                        let vals = self
                            .get_var(var)
                            .ok_or_else(|| Error::VariableNotFound(var.into()))?;
                        if matches!(frag, WordFrag::Variable(_)) {
                            expanded.push(vals.to_vec());
                        } else {
                            expanded.push(vec![vals.join(" ")]);
                        }
                    }
                    WordFrag::Command(stmt) | WordFrag::CommandNoSplit(stmt) => {
                        let buf = Rc::new(RefCell::new(Vec::new()));
                        let buf_weak = Rc::downgrade(&buf);
                        let io = Io {
                            // FIXME: Avoid double Rc?
                            stdout: Stdio::Collect(Rc::new(move |bytes| {
                                buf_weak
                                    .upgrade()
                                    .ok_or(Error::PipeClosed)?
                                    .borrow_mut()
                                    .extend_from_slice(bytes);
                                Ok(())
                            })),
                            ..Io::default()
                        };
                        // TODO: check status?
                        let _ret = self.exec_stmt_boxed(stmt, io).await;
                        let buf = Rc::into_inner(buf).unwrap().into_inner();
                        let mut buf = String::from_utf8(buf).expect("TODO");

                        if let WordFrag::CommandNoSplit(_) = frag {
                            let len = buf.trim_end_matches('\n').len();
                            buf.truncate(len);
                            expanded.push(vec![buf]);
                        } else {
                            expanded.push(buf.lines().map(|s| s.to_owned()).collect());
                        }
                    }
                    WordFrag::Brace(words) => {
                        let mut alts = Vec::with_capacity(words.len());
                        for w in words {
                            let mut vals = Box::pin(self.expand_words(slice::from_ref(w))).await?;
                            if vals.len() != 1 {
                                todo!();
                            }
                            alts.push(vals.pop().unwrap());
                        }
                        expanded.push(alts);
                    }
                    WordFrag::TildeSegment | WordFrag::Wildcard | WordFrag::WildcardRecursive => {
                        todo!()
                    }
                }
            }

            dfs(&mut ret, &mut String::new(), frags, &expanded);
        }

        Ok(ret)
    }
}

type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

pub trait DynCommand: 'static + dyn_clone::DynClone {
    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_, '_>,
        args: &'fut [String],
        io: Io,
    ) -> BoxFuture<'fut, ExecResult>;
}

impl<F> DynCommand for F
where
    F: 'static
        + Clone
        + for<'e, 'a, 'b> Fn(
            &'e mut ExecContext<'a, 'b>,
            &'e [String],
            Io,
        ) -> BoxFuture<'e, ExecResult>,
{
    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_, '_>,
        args: &'fut [String],
        io: Io,
    ) -> BoxFuture<'fut, ExecResult> {
        self(ctx, args, io)
    }
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

    const fn new_zst_fn<F>(_func: F) -> Self
    where
        F: 'static
            + Copy
            + for<'e, 'a, 'b> Fn(
                &'e mut ExecContext<'a, 'b>,
                &'e [String],
                Io,
            ) -> BoxFuture<'e, ExecResult>,
    {
        const { assert!(size_of::<F>() == 0) };
        // SAFETY: `Box<ZST>` can be constructed via a aligned non-null dangling pointer.
        // Ref: <https://doc.rust-lang.org/stable/std/boxed/index.html#memory-layout>
        let b = unsafe { std::mem::transmute::<*mut F, Box<F>>(std::ptr::dangling_mut()) };
        Self::new(b)
    }

    pub fn new_function(stmt: Stmt) -> Self {
        #[derive(Clone)]
        struct Func(Rc<Stmt>);

        impl DynCommand for Func {
            fn exec<'fut>(
                &'fut self,
                ctx: &'fut mut ExecContext<'_, '_>,
                args: &'fut [String],
                io: Io,
            ) -> BoxFuture<'fut, ExecResult> {
                if !args.is_empty() {
                    todo!();
                }
                Box::pin(ctx.exec_stmt(&self.0, io))
            }
        }

        Self::new(Box::new(Func(Rc::new(stmt))))
    }

    pub fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_, '_>,
        args: &'fut [String],
        io: Io,
    ) -> impl Future<Output = ExecResult> + use<'fut> {
        self.func.exec(ctx, args, io)
    }
}
