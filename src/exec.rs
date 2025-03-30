use std::cell::{Ref, RefCell};
use std::collections::BTreeMap;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::marker::PhantomData;
use std::ops::{ControlFlow, Deref};
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::pin::Pin;
use std::rc::Rc;
use std::{fmt, slice};

use crate::syntax::{self, RedirectDest, RedirectMode, RedirectPort, Stmt, WordFrag};

pub mod builtins;
mod utils;

use either::Either;
pub use utils::*;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type ExecResult<T = ()> = Result<T>;

pub type Value = String;
pub type ValueList = Vec<String>;

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

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct VarOpt: u8 {
        const LOCAL     = 0b00;
        const FUNCTION  = 0b01;
        const GLOBAL    = 0b10;
        const UNIVERSAL = 0b11;

        const SCOPES    = 0b11;

        const EXPORT = 0b100;
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum VarScope {
    Local,
    #[default]
    Function,
    Global,
    Universal,
}

impl From<VarScope> for VarOpt {
    fn from(scope: VarScope) -> Self {
        VarOpt::from_bits(scope as u8).expect("always valid")
    }
}

impl VarOpt {
    pub fn scope(self) -> VarScope {
        match self & Self::SCOPES {
            Self::LOCAL => VarScope::Local,
            Self::FUNCTION => VarScope::Function,
            Self::GLOBAL => VarScope::Global,
            Self::UNIVERSAL => VarScope::Universal,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Executor {
    builtin_funcs: BTreeMap<String, Command>,
    funcs: RefCell<BTreeMap<String, Command>>,
    vars: RefCell<BTreeMap<String, ValueList>>,
}

impl Default for Executor {
    fn default() -> Self {
        let mut this = Self::new();
        this.builtin_funcs
            .extend(builtins::all_builtins().map(|(name, cmd)| (name.to_owned(), cmd)));
        this
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            builtin_funcs: BTreeMap::new(),
            funcs: RefCell::new(BTreeMap::new()),
            vars: RefCell::new(BTreeMap::new()),
        }
    }

    pub fn builtins(&self) -> impl ExactSizeIterator<Item = (&str, &Command)> {
        self.builtin_funcs.iter().map(|(name, cmd)| (&**name, cmd))
    }

    pub fn set_builtin(&mut self, name: String, cmd: Command) {
        self.builtin_funcs.insert(name, cmd);
    }

    pub fn get_builtin(&self, name: &str) -> Option<Command> {
        self.builtin_funcs.get(name).cloned()
    }

    pub(crate) fn global_funcs(&self) -> Ref<'_, BTreeMap<String, Command>> {
        self.funcs.borrow()
    }

    pub fn get_global_func(&self, name: &str) -> Option<Command> {
        self.funcs.borrow().get(name).cloned()
    }

    pub fn set_global_func(&self, name: String, cmd: Command) {
        self.funcs.borrow_mut().insert(name, cmd);
    }

    pub fn remove_global_func(&self, name: &str) {
        self.funcs.borrow_mut().remove(name);
    }

    pub fn list_global_vars<B>(
        &self,
        mut f: impl FnMut(&str, &ValueList) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        for (name, vals) in self.vars.borrow().iter() {
            f(name, vals)?;
        }
        ControlFlow::Continue(())
    }

    pub fn get_global_var(&self, name: &str) -> Option<impl Deref<Target = ValueList>> {
        Ref::filter_map(self.vars.borrow(), |m| m.get(name)).ok()
    }

    pub fn set_global_var(&self, name: String, vals: ValueList) {
        self.vars.borrow_mut().insert(name, vals);
    }

    pub fn remove_global_var(&self, name: &str) {
        self.vars.borrow_mut().remove(name);
    }
}

#[derive(Debug)]
pub struct ExecContext<'a> {
    func_vars: BTreeMap<String, ValueList>,
    root: &'a Executor,
    outer: Option<&'a ExecContext<'a>>,
}

impl Deref for ExecContext<'_> {
    type Target = Executor;

    fn deref(&self) -> &Self::Target {
        self.root
    }
}

impl<'a> ExecContext<'a> {
    pub fn new(root: &'a Executor) -> Self {
        Self {
            func_vars: BTreeMap::new(),
            root,
            outer: None,
        }
    }

    pub fn new_inside(outer: &'a ExecContext<'a>) -> Self {
        Self {
            func_vars: BTreeMap::new(),
            root: outer.root,
            outer: Some(outer),
        }
    }

    pub fn backtrace(&self) -> impl Iterator<Item = &Self> {
        std::iter::successors(Some(self), |ctx| ctx.outer)
    }

    pub fn list_funcs<B>(
        &self,
        mut f: impl FnMut(&str, &Command) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        for (name, cmd) in itertools::merge_join_by(
            self.global_funcs()
                .iter()
                .map(|(name, cmd)| (&name[..], cmd)),
            self.builtins(),
            |lhs, rhs| lhs.0.cmp(rhs.0),
        )
        .map(|kv| kv.into_left())
        {
            f(name, cmd)?;
        }
        ControlFlow::Continue(())
    }

    pub fn get_func(&self, name: &str) -> Option<Command> {
        self.get_global_func(name)
            .or_else(|| self.get_builtin(name))
    }

    pub fn list_vars<B>(
        &self,
        scope: VarScope,
        mut f: impl FnMut(&str, &ValueList) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match scope {
            VarScope::Local => todo!(),
            VarScope::Function => {
                for (name, vals) in &self.func_vars {
                    f(name, vals)?;
                }
                ControlFlow::Continue(())
            }
            VarScope::Global => self.list_global_vars(f),
            VarScope::Universal => todo!(),
        }
    }

    pub fn get_var(&self, name: &str) -> Option<impl Deref<Target = ValueList>> {
        match self.func_vars.get(name) {
            Some(vals) => Some(Either::Left(vals)),
            None => self.get_global_var(name).map(Either::Right),
        }
    }

    pub fn set_var(
        &mut self,
        name: impl Into<String>,
        opt: impl Into<VarOpt>,
        vals: impl Into<ValueList>,
    ) {
        let (name, opt, vals) = (name.into(), opt.into(), vals.into());
        if opt.contains(VarOpt::EXPORT) {
            todo!();
        }
        match opt.scope() {
            VarScope::Local => todo!(),
            VarScope::Function => {
                self.func_vars.insert(name, vals);
            }
            VarScope::Global => {
                self.set_global_var(name, vals);
            }
            VarScope::Universal => todo!(),
        }
    }

    pub fn remove_var(&mut self, scope: impl Into<Option<VarScope>>, name: &str) {
        match scope.into() {
            Some(VarScope::Local) => {
                todo!()
            }
            Some(VarScope::Function) => {
                self.func_vars.remove(name);
            }
            Some(VarScope::Global) => {
                self.remove_global_var(name);
            }
            Some(VarScope::Universal) => todo!(),
            None => {
                if self.func_vars.remove(name).is_none() {
                    self.remove_global_var(name);
                }
            }
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
                self.exec_command(&words, io).await
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
                    self.set_var(var, VarScope::default(), [elem]);
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
                self.set_global_func(name.into(), func_cmd);
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

                let mut lhs_ctx = ExecContext::new_inside(self);
                let mut rhs_ctx = ExecContext::new_inside(self);
                let (_ret1, ret2) = tokio::join!(
                    Box::pin(async move { lhs_ctx.exec_stmt(lhs, lhs_io).await }),
                    Box::pin(async move { rhs_ctx.exec_stmt(rhs, rhs_io).await }),
                );
                // TODO: pipestatus
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

    pub async fn exec_command(&mut self, words: &[String], io: Io) -> ExecResult {
        let cmd = words.first().ok_or(Error::EmptyCommand)?;

        if let Some(cmd) = self.get_func(cmd) {
            return cmd.exec(self, words, io).await;
        }

        let args = builtins::CommandArgs {
            all: false,
            query: false,
            search: false,
            args: words.to_vec(),
        };
        builtins::command(self, args, io).await
    }

    // TODO: Word and size limit.
    async fn expand_words(&mut self, words: &[syntax::Word]) -> Result<Vec<String>> {
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
                syntax::Word::Simple(w) => {
                    ret.push(w.clone());
                    continue;
                }
                syntax::Word::Complex(frags) => frags,
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
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
        io: Io,
    ) -> BoxFuture<'fut, ExecResult>;
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

    pub fn new_native<F>(func: F) -> Self
    where
        F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, &[String], Io) -> ExecResult,
    {
        #[derive(Clone)]
        struct Native<F>(F);

        impl<F> DynCommand for Native<F>
        where
            F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, &[String], Io) -> ExecResult,
        {
            fn exec<'fut>(
                &'fut self,
                ctx: &'fut mut ExecContext<'_>,
                args: &'fut [String],
                io: Io,
            ) -> BoxFuture<'fut, ExecResult> {
                Box::pin(self.0(ctx, args, io))
            }
        }

        Self::new(Box::new(Native(func)))
    }

    pub fn new_native_parsed<F, Args>(func: F) -> Self
    where
        F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, Args, Io) -> ExecResult,
        Args: 'static + argh::FromArgs,
    {
        struct Native<F, Args>(F, PhantomData<fn(Args)>);

        impl<F: Clone, Args> Clone for Native<F, Args> {
            fn clone(&self) -> Self {
                Self(self.0.clone(), PhantomData)
            }
        }

        impl<F, Args> DynCommand for Native<F, Args>
        where
            F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, Args, Io) -> ExecResult,
            Args: 'static + argh::FromArgs,
        {
            fn exec<'fut>(
                &'fut self,
                ctx: &'fut mut ExecContext<'_>,
                args: &'fut [String],
                io: Io,
            ) -> BoxFuture<'fut, ExecResult> {
                let strs = args.iter().map(|s| s.as_str()).collect::<Vec<_>>();
                match <Args as argh::FromArgs>::from_args(&[strs[0]], &strs[1..]) {
                    Ok(parsed) => Box::pin((self.0)(ctx, parsed, io)),
                    Err(_) => todo!(),
                }
            }
        }

        Self::new(Box::new(Native(func, PhantomData)))
    }

    pub fn new_function(stmt: Stmt) -> Self {
        #[derive(Clone)]
        struct Func(Rc<Stmt>);

        impl DynCommand for Func {
            fn exec<'fut>(
                &'fut self,
                ctx: &'fut mut ExecContext<'_>,
                args: &'fut [String],
                io: Io,
            ) -> BoxFuture<'fut, ExecResult> {
                Box::pin(async move {
                    let mut subctx = ExecContext::new_inside(ctx);
                    subctx.set_var("argv", VarScope::Function, args[1..].to_vec());
                    subctx.exec_stmt(&self.0, io).await
                })
            }
        }

        Self::new(Box::new(Func(Rc::new(stmt))))
    }

    pub fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
        io: Io,
    ) -> impl Future<Output = ExecResult> + use<'fut> {
        self.func.exec(ctx, args, io)
    }
}
