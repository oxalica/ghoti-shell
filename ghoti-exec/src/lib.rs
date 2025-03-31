use std::cell::{Ref, RefCell};
use std::collections::BTreeMap;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::ops::{ControlFlow, Deref};
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::rc::Rc;
use std::{fmt, slice};

use builtins::FunctionOpts;
use command::{BoxCommand, Command, UserFunc};
use either::Either;
use ghoti_syntax::{self as ast, RedirectDest, RedirectMode, RedirectPort, Stmt, WordFrag};

use crate::utils::{validate_function_name, validate_variable_words};

pub mod builtins;
pub mod command;
mod utils;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type ExecResult<T = ExitStatus> = Result<T>;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExitStatus(pub i32);

impl ExitStatus {
    pub fn is_success(self) -> bool {
        self == Self::SUCCESS
    }
}

impl ExitStatus {
    pub const SUCCESS: Self = Self(0);
    pub const FAILURE: Self = Self(1);
}

impl fmt::Display for ExitStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<i32> for ExitStatus {
    fn from(n: i32) -> Self {
        Self(n)
    }
}

impl From<bool> for ExitStatus {
    fn from(b: bool) -> Self {
        match b {
            true => Self::SUCCESS,
            false => Self::FAILURE,
        }
    }
}

pub type Value = String;
pub type ValueList = Vec<String>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Custom(String),
    #[error("io error: {0}")]
    Io(std::io::Error),

    #[error("invalid options: {0}")]
    InvalidOptions(String),
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

impl Error {
    pub fn to_status(&self) -> ExitStatus {
        // TODO
        ExitStatus::FAILURE
    }
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
            Stdio::Inherit => {
                std::io::stdout()
                    .lock()
                    .write_fmt(format_args!("{s}"))
                    .map_err(Error::Io)?;
                Ok(ExitStatus::SUCCESS)
            }
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
    builtin_funcs: BTreeMap<String, Box<dyn Command>>,
    funcs: RefCell<BTreeMap<String, Box<dyn Command>>>,
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

    pub fn builtins(&self) -> impl ExactSizeIterator<Item = (&str, &BoxCommand)> {
        self.builtin_funcs.iter().map(|(name, cmd)| (&**name, cmd))
    }

    pub fn set_builtin(&mut self, name: String, cmd: impl Command) {
        self.builtin_funcs.insert(name, Box::new(cmd));
    }

    pub fn get_builtin(&self, name: &str) -> Option<BoxCommand> {
        self.builtin_funcs.get(name).cloned()
    }

    pub(crate) fn global_funcs(&self) -> Ref<'_, BTreeMap<String, BoxCommand>> {
        self.funcs.borrow()
    }

    pub fn get_global_func(&self, name: &str) -> Option<BoxCommand> {
        self.funcs.borrow().get(name).cloned()
    }

    pub fn set_global_func(&self, name: String, cmd: impl Command) {
        self.funcs.borrow_mut().insert(name, Box::new(cmd));
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
    last_status: ExitStatus,
    io: Io,
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
            last_status: ExitStatus::SUCCESS,
            io: Io::default(),
        }
    }

    pub fn new_inside(outer: &'a ExecContext<'a>) -> Self {
        Self {
            func_vars: BTreeMap::new(),
            root: outer.root,
            outer: Some(outer),
            last_status: ExitStatus::SUCCESS,
            io: outer.io.clone(),
        }
    }

    pub fn last_status(&self) -> ExitStatus {
        self.last_status
    }

    pub fn set_last_status(&mut self, n: impl Into<ExitStatus>) {
        self.last_status = n.into();
    }

    pub fn io(&self) -> &Io {
        &self.io
    }

    pub fn backtrace(&self) -> impl Iterator<Item = &Self> {
        std::iter::successors(Some(self), |ctx| ctx.outer)
    }

    pub fn list_funcs<B>(
        &self,
        mut f: impl FnMut(&str, &BoxCommand) -> ControlFlow<B>,
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

    pub fn get_func(&self, name: &str) -> Option<BoxCommand> {
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

    pub async fn exec_stmts(&mut self, stmts: &[Stmt]) -> ExecResult {
        let mut last = ExitStatus::SUCCESS;
        for s in stmts {
            last = self.exec_stmt(s).await?;
        }
        Ok(last)
    }

    pub async fn exec_stmt(&mut self, stmt: &Stmt) -> ExecResult {
        let ret = Box::pin(self.exec_stmt_inner(stmt)).await;
        let st = match &ret {
            Ok(n) => *n,
            Err(err) => err.to_status(),
        };
        self.set_last_status(st);
        ret
    }

    async fn exec_stmt_inner(&mut self, stmt: &Stmt) -> ExecResult {
        match stmt {
            Stmt::Command(words) => {
                let words = self.expand_words(words).await?;
                self.exec_command(&words).await
            }
            Stmt::Block(stmts) => Box::pin(self.exec_stmts(stmts)).await,
            Stmt::If(cond, then, else_) => {
                self.exec_stmt(cond).await?;
                if self.last_status().is_success() {
                    self.exec_stmt(then).await
                } else if let Some(else_) = else_ {
                    self.exec_stmt(else_).await
                } else {
                    Ok(ExitStatus::SUCCESS)
                }
            }
            Stmt::Switch(_, _) => todo!(),
            Stmt::While(cond, body) => loop {
                self.exec_stmt(cond).await?;
                if !self.last_status().is_success() {
                    break Ok(self.last_status());
                }
                self.exec_stmt(body).await?;
            },
            Stmt::For(var, elem_ws, body) => {
                let var = self.expand_words(slice::from_ref(var)).await?;
                let var = validate_variable_words(&var)?;
                let elems = self.expand_words(elem_ws).await?;
                for elem in elems {
                    self.set_var(var, VarScope::default(), [elem]);
                    self.exec_stmt(body).await?;
                }
                Ok(self.last_status())
            }
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Function(words, stmt) => {
                let words = &*self.expand_words(words).await?;
                let name = words.first().ok_or(Error::InvalidateIdentifierWords(0))?;
                validate_function_name(name)?;
                let strs = words[1..].iter().map(|s| s.as_str()).collect::<Vec<_>>();
                let desc = match <FunctionOpts as argh::FromArgs>::from_args(&[name], &strs) {
                    Ok(opts) => opts.description,
                    Err(err) => return Err(Error::InvalidOptions(err.output)),
                };
                let user_func = UserFunc::new((**stmt).clone(), desc);
                self.set_global_func(name.into(), user_func);
                Ok(ExitStatus::SUCCESS)
            }
            Stmt::Return(_) => todo!(),
            Stmt::Redirect(stmt, redirects) => {
                let prev_io = self.io().clone();
                let mut this = scopeguard::guard(&mut *self, |this| this.io = prev_io);

                for redir in redirects {
                    let (RedirectDest::File(file_word) | RedirectDest::Fd(file_word)) = &redir.dest;
                    let expanded = this.expand_words(slice::from_ref(file_word)).await?;
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
                        RedirectPort::STDIN => this.io.stdin = f,
                        RedirectPort::STDOUT => this.io.stdout = f,
                        RedirectPort::STDOUT_STDERR => {
                            (this.io.stdout, this.io.stderr) = (f.clone(), f)
                        }
                        _ => todo!(),
                    }
                }

                this.exec_stmt(stmt).await
            }
            Stmt::Pipe(port, lhs, rhs) => {
                let (pipe_r, pipe_w) = os_pipe::pipe().map_err(Error::CreatePipe)?;
                let pipe_r = Stdio::Raw(Rc::new(pipe_r.into()));
                let pipe_w = Stdio::Raw(Rc::new(pipe_w.into()));

                let mut lctx = ExecContext::new_inside(self);
                let mut rctx = ExecContext::new_inside(self);
                match *port {
                    RedirectPort::STDOUT => lctx.io.stdout = pipe_w,
                    RedirectPort::STDERR => lctx.io.stderr = pipe_w,
                    RedirectPort::STDOUT_STDERR => {
                        (lctx.io.stdout, lctx.io.stderr) = (pipe_w.clone(), pipe_w)
                    }
                    _ => todo!(),
                }
                rctx.io.stdin = pipe_r;

                let (ret1, ret2) = tokio::join!(
                    Box::pin(async move { lctx.exec_stmt(lhs).await }),
                    Box::pin(async move { rctx.exec_stmt(rhs).await }),
                );
                // TODO: pipestatus
                ret1?;
                ret2
            }
            Stmt::Not(stmt) => {
                let n = self.exec_stmt(stmt).await?;
                Ok((!n.is_success()).into())
            }
            Stmt::And(stmt) => {
                if self.last_status.is_success() {
                    self.exec_stmt(stmt).await
                } else {
                    Ok(self.last_status)
                }
            }
            Stmt::Or(stmt) => {
                if self.last_status.is_success() {
                    Ok(self.last_status)
                } else {
                    self.exec_stmt(stmt).await
                }
            }
        }
    }

    pub async fn exec_command(&mut self, words: &[String]) -> ExecResult {
        let cmd = words.first().ok_or(Error::EmptyCommand)?;

        if let Some(cmd) = self.get_func(cmd) {
            return cmd.exec(self, words).await;
        }

        let args = builtins::CommandArgs {
            all: false,
            query: false,
            search: false,
            args: words.to_vec(),
        };
        builtins::command(self, args).await
    }

    // TODO: Word and size limit.
    async fn expand_words(&mut self, words: &[ast::Word]) -> Result<Vec<String>> {
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
                ast::Word::Simple(w) => {
                    ret.push(w.clone());
                    continue;
                }
                ast::Word::Complex(frags) => frags,
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
                        let buf = {
                            let buf = Rc::new(RefCell::new(Vec::new()));
                            let buf_weak = Rc::downgrade(&buf);
                            // FIXME: Avoid double Rc?
                            let io_collect = Stdio::Collect(Rc::new(move |bytes| {
                                buf_weak
                                    .upgrade()
                                    .ok_or(Error::PipeClosed)?
                                    .borrow_mut()
                                    .extend_from_slice(bytes);
                                Ok(ExitStatus::SUCCESS)
                            }));
                            let prev_stdout = std::mem::replace(&mut self.io.stdout, io_collect);
                            let mut this = scopeguard::guard(&mut *self, move |this| {
                                this.io.stdout = prev_stdout
                            });
                            // TODO: check status?
                            this.exec_stmt(stmt).await?;
                            Rc::into_inner(buf).unwrap().into_inner()
                        };
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
