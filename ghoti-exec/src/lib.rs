use std::cell::{Ref, RefCell};
use std::collections::BTreeMap;
use std::fs::OpenOptions;
use std::io;
use std::ops::{ControlFlow, Deref, DerefMut};
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::rc::Rc;
use std::{fmt, slice};

use builtins::FunctionOpts;
use command::{BoxCommand, Command, ReportResult};
use either::Either;
use ghoti_syntax::{
    self as ast, RedirectDest, RedirectMode, RedirectPort, SourceFile, Stmt, WordFrag,
};
use itertools::EitherOrBoth;
use tokio::io::AsyncWriteExt;
use utils::validate_variable_name;

use crate::utils::validate_function_name;

pub mod builtins;
pub mod command;
mod utils;

#[cfg(test)]
mod tests;

const HOME_VAR: &str = "HOME";

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type ExecResult<T = Status> = Result<T>;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Status(pub i32);

impl Status {
    pub fn is_success(self) -> bool {
        self == Self::SUCCESS
    }
}

impl Status {
    pub const SUCCESS: Self = Self(0);
    pub const FAILURE: Self = Self(1);
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<i32> for Status {
    fn from(n: i32) -> Self {
        Self(n)
    }
}

impl From<bool> for Status {
    fn from(b: bool) -> Self {
        match b {
            true => Self::SUCCESS,
            false => Self::FAILURE,
        }
    }
}

type ExecControlFlow = ControlFlow<ExecBreak>;

#[derive(Debug, Clone)]
pub enum ExecBreak {
    LoopBreak,
    LoopContinue,
    FuncReturn(Status),
}

pub type Value = String;
pub type ValueList = Vec<String>;

/// An error triggered at runtime.
///
/// Typically, it will not break the execution but only print a message and set a non-zero status
/// code.
/// [`ExecContext::emit_error`] can be used to report it with backtrace and get a coresponding
/// [`ExitStatus`].
#[derive(Debug, thiserror::Error)]
pub enum Error {
    // User errors.
    #[error("{0}")]
    Custom(String),
    #[error("invalid options: {0}")]
    InvalidOptions(clap::Error),
    #[error("invalid identifier: {0:?}")]
    InvalidIdentifier(String),
    #[error("invalid integer: {0:?}")]
    InvalidInteger(String),
    #[error("expecting a single word, got {0} words")]
    NotOneWord(usize),
    #[error("empty command")]
    EmptyCommand,
    #[error("command not found: {0:?}")]
    CommandNotFound(String),
    #[error("variable not found: {0:?}")]
    VariableNotFound(String),
    #[error("cannot modify special variable: {0:?}")]
    ModifySpecialVariable(String),

    // System errors.
    #[error("read/write error: {0}")]
    ReadWrite(io::Error),
    #[error("failed open redirection file {0:?}: {1}")]
    OpenRedirectionFile(PathBuf, io::Error),
    #[error("failed to spawn process {0:?}: {1}")]
    SpawnProcess(PathBuf, io::Error),
    #[error("pipe closed")]
    PipeClosed,
    #[error("failed to create pipe: {0}")]
    CreatePipe(io::Error),
    #[error("failed to clone fd: {0}")]
    CloneHandle(io::Error),
    #[error("failed to wait process: {0}")]
    WaitProcess(io::Error),

    // TODO
    #[error("invalid UTF8 in {0}")]
    InvalidUtf8(String),
}

impl Error {
    pub fn to_status(&self) -> Status {
        // TODO
        Status::FAILURE
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
    pub async fn write_stdout(&self, bytes: impl AsRef<[u8]>) -> ExecResult {
        let bytes = bytes.as_ref();
        match &self.stdout {
            Stdio::Inherit => {
                tokio::io::stdout()
                    .write_all(bytes)
                    .await
                    .map_err(Error::ReadWrite)?;
                Ok(Status::SUCCESS)
            }
            Stdio::Close => Err(Error::PipeClosed),
            Stdio::Collect(sink) => sink(bytes),
            Stdio::Raw(_) => {
                todo!()
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum VarScope {
    #[default]
    Auto,
    Local,
    Function,
    Global,
    Universal,
}

#[derive(Debug)]
pub struct Variable {
    pub value: ValueList,
    pub export: bool,
}

impl Variable {
    pub fn new(val: impl Into<String>) -> Self {
        Self::new_list(Some(val.into()))
    }

    pub fn new_list(vals: impl IntoIterator<Item = String>) -> Self {
        Self {
            value: vals.into_iter().collect(),
            export: false,
        }
    }

    pub fn exported(mut self) -> Self {
        self.export = true;
        self
    }
}

impl From<String> for Variable {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

impl From<Vec<String>> for Variable {
    fn from(vals: Vec<String>) -> Self {
        Self::new_list(vals)
    }
}

type DynSpecialVarGetter = dyn Fn(&ExecContext<'_>) -> ValueList;

pub struct Executor {
    builtin_funcs: BTreeMap<String, Box<dyn Command>>,
    global_funcs: RefCell<BTreeMap<String, Box<dyn Command>>>,
    global_vars: RefCell<BTreeMap<String, Variable>>,

    /// Special read-only variables implemented as getters.
    special_vars: BTreeMap<String, Box<DynSpecialVarGetter>>,
}

impl std::fmt::Debug for Executor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Executor")
            .field("builtin_funcs", &self.builtin_funcs)
            .field("global_funcs", &self.global_funcs)
            .field("global_vars", &self.global_vars)
            .field("special_vars", &self.special_vars.keys())
            .finish()
    }
}

impl Default for Executor {
    fn default() -> Self {
        let mut this = Self::new();
        this.builtin_funcs
            .extend(builtins::all_builtins().map(|(name, cmd)| (name.to_owned(), cmd)));
        this.set_special_var("status", |ctx| vec![ctx.last_status().to_string()]);
        this
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            builtin_funcs: BTreeMap::new(),
            global_funcs: RefCell::new(BTreeMap::new()),
            global_vars: RefCell::new(BTreeMap::new()),
            special_vars: BTreeMap::new(),
        }
    }

    pub fn default_from_env() -> Self {
        let mut this = Self::default();
        this.import_from_env();
        this
    }

    pub fn import_from_env(&mut self) {
        // TODO: Non-UTF8
        for (name, value) in std::env::vars() {
            self.set_global_var(name, Variable::new(value).exported());
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

    pub fn special_vars(&self) -> impl Iterator<Item = (&str, &DynSpecialVarGetter)> {
        self.special_vars
            .iter()
            .map(|(s, getter)| (&**s, &**getter))
    }

    pub fn set_special_var(
        &mut self,
        name: impl Into<String>,
        getter: impl Fn(&ExecContext<'_>) -> ValueList + 'static,
    ) {
        self.special_vars.insert(name.into(), Box::new(getter));
    }

    pub fn remove_special_var(&mut self, name: &str) -> bool {
        self.special_vars.remove(name).is_some()
    }

    pub(crate) fn global_funcs(&self) -> Ref<'_, BTreeMap<String, BoxCommand>> {
        self.global_funcs.borrow()
    }

    pub fn get_global_func(&self, name: &str) -> Option<BoxCommand> {
        self.global_funcs.borrow().get(name).cloned()
    }

    pub fn set_global_func(&self, name: String, cmd: impl Command) {
        self.global_funcs.borrow_mut().insert(name, Box::new(cmd));
    }

    pub fn remove_global_func(&self, name: &str) -> bool {
        self.global_funcs.borrow_mut().remove(name).is_some()
    }

    pub(crate) fn global_vars(&self) -> Ref<'_, BTreeMap<String, Variable>> {
        self.global_vars.borrow()
    }

    pub fn get_global_var(&self, name: &str) -> Option<impl Deref<Target = Variable>> {
        Ref::filter_map(self.global_vars.borrow(), |m| m.get(name)).ok()
    }

    pub fn set_global_var(&self, name: String, var: Variable) {
        self.global_vars.borrow_mut().insert(name, var);
    }

    pub fn remove_global_var(&self, name: &str) -> bool {
        self.global_vars.borrow_mut().remove(name).is_some()
    }
}

#[derive(Debug)]
pub struct ExecContext<'a> {
    local_var_map: BTreeMap<String, usize>,
    local_vars: Vec<Option<LocalVar>>,
    cur_scope_idx: usize,

    root: &'a Executor,
    outer: Option<&'a ExecContext<'a>>,
    last_status: Status,
    io: Io,
}

#[derive(Debug)]
struct LocalVar {
    var: Variable,
    name: String,
    scope_idx: usize,
    shadowed: Option<usize>,
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
            local_var_map: BTreeMap::new(),
            local_vars: Vec::new(),
            cur_scope_idx: 0,
            root,
            outer: None,
            last_status: Status::SUCCESS,
            io: Io::default(),
        }
    }

    pub fn new_inside(outer: &'a ExecContext<'a>) -> Self {
        Self {
            local_var_map: BTreeMap::new(),
            local_vars: Vec::new(),
            cur_scope_idx: 0,
            root: outer.root,
            outer: Some(outer),
            last_status: Status::SUCCESS,
            io: outer.io.clone(),
        }
    }

    pub fn last_status(&self) -> Status {
        self.last_status
    }

    pub fn set_last_status(&mut self, n: impl Into<Status>) {
        self.last_status = n.into();
    }

    pub fn io(&self) -> &Io {
        &self.io
    }

    pub fn backtrace(&self) -> impl Iterator<Item = &Self> {
        std::iter::successors(Some(self), |ctx| ctx.outer)
    }

    pub async fn emit_error(&mut self, err: Error) -> Status {
        // TODO: Backtrace.
        let _: ExecResult<_> = self.io().write_stdout(format!("{err}\n")).await;
        let st = err.to_status();
        self.set_last_status(st);
        st
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

    pub fn has_special_var(&self, name: &str) -> bool {
        self.special_vars.contains_key(name)
    }

    pub fn get_special_var(&self, name: &str) -> Option<ValueList> {
        self.special_vars.get(name).map(|getter| getter(self))
    }

    pub fn list_vars<B>(
        &self,
        scope: VarScope,
        mut f: impl FnMut(&str, &Variable) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        let local_iter = self
            .local_var_map
            .iter()
            .map(|(name, &idx)| (name.as_str(), &self.local_vars[idx].as_ref().unwrap().var));
        match scope {
            // Does not quite make sense, fish also returns empty for this.
            VarScope::Function => return ControlFlow::Continue(()),
            VarScope::Local => {
                for (name, var) in local_iter {
                    f(name, var)?;
                }
            }
            VarScope::Global | VarScope::Auto => {
                let global_vars = self.global_vars();
                let mut global_iter = global_vars.iter().map(|(name, var)| (name.as_str(), var));
                let mut merged_iter;
                let iter = if scope == VarScope::Global {
                    &mut global_iter as &mut dyn Iterator<Item = _>
                } else {
                    merged_iter = itertools::merge_join_by(local_iter, global_iter, |lhs, rhs| {
                        lhs.0.cmp(rhs.0)
                    })
                    .map(|either| either.into_left());
                    &mut merged_iter
                };

                for either in
                    itertools::merge_join_by(iter, self.special_vars(), |lhs, rhs| lhs.0.cmp(rhs.0))
                {
                    match either {
                        EitherOrBoth::Both((name, var), _) | EitherOrBoth::Left((name, var)) => {
                            f(name, var)?;
                        }
                        EitherOrBoth::Right((name, getter)) => {
                            let val = getter(self);
                            f(name, &Variable::from(val))?;
                        }
                    }
                }
            }
            VarScope::Universal => todo!(),
        }
        ControlFlow::Continue(())
    }

    pub fn get_var(&self, name: &str) -> Option<impl Deref<Target = Variable>> {
        if let Some(value) = self.get_special_var(name) {
            struct Id<T>(T);
            impl<T> Deref for Id<T> {
                type Target = T;
                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }

            return Some(Either::Left(Id(Variable {
                value,
                export: false,
            })));
        }

        match self.local_var_map.get(name) {
            Some(&idx) => Some(Either::Right(Either::Left(
                &self.local_vars[idx].as_ref().unwrap().var,
            ))),
            None => self
                .get_global_var(name)
                .map(|v| Either::Right(Either::Right(v))),
        }
    }

    pub fn set_var(&mut self, name: impl Into<String>, scope: VarScope, var: impl Into<Variable>) {
        let (name, var) = (name.into(), var.into());
        match scope {
            VarScope::Function if self.cur_scope_idx > 0 => {
                if let Some(&idx) = self.local_var_map.get(&name) {
                    let cur = self.local_vars[idx].as_mut().unwrap();
                    if cur.scope_idx == 0 {
                        cur.var = var;
                    } else {
                        todo!("set beneath");
                    }
                } else {
                    todo!("set new func var");
                }
            }
            VarScope::Local | VarScope::Function => {
                if let Some(idx) = self.local_var_map.get_mut(&name) {
                    let cur = self.local_vars[*idx].as_mut().unwrap();
                    if cur.scope_idx == self.cur_scope_idx {
                        cur.var = var;
                    } else {
                        let shadowed = Some(*idx);
                        *idx = self.local_vars.len();
                        self.local_vars.push(Some(LocalVar {
                            var,
                            name: name.clone(),
                            scope_idx: self.cur_scope_idx,
                            shadowed,
                        }));
                    }
                } else {
                    self.local_var_map
                        .insert(name.clone(), self.local_vars.len());
                    self.local_vars.push(Some(LocalVar {
                        var,
                        name,
                        scope_idx: self.cur_scope_idx,
                        shadowed: None,
                    }));
                }
            }
            VarScope::Global => {
                self.set_global_var(name, var);
            }
            VarScope::Universal => todo!(),
            VarScope::Auto => {
                let scope = if self.local_var_map.contains_key(&name) {
                    VarScope::Local
                } else if self.global_vars().contains_key(&name) {
                    VarScope::Global
                } else {
                    VarScope::Function
                };
                self.set_var(name, scope, var);
            }
        }
    }

    pub fn remove_var(&mut self, scope: VarScope, name: &str) -> bool {
        match scope {
            VarScope::Function if self.cur_scope_idx > 0 => {
                if let Some(&idx) = self.local_var_map.get(name) {
                    let mut opt_idx = Some(idx);
                    while let Some(idx) = opt_idx {
                        let cur = self.local_vars[idx].as_mut().unwrap();
                        if cur.scope_idx == 0 {
                            self.local_vars[idx] = None;
                            self.local_var_map.remove(name);
                            return true;
                        } else {
                            opt_idx = cur.shadowed;
                        }
                    }
                }
                false
            }
            VarScope::Local | VarScope::Function => {
                if let Some(idx) = self.local_var_map.get_mut(name) {
                    let cur = self.local_vars[*idx].take().unwrap();
                    if let Some(prev_idx) = cur.shadowed {
                        *idx = prev_idx;
                    } else {
                        self.local_var_map.remove(name);
                    }
                    return true;
                }
                false
            }
            VarScope::Global => self.remove_global_var(name),
            VarScope::Universal => todo!(),
            VarScope::Auto => {
                self.remove_var(VarScope::Local, name) || self.remove_var(VarScope::Global, name)
            }
        }
    }

    pub fn enter_local_scope(&mut self) -> impl DerefMut<Target = &mut Self> {
        self.cur_scope_idx += 1;
        scopeguard::guard(self, |this| {
            while let Some(var) = this.local_vars.last() {
                if matches!(var, Some(v) if v.scope_idx < this.cur_scope_idx) {
                    break;
                }
                if let Some(Some(var)) = this.local_vars.pop() {
                    match var.shadowed {
                        None => {
                            this.local_var_map.remove(&var.name);
                        }
                        Some(idx) => {
                            *this.local_var_map.get_mut(&var.name).unwrap() = idx;
                        }
                    }
                }
            }
            this.cur_scope_idx -= 1;
        })
    }

    pub async fn exec_source(&mut self, src: &SourceFile) -> ExecControlFlow {
        for stmt in &src.stmts {
            self.exec_stmt(stmt).await?;
        }
        ControlFlow::Continue(())
    }

    async fn exec_stmt(&mut self, stmt: &Stmt) -> ExecControlFlow {
        Box::pin(self.exec_stmt_inner(stmt)).await
    }

    async fn exec_stmt_inner(&mut self, stmt: &Stmt) -> ExecControlFlow {
        macro_rules! bail {
            ($err:expr) => {
                bail!(self, $err)
            };
            ($this:expr, $err:expr) => {{
                $this.emit_error($err).await;
                return ControlFlow::Continue(());
            }};
        }
        macro_rules! tri {
            ($e:expr) => {
                tri!(self, $e)
            };
            ($this:expr, $e:expr) => {
                match $e {
                    Ok(v) => v,
                    Err(err) => bail!($this, err),
                }
            };
        }

        match stmt {
            Stmt::Command(_pos, words) => {
                let words = self.expand_words(words).await;
                self.exec_command(&words).await;
            }
            Stmt::Block(_pos, stmts) => {
                // FIXME: Exclude group-only blocks, eg. if conditions.
                let this = &mut **self.enter_local_scope();
                for stmt in stmts {
                    this.exec_stmt(stmt).await?;
                }
            }
            Stmt::If(_pos, cond, then, else_) => {
                self.exec_stmt(cond).await?;
                if self.last_status().is_success() {
                    self.exec_stmt(then).await?;
                } else if let Some(else_) = else_ {
                    self.exec_stmt(else_).await?;
                }
            }
            Stmt::Switch(..) => todo!(),
            Stmt::While(_pos, cond, body) => loop {
                self.exec_stmt(cond).await?;
                if !self.last_status().is_success() {
                    break;
                }
                match self.exec_stmt(body).await {
                    ControlFlow::Break(ExecBreak::LoopContinue) => {}
                    ControlFlow::Break(ExecBreak::LoopBreak) => break,
                    ctl => return ctl,
                }
            },
            Stmt::For(_pos, var, elem_ws, body) => {
                let var = self.expand_words(slice::from_ref(var)).await;
                if var.len() != 1 {
                    bail!(Error::NotOneWord(var.len()));
                }
                let var = tri!(validate_variable_name(&var[0]));
                let elems = self.expand_words(elem_ws).await;
                for elem in elems {
                    self.set_var(var, VarScope::default(), elem);
                    match self.exec_stmt(body).await {
                        ControlFlow::Break(ExecBreak::LoopContinue) => {}
                        ControlFlow::Break(ExecBreak::LoopBreak) => break,
                        ctl => ctl?,
                    }
                }
            }
            Stmt::Break(_pos) => return ControlFlow::Break(ExecBreak::LoopBreak),
            Stmt::Continue(_pos) => return ControlFlow::Break(ExecBreak::LoopContinue),
            Stmt::Function(_pos, words, stmt) => {
                let words = &*self.expand_words(words).await;
                let name = tri!(words.first().ok_or(Error::NotOneWord(0)));
                let name = tri!(validate_function_name(name));
                let opts = tri!(
                    <FunctionOpts as clap::Parser>::try_parse_from(words)
                        .map_err(Error::InvalidOptions)
                );
                let user_func = command::user_func((**stmt).clone(), opts.description);
                self.set_global_func(name.into(), user_func);
            }
            Stmt::Return(_, w) => {
                let st = match w {
                    None => Status::SUCCESS,
                    Some(w) => {
                        let expanded = self.expand_words(slice::from_ref(w)).await;
                        match &expanded[..] {
                            [] => Status::SUCCESS,
                            [n] => {
                                let n =
                                    tri!(n.parse().map_err(|_| Error::InvalidInteger(n.into())));
                                Status(n)
                            }
                            _ => bail!(Error::NotOneWord(expanded.len())),
                        }
                    }
                };
                return ControlFlow::Break(ExecBreak::FuncReturn(st));
            }
            Stmt::Redirect(_pos, stmt, redirects) => {
                let prev_io = self.io().clone();
                let mut this = scopeguard::guard(&mut *self, |this| this.io = prev_io);

                for redir in redirects {
                    let (RedirectDest::File(file_word) | RedirectDest::Fd(file_word)) = &redir.dest;
                    let expanded = this.expand_words(slice::from_ref(file_word)).await;
                    let [file_path] = &*expanded else {
                        bail!(this, Error::NotOneWord(expanded.len()));
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
                    let f = tri!(
                        this,
                        opt.open(file_path)
                            .map_err(|err| Error::OpenRedirectionFile(file_path.into(), err))
                    );
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

                this.exec_stmt(stmt).await?;
            }
            Stmt::Pipe(_pos, port, lhs, rhs) => {
                let (pipe_r, pipe_w) = tri!(os_pipe::pipe().map_err(Error::CreatePipe));
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

                let (_st1, st2) = tokio::join!(
                    Box::pin(async move {
                        lctx.exec_stmt(lhs).await;
                        lctx.last_status()
                    }),
                    Box::pin(async move {
                        rctx.exec_stmt(rhs).await;
                        rctx.last_status()
                    }),
                );
                // TODO: pipestatus
                self.set_last_status(st2);
            }
            Stmt::Not(_pos, stmt) => {
                self.exec_stmt(stmt).await?;
                let ok = self.last_status().is_success();
                self.set_last_status(!ok);
            }
            Stmt::And(_pos, stmt) => {
                if self.last_status.is_success() {
                    self.exec_stmt(stmt).await?;
                }
            }
            Stmt::Or(_pos, stmt) => {
                if !self.last_status.is_success() {
                    self.exec_stmt(stmt).await?;
                }
            }
        }

        ControlFlow::Continue(())
    }

    pub async fn exec_command(&mut self, words: &[String]) {
        let Some(cmd) = words.first() else {
            self.emit_error(Error::EmptyCommand).await;
            return;
        };

        if let Some(cmd) = self.get_func(cmd) {
            let st = cmd.exec(self, words).await;
            self.set_last_status(st);
            return;
        }

        let args = builtins::CommandArgs {
            all: false,
            query: false,
            search: false,
            args: words.to_vec(),
        };
        let st = builtins::command(self, args).await.report(self).await;
        self.set_last_status(st);
    }

    async fn expand_words(&mut self, words: &[ast::Word]) -> Vec<String> {
        let mut out = Vec::new();
        for w in words {
            self.expand_word_into(&mut out, w).await;
        }
        out
    }

    // TODO: Word and size limit.
    async fn expand_word_into(&mut self, out: &mut Vec<String>, word: &ast::Word) {
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
                | WordFrag::Brace(_)
                | WordFrag::Home { .. } => {
                    let (words, rest_computed) = expanded.split_first().unwrap();
                    for w in words {
                        stack.push_str(w);
                        dfs(ret, stack, rest_frags, rest_computed);
                        stack.truncate(prev_len);
                    }
                }
                WordFrag::Wildcard | WordFrag::WildcardRecursive => {
                    todo!()
                }
            }
        }

        // Pre-expand.
        let frags = match word {
            ast::Word::Simple(w) => {
                out.push(w.clone());
                return;
            }
            ast::Word::Complex(frags) => frags,
        };

        let mut frag_expanded = Vec::with_capacity(frags.len());
        for frag in frags {
            match frag {
                WordFrag::Literal(_) => {}
                WordFrag::Variable(var_name) | WordFrag::VariableNoSplit(var_name) => {
                    let var = self.get_var(var_name);
                    let vals = match &var {
                        Some(var) => &var.value[..],
                        None => {
                            drop(var);
                            self.emit_error(Error::VariableNotFound(var_name.into()))
                                .await;
                            &[]
                        }
                    };
                    if matches!(frag, WordFrag::Variable(_)) {
                        frag_expanded.push(vals.to_vec());
                    } else {
                        frag_expanded.push(vec![vals.join(" ")]);
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
                            Ok(Status::SUCCESS)
                        }));
                        let prev_stdout = std::mem::replace(&mut self.io.stdout, io_collect);
                        let mut this =
                            scopeguard::guard(&mut *self, move |this| this.io.stdout = prev_stdout);
                        this.exec_stmt(stmt).await;
                        Rc::into_inner(buf).unwrap().into_inner()
                    };
                    let mut buf = String::from_utf8(buf).expect("TODO");

                    if let WordFrag::CommandNoSplit(_) = frag {
                        let len = buf.trim_end_matches('\n').len();
                        buf.truncate(len);
                        frag_expanded.push(vec![buf]);
                    } else {
                        frag_expanded.push(buf.lines().map(|s| s.to_owned()).collect());
                    }
                }
                WordFrag::Brace(words) => {
                    let mut alts = Vec::with_capacity(words.len());
                    for w in words {
                        Box::pin(self.expand_word_into(&mut alts, w)).await;
                    }
                    frag_expanded.push(alts);
                }
                WordFrag::Home { slash } => {
                    frag_expanded.push(self.get_home(*slash).into_iter().collect());
                }
                WordFrag::Wildcard | WordFrag::WildcardRecursive => {
                    todo!()
                }
            }
        }

        if frag_expanded.iter().all(|alts| !alts.is_empty()) {
            dfs(out, &mut String::new(), frags, &frag_expanded);
        }
    }

    fn get_home(&self, slash: bool) -> Option<String> {
        let var = self.get_var(HOME_VAR)?;
        let mut s = var.value.join(" ");
        let pos = s.trim_end_matches('/').len();
        s.truncate(pos);
        if slash || s.is_empty() {
            s.push('/');
        }
        Some(s)
    }
}
