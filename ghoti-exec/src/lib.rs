use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::OpenOptions;
use std::ops::{ControlFlow, Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;
use std::{fmt, io as stdio, mem, slice};

use builtins::FunctionOpts;
use command::{BoxCommand, Command, UserFunc, UserFuncImpl};
use either::Either;
use ghoti_syntax::{
    self as ast, ParseError, RedirectDest, RedirectMode, RedirectPort, Stmt, WordFrag, parse_source,
};
use io::StdioCollectSink;
use itertools::Itertools;
use tokio::io::AsyncRead;
use utils::{access_can_exec, access_can_read, validate_variable_name};

use crate::io::{Io, IoConfig};
use crate::utils::validate_function_name;

pub mod builtins;
pub mod command;
pub mod io;
mod utils;

#[cfg(test)]
mod tests;

// Well-known variables.
const VAR_STATUS: &str = "status";
const VAR_STATUS_GENERATION: &str = "status_generation";
const VAR_PIPE_STATUS: &str = "pipestatus";
const VAR_PATH: &str = "PATH";
const VAR_HOME: &str = "HOME";
const VAR_AUTOLOAD_PATH: &str = "fish_function_path";

const AUTOLOAD_NAME_SUFFIX: &str = ".fish";

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type ExecResult<T = Status> = Result<T>;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Status(pub u8);

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

impl From<u8> for Status {
    fn from(n: u8) -> Self {
        Self(n)
    }
}

impl From<usize> for Status {
    fn from(n: usize) -> Self {
        Self(u8::try_from(n).unwrap_or(u8::MAX))
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

impl std::process::Termination for Status {
    fn report(self) -> std::process::ExitCode {
        self.0.into()
    }
}

type ExecControlFlow = ControlFlow<ExecBreak>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecBreak {
    LoopBreak,
    LoopContinue,
    Return,
}

pub type Value = String;
pub type ValueList = Vec<String>;

/// An error triggered at runtime.
///
/// Typically, it will not break the execution but only print a message and set a non-zero status
/// code.
/// [`ExecContext::emit_error`] can be used to report it with backtrace and get a corresponding
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
    #[error("command not found: {0}")]
    CommandNotFound(String),
    #[error("cannot modify special variable: {0:?}")]
    ModifySpecialVariable(String),
    #[error("syntax error: {}", .0.kind)]
    SyntaxError(ParseError),

    // System errors.
    #[error("read/write error: {0}")]
    ReadWrite(stdio::Error),
    #[error("failed open redirection file {0:?}: {1}")]
    OpenRedirectionFile(PathBuf, stdio::Error),
    #[error("failed to spawn process {0:?}: {1}")]
    SpawnProcess(PathBuf, stdio::Error),
    #[error("pipe closed")]
    PipeClosed,
    #[error("failed to create pipe: {0}")]
    CreatePipe(stdio::Error),
    #[error("failed to clone fd: {0}")]
    CloneHandle(stdio::Error),
    #[error("failed to wait process: {0}")]
    WaitProcess(stdio::Error),
    #[error("cannot change the current directory: {0}")]
    ChangeCurrentDir(stdio::Error),
    #[error("invalid UTF8")]
    InvalidUtf8,
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Self::Custom(s)
    }
}

impl Error {
    pub fn to_status(&self) -> Status {
        Status(match self {
            Error::Custom(_) => 1,
            Error::InvalidOptions(_) => 121,
            Error::InvalidIdentifier(_) => 2,
            Error::InvalidInteger(_) => 2,
            Error::NotOneWord(_) => 121,
            Error::EmptyCommand => 123,
            Error::CommandNotFound(_) => 127,
            Error::ModifySpecialVariable(_) => 1,
            Error::SyntaxError(_) => 127,
            Error::SpawnProcess(..) => 125,
            Error::ReadWrite(_) => 1,
            Error::OpenRedirectionFile(..) => 1,
            Error::PipeClosed => 1,
            Error::CreatePipe(_) => 1,
            Error::CloneHandle(_) => 1,
            Error::WaitProcess(_) => 1,
            Error::ChangeCurrentDir(_) => 1,
            Error::InvalidUtf8 => 123,
        })
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

// FIXME: Cloning this is no good.
#[derive(Debug, Clone)]
pub struct Variable {
    pub value: ValueList,
    pub export: bool,
    cache: Option<RefCell<Box<VarCache>>>,
}

#[derive(Debug, Clone, Default)]
struct VarCache {
    /// For `VAR_AUTOLOAD_PATH`, this is a map of function-name => autoload-path.
    name_to_path: Option<BTreeMap<String, PathBuf>>,
}

impl Variable {
    pub fn new(val: impl Into<String>) -> Self {
        Self::new_list(Some(val.into()))
    }

    pub fn new_list(vals: impl IntoIterator<Item = String>) -> Self {
        Self {
            value: vals.into_iter().collect(),
            export: false,
            cache: None,
        }
    }

    pub fn exported(mut self) -> Self {
        self.export = true;
        self
    }

    fn with_cache(mut self) -> Self {
        self.cache = Some(Default::default());
        self
    }

    fn name_to_path_cache(&self) -> Ref<'_, Option<BTreeMap<String, PathBuf>>> {
        Ref::map(self.cache.as_ref().expect("has cache").borrow(), |cache| {
            &cache.name_to_path
        })
    }

    fn set_name_to_path_cache(&self, cache: BTreeMap<String, PathBuf>) {
        self.cache
            .as_ref()
            .expect("has cache")
            .borrow_mut()
            .name_to_path = Some(cache);
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
    /// Special read-only variables implemented as getters.
    special_vars: BTreeMap<String, Box<DynSpecialVarGetter>>,

    global_funcs: RefCell<BTreeMap<String, Box<dyn Command>>>,
    global_vars: RefCell<BTreeMap<String, Variable>>,

    error_renderer: annotate_snippets::Renderer,
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
        this.error_renderer = annotate_snippets::Renderer::styled();

        this.builtin_funcs
            .extend(builtins::all_builtins().map(|(name, cmd)| (name.to_owned(), cmd)));

        this.set_special_var(VAR_STATUS, |ctx| vec![ctx.last_status().to_string()]);
        // TODO?
        this.set_special_var(VAR_STATUS_GENERATION, |_ctx| vec!["0".into()]);
        this.set_special_var(VAR_PIPE_STATUS, |ctx| {
            ctx.last_pipe_status()
                .iter()
                .map(|s| s.to_string())
                .collect()
        });

        this.set_global_var(VAR_AUTOLOAD_PATH, Variable::new_list([]).with_cache());

        this
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            builtin_funcs: BTreeMap::new(),
            special_vars: BTreeMap::new(),

            global_funcs: RefCell::new(BTreeMap::new()),
            global_vars: RefCell::new(BTreeMap::new()),

            error_renderer: annotate_snippets::Renderer::plain(),
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

    pub fn set_global_var(&self, name: impl Into<String>, var: Variable) {
        self.global_vars.borrow_mut().insert(name.into(), var);
    }

    pub fn remove_global_var(&self, name: &str) -> bool {
        self.global_vars.borrow_mut().remove(name).is_some()
    }
}

#[derive(Debug)]
pub struct ExecContext<'a> {
    local_vars_by_name: BTreeMap<(String, Reverse<u32>), Variable>,
    local_vars_by_scope: BTreeSet<(u32, String)>,
    cur_scope_idx: u32,

    root: &'a Executor,
    outer: Option<&'a ExecContext<'a>>,
    last_status: Status,
    last_pipe_status: Vec<Status>,
    io: IoConfig,

    backtrace: Vec<FrameInfo>,
}

#[derive(Debug)]
struct SourceFile {
    origin: Option<String>,
    text: String,
}

#[derive(Debug)]
struct FrameInfo {
    kind: FrameKind,
    source: Rc<SourceFile>,
    pos: u32,
}

#[derive(Debug)]
enum FrameKind {
    Function { name: String, def_pos: u32 },
    Source,
    Pipe { def_pos: u32 },
    CommandSubst { def_pos: u32 },
}

#[derive(Debug)]
pub enum LocateExternalCommand {
    ExecFile(PathBuf),
    NotExecFile(PathBuf),
    Dir(PathBuf),
    NotFound,
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
            local_vars_by_name: BTreeMap::new(),
            local_vars_by_scope: BTreeSet::new(),
            cur_scope_idx: 0,

            root,
            outer: None,
            last_status: Status::SUCCESS,
            last_pipe_status: Vec::new(),
            io: IoConfig::default(),

            backtrace: Vec::new(),
        }
    }

    pub(crate) fn new_inside(outer: &'a ExecContext<'a>, frame: FrameInfo) -> Self {
        let mut this = Self::new(outer.root);
        this.outer = Some(outer);
        this.io = outer.io.clone();
        this.backtrace.push(frame);
        this
    }

    pub fn last_status(&self) -> Status {
        self.last_status
    }

    pub fn set_last_status(&mut self, n: impl Into<Status>) {
        self.last_status = n.into();
    }

    pub fn last_pipe_status(&self) -> &[Status] {
        &self.last_pipe_status
    }

    pub fn io(&self) -> &IoConfig {
        &self.io
    }

    pub fn backtrace_context(&self) -> impl Iterator<Item = &Self> {
        std::iter::successors(Some(self), |ctx| ctx.outer)
    }

    fn backtrace(&self) -> impl Iterator<Item = &FrameInfo> {
        self.backtrace_context()
            .flat_map(|ctx| ctx.backtrace.iter().rev())
    }

    fn render_error(&self, err: &Error) -> String {
        use annotate_snippets::{Level, Snippet};
        use std::fmt::Write;

        let err_str = err.to_string();
        let mut msg = Level::Error.title(&err_str);
        let first_note;

        let mut iter = self.backtrace();
        {
            let frame = iter.next().unwrap();
            let mut span = frame.pos as usize..frame.pos as usize;
            if let Error::SyntaxError(err) = err {
                span = err.span();
            }

            let mut snip = Snippet::source(&frame.source.text)
                .annotation(Level::Error.span(span.clone()))
                .fold(true);
            if let Some(origin) = &frame.source.origin {
                snip = snip.origin(origin);
            } else if span.start == 0 {
                // Special simple case for errors on the only command in interactive shell.
                return self.error_renderer.render(msg).to_string();
            }

            let (def_note, def_pos) = match &frame.kind {
                FrameKind::Function { name, def_pos } => {
                    first_note = format!("in function {name:?}");
                    (first_note.as_str(), *def_pos)
                }
                FrameKind::Pipe { def_pos } => ("in pipeline", *def_pos),
                FrameKind::CommandSubst { def_pos } => ("in command substitution", *def_pos),
                FrameKind::Source => ("before sourcing this file", 0),
            };
            snip = snip.annotation(
                Level::Note
                    .span(def_pos as usize..def_pos as usize)
                    .label(def_note),
            );

            msg = msg.snippet(snip);
        }

        let footers = iter
            .filter_map(|frame| {
                let mut s = match &frame.kind {
                    FrameKind::Function { name, .. } => format!("from function {name:?}"),
                    FrameKind::Source => "from sourcing".into(),
                    FrameKind::Pipe { .. } => "from pipeline".into(),
                    FrameKind::CommandSubst { .. } => "from command substitution".into(),
                };
                if let Some(origin) = &frame.source.origin {
                    let line = 1 + frame.source.text[..frame.pos as usize]
                        .bytes()
                        .filter(|&b| b == b'\n')
                        .count();
                    write!(s, " {origin}:{line}").unwrap();
                } else if let FrameKind::Source = frame.kind {
                    return None;
                }
                Some(s)
            })
            .collect::<Vec<String>>();
        msg = msg.footers(footers.iter().map(|msg| Level::Note.title(msg)));

        format!("{}\n", self.error_renderer.render(msg))
    }

    pub async fn emit_error(&mut self, err: Error, set_status: bool) {
        let msg = self.render_error(&err);
        let _: ExecResult<_> = self.io().stderr.write_all(msg).await;
        if set_status {
            self.set_last_status(err.to_status());
        }
    }

    pub async fn list_funcs<B>(
        &mut self,
        f: impl FnMut(&str, Option<&BoxCommand>) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        self.populate_autoload_func_cache().await;
        self.list_funcs_without_autoload(f)
    }

    pub fn list_funcs_without_autoload<B>(
        &self,
        mut f: impl FnMut(&str, Option<&BoxCommand>) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        let global_funcs = self.global_funcs.borrow();
        let global_funcs = global_funcs
            .iter()
            .map(|(name, cmd)| (&name[..], Some(cmd)));

        let builtin_funcs = self.builtins().map(|(name, cmd)| (name, Some(cmd)));

        let autoload_var = self.get_var(VAR_AUTOLOAD_PATH);
        let autoload_cache = autoload_var.as_ref().map(|var| var.name_to_path_cache());
        let autoload_funcs = autoload_cache
            .as_ref()
            .and_then(|cache| cache.as_ref())
            .into_iter()
            .flat_map(|map| map.keys())
            .map(|name| (name.as_str(), None));

        for (name, cmd) in global_funcs
            .merge_join_by(autoload_funcs, |lhs, rhs| lhs.0.cmp(rhs.0))
            .map(|either| either.into_left())
            .merge_join_by(builtin_funcs, |lhs, rhs| lhs.0.cmp(rhs.0))
            .map(|either| either.into_left())
        {
            f(name, cmd)?;
        }
        ControlFlow::Continue(())
    }

    pub async fn get_or_autoload_func(&mut self, name: &str) -> Option<BoxCommand> {
        if let Some(func) = self.get_global_func(name) {
            return Some(func);
        }

        if validate_function_name(name).is_ok() && self.try_autoload_func(name).await.is_some() {
            if let Some(cmd) = self.get_global_func(name) {
                return Some(cmd);
            }
        }

        self.get_builtin(name)
    }

    pub fn has_special_var(&self, name: &str) -> bool {
        self.special_vars.contains_key(name)
    }

    pub fn get_special_var(&self, name: &str) -> Option<ValueList> {
        self.special_vars.get(name).map(|getter| getter(self))
    }

    pub fn local_vars(&self) -> impl Iterator<Item = (&str, &Variable)> {
        let mut iter = self.local_vars_by_name.iter().peekable();
        std::iter::from_fn(move || {
            let ((name, sid), elem) = iter.next()?;
            while iter.next_if(|((_, next_sid), _)| sid == next_sid).is_some() {}
            Some((name.as_str(), elem))
        })
    }

    pub fn list_vars<B>(
        &self,
        scope: VarScope,
        mut f: impl FnMut(&str, &Variable) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match scope {
            // Does not quite make sense, fish also returns empty for this.
            VarScope::Function => ControlFlow::Continue(()),
            VarScope::Global => self
                .global_vars()
                .iter()
                .try_for_each(|(name, var)| f(name, var)),
            VarScope::Local => self.local_vars().try_for_each(|(name, var)| f(name, var)),
            VarScope::Auto => itertools::merge_join_by(
                self.local_vars(),
                self.global_vars()
                    .iter()
                    .map(|(name, var)| (name.as_str(), var)),
                |lhs, rhs| Ord::cmp(lhs.0, rhs.0),
            )
            .map(|either| either.into_left())
            .try_for_each(|(name, var)| f(name, var)),
            VarScope::Universal => todo!(),
        }
    }

    fn get_local_var(&self, name: String) -> Option<(&Variable, u32)> {
        let key = (name, Reverse(u32::MAX));
        let ((varname, sid), val) = self.local_vars_by_name.range(&key..).next()?;
        (*varname == key.0).then_some((val, sid.0))
    }

    fn get_local_var_mut(&mut self, name: String) -> Option<(&mut Variable, u32)> {
        let key = (name, Reverse(u32::MAX));
        let ((varname, sid), val) = self.local_vars_by_name.range_mut(&key..).next()?;
        (*varname == key.0).then_some((val, sid.0))
    }

    pub fn get_var(&self, name: &str) -> Option<impl Deref<Target = Variable>> {
        if let Some(value) = self.get_special_var(name) {
            return Some(Either::Left(Cow::Owned(Variable::from(value))));
        }
        if let Some((var, _sid)) = self.get_local_var(name.to_owned()) {
            return Some(Either::Left(Cow::Borrowed(var)));
        }
        if let Some(var) = self.get_global_var(name) {
            return Some(Either::Right(var));
        }
        None
    }

    fn set_local_var_at(&mut self, name: String, sid: u32, var: Variable) {
        if self
            .local_vars_by_name
            .insert((name.clone(), Reverse(sid)), var)
            .is_none()
        {
            self.local_vars_by_scope.insert((sid, name));
        }
    }

    pub fn set_var(&mut self, name: impl Into<String>, scope: VarScope, var: impl Into<Variable>) {
        let (name, mut var) = (name.into(), var.into());
        if name == VAR_AUTOLOAD_PATH {
            var = var.with_cache();
        }
        match scope {
            VarScope::Local | VarScope::Function => {
                let sid = if scope == VarScope::Local {
                    self.cur_scope_idx
                } else {
                    0
                };
                self.set_local_var_at(name, sid, var);
            }
            VarScope::Global => {
                self.set_global_var(name, var);
            }
            VarScope::Universal => todo!(),
            VarScope::Auto => {
                if let Some((place, _)) = self.get_local_var_mut(name.clone()) {
                    *place = var;
                } else if let Some(place) = self.global_vars.borrow_mut().get_mut(&name) {
                    *place = var;
                } else {
                    // Function scope by default.
                    self.set_local_var_at(name, 0, var);
                }
            }
        }
    }

    fn remove_local_var_at(&mut self, name: String, sid: u32) -> bool {
        let key = (name, Reverse(sid));
        if self.local_vars_by_name.remove(&key).is_some() {
            assert!(self.local_vars_by_scope.remove(&(sid, key.0)));
            true
        } else {
            false
        }
    }

    pub fn remove_var(&mut self, scope: VarScope, name: &str) -> bool {
        match scope {
            VarScope::Local if self.cur_scope_idx > 0 => {
                if let Some((_var, sid)) = self.get_local_var(name.to_owned()) {
                    assert!(self.remove_local_var_at(name.to_owned(), sid));
                    true
                } else {
                    false
                }
            }
            VarScope::Local | VarScope::Function => self.remove_local_var_at(name.to_owned(), 0),
            VarScope::Global => self.remove_global_var(name),
            VarScope::Universal => todo!(),
            VarScope::Auto => {
                self.remove_var(VarScope::Local, name) || self.remove_global_var(name)
            }
        }
    }

    /// Populate autoload functions cache if it's uninitialized or dirty.
    pub async fn populate_autoload_func_cache(&mut self) {
        let Some(var) = self.get_var(VAR_AUTOLOAD_PATH) else {
            return;
        };

        if dbg!(var.name_to_path_cache().is_some()) {
            return;
        }

        let paths = var.value.clone();
        let map = tokio::task::spawn_blocking(move || {
            let mut map = BTreeMap::new();
            for (name, path) in paths
                .iter()
                .flat_map(std::fs::read_dir)
                .flatten()
                .filter_map(|ent| {
                    let ent = ent.ok()?;
                    ent.file_type().ok().filter(|ft| ft.is_file())?;
                    let mut name = ent.file_name().into_string().ok()?;
                    name.truncate(name.strip_suffix(AUTOLOAD_NAME_SUFFIX)?.len());
                    let path = ent.path();
                    access_can_read(&path).then_some((name, path))
                })
            {
                map.entry(name).or_insert(path);
            }
            map
        })
        .await
        .expect("no panic");

        var.set_name_to_path_cache(dbg!(map));
    }

    /// Try to find and source the autoload function `name`.
    ///
    /// Return `Some(())` if a file is found and sourced, `None` otherwise.
    /// It does not check if the sourced file actually contains any relavent definition.
    async fn try_autoload_func(&mut self, name: &str) -> Option<()> {
        debug_assert!(!name.contains("/"));

        self.populate_autoload_func_cache().await;
        let path = self
            .get_var(VAR_AUTOLOAD_PATH)?
            .name_to_path_cache()
            .as_ref()
            .expect("populated")
            .get(name)?
            .to_owned();

        let (ret, path) =
            tokio::task::spawn_blocking(move || (std::fs::read_to_string(&path), path))
                .await
                .expect("no panic");
        match ret {
            Ok(text) => {
                self.exec_source(Some(path.to_string_lossy().into_owned()), text)
                    .await;
                Some(())
            }
            Err(err) => {
                self.emit_error(Error::ReadWrite(err), false).await;
                None
            }
        }
    }

    pub fn locate_external_command(&self, name: &str) -> LocateExternalCommand {
        // Absolute or relative path.
        if name.contains("/") {
            let p = Path::new(name);
            return match p.metadata() {
                Ok(m) => {
                    if m.is_dir() {
                        LocateExternalCommand::Dir(p.into())
                    } else if m.is_file() && access_can_exec(p) {
                        LocateExternalCommand::ExecFile(p.into())
                    } else {
                        LocateExternalCommand::NotExecFile(p.into())
                    }
                }
                Err(_) => LocateExternalCommand::NotFound,
            };
        }

        // Special case.
        if name == ".." {
            return LocateExternalCommand::Dir("..".into());
        }

        // Finally, search for external commands in PATH.
        if let Some(path) = self.get_var(VAR_PATH) {
            let mut found_file = None;
            for dir in path.value.iter().flat_map(|s| s.split(':')) {
                let p = Path::new(dir).join(name);
                if let Ok(m) = p.metadata() {
                    if m.is_file() && access_can_exec(&p) {
                        return LocateExternalCommand::ExecFile(p);
                    }
                    found_file.get_or_insert(p);
                }
            }
            if let Some(p) = found_file {
                // No special treatment for directories here.
                return LocateExternalCommand::NotExecFile(p);
            }
        }

        LocateExternalCommand::NotFound
    }

    pub fn enter_local_scope(&mut self) -> impl DerefMut<Target = &mut Self> {
        self.cur_scope_idx += 1;
        scopeguard::guard(self, |this| {
            let scope = this
                .local_vars_by_scope
                .split_off(&(this.cur_scope_idx, String::new()));
            for (sid, name) in scope {
                this.local_vars_by_name.remove(&(name, Reverse(sid)));
            }
            this.cur_scope_idx -= 1;
        })
    }

    pub async fn exec_source(&mut self, origin: Option<String>, text: String) {
        self.backtrace.push(FrameInfo {
            kind: FrameKind::Source,
            source: Rc::new(SourceFile { origin, text }),
            pos: 0,
        });
        let mut this = scopeguard::guard(self, |this| {
            this.backtrace.pop();
        });
        let text = &this.backtrace.last().unwrap().source.text;

        let ast = match parse_source(text) {
            Ok(src) => src,
            Err(mut errs) => {
                // FIXME: More errors?
                errs.truncate(1);
                let err = Error::SyntaxError(errs.pop().unwrap());
                this.emit_error(err, true).await;
                return;
            }
        };

        for stmt in &ast.stmts {
            if this.exec_stmt(stmt).await.is_break() {
                break;
            }
        }
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
                $this.emit_error($err, true).await;
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

        self.backtrace.last_mut().unwrap().pos = stmt.pos();

        match stmt {
            Stmt::Command(_pos, words) => {
                let words = self.expand_words(words).await;
                tri!(self.exec_command(&words).await);
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
                    ctl => ctl?,
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
                    self.set_var(var, VarScope::Auto, elem);
                    match self.exec_stmt(body).await {
                        ControlFlow::Break(ExecBreak::LoopContinue) => {}
                        ControlFlow::Break(ExecBreak::LoopBreak) => break,
                        ctl => ctl?,
                    }
                }
            }
            Stmt::Break(_pos) => return ControlFlow::Break(ExecBreak::LoopBreak),
            Stmt::Continue(_pos) => return ControlFlow::Break(ExecBreak::LoopContinue),
            Stmt::Function(pos, words, stmt) => {
                let words = &*self.expand_words(words).await;
                let name = tri!(words.first().ok_or(Error::NotOneWord(0)));
                let name = tri!(validate_function_name(name));
                let opts = tri!(
                    <FunctionOpts as clap::Parser>::try_parse_from(words)
                        .map_err(Error::InvalidOptions)
                );
                let user_func = UserFunc(Rc::new(UserFuncImpl {
                    name: name.to_owned(),
                    // FIXME: Slow?
                    stmt: (**stmt).clone(),
                    description: opts.description,
                    source: self.backtrace.last().unwrap().source.clone(),
                    def_pos: *pos,
                }));
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
                self.set_last_status(st);
                return ControlFlow::Break(ExecBreak::Return);
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
                    let f = Io::File(Arc::new(f));

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
            Stmt::Pipe(pos, pipes, last) => {
                let task_cnt = pipes.len() + 1;

                async fn task(mut ctx: ExecContext<'_>, stmt: &Stmt) -> Status {
                    ctx.exec_stmt(stmt).await;
                    ctx.last_status()
                }

                let pipe_status = {
                    let mut tasks = Vec::with_capacity(task_cnt);

                    let frame = || FrameInfo {
                        // TODO: Should be command pos.
                        kind: FrameKind::Pipe { def_pos: *pos },
                        source: self.backtrace.last().unwrap().source.clone(),
                        pos: *pos,
                    };
                    let mut subctx = ExecContext::new_inside(self, frame());
                    for (lhs, port) in pipes {
                        let (tx, rx) = tri!(subctx, Io::new_os_pipe());
                        match *port {
                            RedirectPort::STDOUT => subctx.io.stdout = tx,
                            RedirectPort::STDERR => subctx.io.stderr = tx,
                            RedirectPort::STDOUT_STDERR => {
                                (subctx.io.stdout, subctx.io.stderr) = (tx.clone(), tx)
                            }
                            _ => todo!(),
                        }
                        tasks.push(task(subctx, lhs));

                        subctx = ExecContext::new_inside(self, frame());
                        subctx.io.stdin = rx;
                    }
                    tasks.push(task(subctx, last));

                    futures_util::future::join_all(tasks).await
                };

                self.set_last_status(pipe_status.last().copied().unwrap());
                self.last_pipe_status = pipe_status;
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

    pub async fn exec_command(&mut self, words: &[String]) -> ExecResult<()> {
        let cmd = words.first().ok_or(Error::EmptyCommand)?;

        if let Some(cmd) = self.get_or_autoload_func(cmd).await {
            cmd.exec(self, words).await;
            return Ok(());
        }

        let exe_path = match self.locate_external_command(cmd) {
            LocateExternalCommand::ExecFile(path) => path,
            LocateExternalCommand::Dir(path) => {
                std::env::set_current_dir(path).map_err(Error::ChangeCurrentDir)?;
                self.set_last_status(Status::SUCCESS);
                return Ok(());
            }
            LocateExternalCommand::NotExecFile(path) => {
                return Err(Error::CommandNotFound(format!(
                    "{} (candidate {} is not an executable file)",
                    cmd,
                    path.display(),
                )));
            }
            LocateExternalCommand::NotFound => {
                return Err(Error::CommandNotFound(cmd.into()));
            }
        };

        let st = self.exec_external_command(&exe_path, words).await?;
        self.set_last_status(st);
        Ok(())
    }

    pub async fn exec_external_command(
        &mut self,
        exe_path: &Path,
        args_with_0: &[String],
    ) -> ExecResult<Status> {
        use std::future::ready;

        async fn copy_stdio_to_sink(
            rdr: impl AsyncRead + Unpin,
            sink: StdioCollectSink,
        ) -> ExecResult<()> {
            use tokio::io::AsyncBufReadExt;

            let mut stdout = tokio::io::BufReader::new(rdr);
            loop {
                let buf = stdout.fill_buf().await.map_err(Error::ReadWrite)?;
                if buf.is_empty() {
                    return Ok(());
                }
                sink(buf)?;
                let len = buf.len();
                stdout.consume(len);
            }
        }

        let (mut os_stdin, stdin_sink) = self.io.stdin.to_stdio()?;
        if stdin_sink.is_some() {
            os_stdin = std::process::Stdio::null();
        }
        let (os_stdout, stdout_sink) = self.io.stdout.to_stdio()?;
        let (os_stderr, stderr_sink) = self.io.stderr.to_stdio()?;

        let mut child = {
            let mut builder = tokio::process::Command::new(exe_path);
            builder
                .kill_on_drop(true)
                .arg0(&args_with_0[0])
                .args(&args_with_0[1..])
                .stdin(os_stdin)
                .stdout(os_stdout)
                .stderr(os_stderr)
                .env_clear();
            self.list_vars::<()>(VarScope::Auto, |name, var| {
                if var.export {
                    builder.env(name, var.value.join(" "));
                }
                ControlFlow::Continue(())
            });
            builder
                .spawn()
                .map_err(|err| Error::SpawnProcess(exe_path.into(), err))?
        };

        let mut copy_stdout = Either::Left(ready(ExecResult::Ok(())));
        let mut copy_stderr = Either::Left(ready(ExecResult::Ok(())));
        if let Some(sink) = stdout_sink {
            copy_stdout = Either::Right(copy_stdio_to_sink(child.stdout.take().unwrap(), sink));
        }
        if let Some(sink) = stderr_sink {
            copy_stderr = Either::Right(copy_stdio_to_sink(child.stderr.take().unwrap(), sink));
        }

        let (ret_wait, ret_stdout, ret_stderr) =
            tokio::join!(child.wait(), copy_stdout, copy_stderr);
        ret_stdout?;
        ret_stderr?;
        let status = ret_wait.map_err(Error::WaitProcess)?;
        if status.success() {
            return Ok(Status::SUCCESS);
        }

        if let Some(code) = status.code() {
            return Ok(Status(u8::try_from(code).unwrap_or(u8::MAX)));
        }

        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            if let Some(sig) = status.signal() {
                let sig = sig.clamp(0, 128) as u8;
                return Ok(Status(127 + sig));
            }
        }

        unreachable!();
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
                        None => &[],
                    };
                    if matches!(frag, WordFrag::Variable(_)) {
                        frag_expanded.push(vals.to_vec());
                    } else {
                        frag_expanded.push(vec![vals.join(" ")]);
                    }
                }
                WordFrag::Command(stmt) | WordFrag::CommandNoSplit(stmt) => {
                    let pos = stmt.pos();
                    let frame = FrameInfo {
                        kind: FrameKind::CommandSubst { def_pos: pos },
                        source: self.backtrace.last().unwrap().source.clone(),
                        pos,
                    };

                    let buf = {
                        let buf = Rc::new(RefCell::new(Vec::new()));
                        let buf_weak = Rc::downgrade(&buf);
                        // FIXME: Avoid double Rc?
                        let io_collect = Io::Collect(Rc::new(move |bytes| {
                            buf_weak
                                .upgrade()
                                .ok_or(Error::PipeClosed)?
                                .borrow_mut()
                                .extend_from_slice(bytes);
                            Ok(Status::SUCCESS)
                        }));

                        self.backtrace.push(frame);
                        let prev_stdout = mem::replace(&mut self.io.stdout, io_collect);
                        let mut this = scopeguard::guard(&mut *self, move |this| {
                            this.io.stdout = prev_stdout;
                            this.backtrace.pop();
                        });
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
        let var = self.get_var(VAR_HOME)?;
        let mut s = var.value.join(" ");
        let pos = s.trim_end_matches('/').len();
        s.truncate(pos);
        if slash || s.is_empty() {
            s.push('/');
        }
        Some(s)
    }
}
