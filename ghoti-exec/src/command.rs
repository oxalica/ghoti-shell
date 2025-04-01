use std::any::Any;
use std::fmt;
use std::marker::PhantomData;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::rc::Rc;

use ghoti_syntax::Stmt;

use crate::{Error, ExecBreak, ExecContext, Status, VarScope};

pub type BoxCommand = Box<dyn Command>;

type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

/// An executable command-like object.
///
/// Builtins and user functions are all its instances.
///
/// It must be cheaply clone-able.
pub trait Command: Any + fmt::Debug + dyn_clone::DynClone + 'static {
    // FIXME: Should be unnecessary since Rust 1.86.
    fn as_any(&self) -> &dyn Any;

    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, Status>;

    fn description(&self) -> Option<&str> {
        None
    }
}

dyn_clone::clone_trait_object!(Command);

#[expect(async_fn_in_trait, reason = "we accept !Send")]
pub trait ReportResult {
    async fn report(self, ctx: &mut ExecContext<'_>) -> Status;
}

impl ReportResult for () {
    async fn report(self, _ctx: &mut ExecContext<'_>) -> Status {
        Status::SUCCESS
    }
}

impl ReportResult for bool {
    async fn report(self, _ctx: &mut ExecContext<'_>) -> Status {
        Status::from(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UnchangedStatus;

impl ReportResult for UnchangedStatus {
    async fn report(self, ctx: &mut ExecContext<'_>) -> Status {
        ctx.last_status()
    }
}

impl ReportResult for Status {
    async fn report(self, _ctx: &mut ExecContext<'_>) -> Status {
        self
    }
}

impl ReportResult for Error {
    async fn report(self, ctx: &mut ExecContext<'_>) -> Status {
        ctx.emit_error(self).await
    }
}

impl<T: ReportResult, E: ReportResult> ReportResult for Result<T, E> {
    async fn report(self, ctx: &mut ExecContext<'_>) -> Status {
        match self {
            Ok(v) => v.report(ctx).await,
            Err(e) => e.report(ctx).await,
        }
    }
}

struct RawBuiltin<F, Ret> {
    func: F,
    _marker: PhantomData<fn() -> Ret>,
}

impl<F, Ret> fmt::Debug for RawBuiltin<F, Ret> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Builtin").finish_non_exhaustive()
    }
}

impl<F: Clone, Ret> Clone for RawBuiltin<F, Ret> {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            _marker: PhantomData,
        }
    }
}

impl<F, Ret> Command for RawBuiltin<F, Ret>
where
    F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, &[String]) -> Ret,
    Ret: 'static + ReportResult,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, Status> {
        Box::pin(async move { (self.func)(ctx, args).await.report(ctx).await })
    }
}

pub fn raw_builtin<F, Ret>(func: F) -> impl Command
where
    F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, &[String]) -> Ret,
    Ret: 'static + ReportResult,
{
    RawBuiltin {
        func,
        _marker: PhantomData,
    }
}

struct ParsedBuiltin<F, Args, Ret> {
    func: F,
    _marker: PhantomData<fn(Args) -> Ret>,
}

impl<F, Args, Ret> fmt::Debug for ParsedBuiltin<F, Args, Ret> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Builtin").finish_non_exhaustive()
    }
}

impl<F: Clone, Args, Ret> Clone for ParsedBuiltin<F, Args, Ret> {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            _marker: PhantomData,
        }
    }
}

impl<F, Args, Ret> Command for ParsedBuiltin<F, Args, Ret>
where
    F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, Args) -> Ret,
    Args: 'static + clap::Parser,
    Ret: 'static + ReportResult,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, Status> {
        let ret = <Args as clap::Parser>::try_parse_from(args);
        Box::pin(async move {
            match ret {
                Ok(parsed) => (self.func)(ctx, parsed).await.report(ctx).await,
                Err(err) => ctx.emit_error(Error::InvalidOptions(err)).await,
            }
        })
    }
}

pub fn parsed_builtin<F, Args, Ret>(func: F) -> impl Command
where
    F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, Args) -> Ret,
    Args: 'static + clap::Parser,
    Ret: 'static + ReportResult,
{
    ParsedBuiltin {
        func,
        _marker: PhantomData,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct UserFunc(Rc<(Stmt, Option<String>)>);

impl UserFunc {
    pub fn stmt(&self) -> &Stmt {
        &self.0.0
    }
}

impl Command for UserFunc {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, Status> {
        Box::pin(async move {
            let mut subctx = ExecContext::new_inside(ctx);
            subctx.set_var("argv", VarScope::Local, args[1..].to_vec());
            let ctl = subctx.exec_stmt(&self.0.0).await;
            match ctl {
                ControlFlow::Continue(()) => Status::SUCCESS,
                ControlFlow::Break(ExecBreak::FuncReturn(st)) => st,
                ControlFlow::Break(_) => unreachable!(),
            }
        })
    }

    fn description(&self) -> Option<&str> {
        self.0.1.as_deref()
    }
}

pub fn user_func(stmt: Stmt, description: Option<String>) -> impl Command {
    UserFunc(Rc::new((stmt, description)))
}
