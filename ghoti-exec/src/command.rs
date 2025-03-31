use std::fmt;
use std::marker::PhantomData;
use std::pin::Pin;
use std::rc::Rc;

use ghoti_syntax::Stmt;

use crate::{Error, ExecContext, ExecResult, VarScope};

pub type BoxCommand = Box<dyn Command>;

type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + 'a>>;

/// An executable command-like object.
///
/// Builtins and user functions are all its instances.
///
/// It must be cheaply clone-able.
pub trait Command: fmt::Debug + dyn_clone::DynClone + 'static {
    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, ExecResult>;

    fn description(&self) -> Option<&str> {
        None
    }
}

dyn_clone::clone_trait_object!(Command);

pub struct Builtin<F, Args> {
    func: F,
    _marker: PhantomData<fn(Args) -> Args>,
}

impl<F, Args> fmt::Debug for Builtin<F, Args> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Builtin").finish_non_exhaustive()
    }
}

impl<F: Clone, Args> Clone for Builtin<F, Args> {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            _marker: PhantomData,
        }
    }
}

impl<F, Args> Command for Builtin<F, Args>
where
    F: 'static + Clone + AsyncFn(&mut ExecContext<'_>, Args) -> ExecResult,
    Args: 'static + clap::Parser,
{
    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, ExecResult> {
        match <Args as clap::Parser>::try_parse_from(args) {
            Ok(parsed) => Box::pin((self.func)(ctx, parsed)),
            Err(err) => Box::pin(std::future::ready(Err(Error::InvalidOptions(err)))),
        }
    }
}

impl<F, Args> Builtin<F, Args> {
    pub fn new(func: F) -> Self {
        Self {
            func,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Debug)]
pub struct UserFunc(Rc<(Stmt, Option<String>)>);

impl Command for UserFunc {
    fn exec<'fut>(
        &'fut self,
        ctx: &'fut mut ExecContext<'_>,
        args: &'fut [String],
    ) -> BoxFuture<'fut, ExecResult> {
        Box::pin(async move {
            let mut subctx = ExecContext::new_inside(ctx);
            subctx.set_var("argv", VarScope::Function, args[1..].to_vec());
            subctx.exec_stmt(&self.0.0).await
        })
    }

    fn description(&self) -> Option<&str> {
        self.0.1.as_deref()
    }
}

impl UserFunc {
    pub fn new(stmt: Stmt, description: Option<String>) -> Self {
        Self(Rc::new((stmt, description)))
    }
}
