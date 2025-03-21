use crate::exec::validate_variable_name;

use super::{Command, ExecContext, ExecResult, Io};

macro_rules! def_all_builtins {
    ($($f:path $(=> $($empty:ident)?)?),* $(,)?) => {
        pub const ALL_BUILTINS: [(&str, Command); 0usize $(+ 1usize $($empty)?)*] = [
            $(
                (
                    stringify!($f),
                    Command::new_zst_fn(|ctx, args, io| Box::pin($f(ctx, args, io))),
                ),
            )*
        ];
    };
}

def_all_builtins! { set }

pub async fn set(ctx: &mut ExecContext<'_, '_>, args: &[String], _io: Io) -> ExecResult {
    match args {
        [_, name, vals @ ..] => {
            validate_variable_name(name)?;
            ctx.set_var(name, vals.iter().cloned());
            Ok(())
        }
        _ => todo!(),
    }
}
