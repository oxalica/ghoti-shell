use std::fmt::Write;
use std::future::ready;
use std::ops::ControlFlow;
use std::process;
use std::rc::Rc;

use clap::Parser;
use either::Either;
use ghoti_syntax::parse_source;
use tokio::io::{AsyncBufReadExt, AsyncRead};

use crate::command::{self, BoxCommand, UnchangedStatus};
use crate::utils::validate_variable_name;
use crate::{
    Error, ExecBreak, ExecContext, ExecResult, Status, Stdio, StdioCollectSink, VarScope, Variable,
};

pub fn all_builtins() -> impl ExactSizeIterator<Item = (&'static str, BoxCommand)> {
    [
        (
            "command",
            Box::new(command::parsed_builtin(command)) as BoxCommand,
        ),
        ("set", Box::new(command::parsed_builtin(set))),
        ("builtin", Box::new(command::parsed_builtin(builtin))),
        ("source", Box::new(command::raw_builtin(source))),
    ]
    .into_iter()
}

macro_rules! ensure {
    ($cond:expr, $($msg:tt)+) => {
        if !$cond {
            return Err(Error::Custom(format!($($msg)+)).into());
        }
    };
}

#[derive(Debug, Parser)]
pub(crate) struct FunctionOpts {
    #[arg(long, short)]
    pub description: Option<String>,
}

// TODO
#[derive(Debug, Parser)]
pub struct SetArgs {
    #[arg(long, short)]
    local: bool,
    #[arg(long, short)]
    function: bool,
    #[arg(long, short)]
    global: bool,
    #[arg(long, short)]
    universal: bool,

    #[arg(long, short = 'x')]
    export: bool,

    #[arg(long, short = 'e')]
    erase: bool,

    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

pub async fn set(ctx: &mut ExecContext<'_>, args: SetArgs) -> ExecResult {
    let scope_flag_cnt = [args.local, args.function, args.global, args.universal]
        .iter()
        .map(|&b| b as u8)
        .sum::<u8>();
    ensure!(scope_flag_cnt <= 1, "scope flags are mutually exclusive");
    let scope = if args.local {
        VarScope::Local
    } else if args.function {
        VarScope::Function
    } else if args.global {
        VarScope::Global
    } else if args.universal {
        VarScope::Universal
    } else {
        VarScope::Auto
    };

    if args.erase {
        for name in &args.args {
            validate_variable_name(name)?;
        }
        let mut ok = true;
        for name in &args.args {
            if !ctx.remove_var(scope, name) {
                ok = false;
            }
        }
        return Ok(ok.into());
    }

    match args.args.split_first() {
        None => {
            let mut buf = String::new();
            ctx.list_vars::<()>(scope, |name, var| {
                if args.export && !var.export {
                    return ControlFlow::Continue(());
                }

                buf.push_str(name);
                if !var.value.is_empty() {
                    buf.push(' ');
                    for (idx, val) in var.value.iter().enumerate() {
                        if idx != 0 {
                            buf.push_str("  ");
                        }
                        write!(buf, "\"{}\"", val.escape_debug()).unwrap();
                    }
                }
                buf.push('\n');
                ControlFlow::Continue(())
            });
            ctx.io().write_stdout(buf).await
        }
        Some((name, vals)) => {
            validate_variable_name(name)?;
            ensure!(
                !ctx.has_special_var(name),
                "cannot modify special variable: {name:?}",
            );
            let var = Variable {
                value: vals.to_vec(),
                export: args.export,
            };
            ctx.set_var(name, scope, var);
            Ok(ctx.last_status())
        }
    }
}

#[derive(Debug, Parser)]
pub struct BuiltinArgs {
    #[arg(long, short)]
    names: bool,
    #[arg(long, short)]
    query: bool,

    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

pub async fn builtin(ctx: &mut ExecContext<'_>, args: BuiltinArgs) -> ExecResult {
    ensure!(
        !(args.names && args.query),
        "--names and --query are mutually exclusive"
    );
    if args.names {
        let mut names = ctx.builtins().map(|(name, _)| name).collect::<Vec<_>>();
        names.sort_unstable();
        let out = names.iter().flat_map(|&s| [s, "\n"]).collect::<String>();
        ctx.io().write_stdout(out).await
    } else if args.query {
        let ok = args.args.iter().any(|name| ctx.get_builtin(name).is_some());
        Ok(ok.into())
    } else {
        ensure!(!args.args.is_empty(), "missing builtin name");
        let name = &args.args[0];
        let cmd = ctx
            .get_builtin(name)
            .ok_or_else(|| Error::CommandNotFound(name.into()))?;
        Ok(cmd.exec(ctx, &args.args).await)
    }
}

#[derive(Debug, Parser)]
pub struct CommandArgs {
    #[arg(long, short)]
    pub all: bool,
    #[arg(long, short)]
    pub query: bool,
    #[arg(long, short)]
    pub search: bool,

    #[arg(trailing_var_arg = true)]
    pub args: Vec<String>,
}

pub async fn command(ctx: &mut ExecContext<'_>, args: CommandArgs) -> ExecResult {
    async fn copy_stdio_to_sink(
        rdr: impl AsyncRead + Unpin,
        sink: StdioCollectSink,
    ) -> ExecResult<()> {
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

    ensure!(!args.all && !args.query && !args.search, "TODO");

    let (cmd, args) = args.args.split_first().ok_or(Error::EmptyCommand)?;

    let cvt_stdio = |s: &Stdio, is_stdin: bool| {
        Ok(match s {
            Stdio::Inherit => (process::Stdio::inherit(), None),
            Stdio::Close => todo!(),
            Stdio::Collect(sink) => {
                if is_stdin {
                    todo!();
                }
                (process::Stdio::piped(), Some(Rc::clone(sink)))
            }
            Stdio::Raw(raw) => (
                (**raw).try_clone().map_err(Error::CloneHandle)?.into(),
                None,
            ),
        })
    };

    let io = ctx.io();
    let (os_stdin, _) = cvt_stdio(&io.stdin, true)?;
    let (os_stdout, stdout_sink) = cvt_stdio(&io.stdout, false)?;
    let (os_stderr, stderr_sink) = cvt_stdio(&io.stderr, false)?;
    let mut child = {
        let mut builder = tokio::process::Command::new(cmd);
        builder
            .kill_on_drop(true)
            .args(args)
            .stdin(os_stdin)
            .stdout(os_stdout)
            .stderr(os_stderr)
            .env_clear();
        ctx.list_vars::<()>(VarScope::Auto, |name, var| {
            if var.export {
                builder.env(name, var.value.join(" "));
            }
            ControlFlow::Continue(())
        });
        builder
            .spawn()
            .map_err(|err| Error::SpawnProcess(cmd.into(), err))?
    };

    let mut copy_stdout = Either::Left(ready(ExecResult::Ok(())));
    let mut copy_stderr = Either::Left(ready(ExecResult::Ok(())));
    if let Some(sink) = stdout_sink {
        copy_stdout = Either::Right(copy_stdio_to_sink(child.stdout.take().unwrap(), sink));
    }
    if let Some(sink) = stderr_sink {
        copy_stderr = Either::Right(copy_stdio_to_sink(child.stderr.take().unwrap(), sink));
    }

    let (ret_wait, ret_stdout, ret_stderr) = tokio::join!(child.wait(), copy_stdout, copy_stderr);
    ret_stdout?;
    ret_stderr?;
    let status = ret_wait.map_err(Error::WaitProcess)?;
    if status.success() {
        return Ok(Status::SUCCESS);
    }

    if let Some(code) = status.code() {
        return Ok(Status(code));
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            return Ok(Status(127 + sig));
        }
    }

    todo!();
}

pub async fn source(ctx: &mut ExecContext<'_>, args: &[String]) -> ExecResult<UnchangedStatus> {
    let Some((path, args)) = args[1..].split_first() else {
        return Err(Error::Custom("TODO: source from stdin".into()));
    };

    let source = {
        let src = tokio::fs::read_to_string(path)
            .await
            .map_err(Error::ReadWrite)?;
        parse_source(&src).map_err(|errs| Error::Custom(errs[0].to_string()))?
    };

    let scope = &mut **ctx.enter_local_scope();
    scope.set_var("argv", VarScope::Local, args.to_vec());
    match scope.exec_source(&source).await {
        ControlFlow::Continue(()) => {}
        ControlFlow::Break(ExecBreak::FuncReturn(n)) => scope.set_last_status(n),
        _ => unreachable!(),
    }

    ExecResult::Ok(UnchangedStatus)
}
