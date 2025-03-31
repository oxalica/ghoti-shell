use std::fmt::Write;
use std::future::ready;
use std::ops::ControlFlow;
use std::process;
use std::rc::Rc;

use argh::FromArgs;
use either::Either;
use tokio::io::{AsyncBufReadExt, AsyncRead};

use crate::command::{BoxCommand, Builtin};
use crate::utils::validate_variable_name;
use crate::{
    Error, ExecContext, ExecResult, ExitStatus, Stdio, StdioCollectSink, VarScope, Variable,
};

pub fn all_builtins() -> impl ExactSizeIterator<Item = (&'static str, BoxCommand)> {
    [
        ("command", Box::new(Builtin::new(command)) as BoxCommand),
        ("set", Box::new(Builtin::new(set))),
        ("builtin", Box::new(Builtin::new(builtin))),
    ]
    .into_iter()
}

macro_rules! ensure {
    ($cond:expr, $($msg:tt)+) => {
        if !$cond {
            return Err(Error::Custom(format!($($msg)+)));
        }
    };
}

#[derive(Debug, FromArgs)]
pub(crate) struct FunctionOpts {
    #[argh(option, short = 'd')]
    pub description: Option<String>,
}

// TODO
#[derive(Debug, FromArgs)]
pub struct SetArgs {
    #[argh(switch, short = 'l')]
    local: bool,
    #[argh(switch, short = 'f')]
    function: bool,
    #[argh(switch, short = 'g')]
    global: bool,
    #[argh(switch, short = 'u')]
    universal: bool,

    #[argh(switch, short = 'x')]
    export: bool,

    #[argh(positional, greedy)]
    args: Vec<String>,
}

pub async fn set(ctx: &mut ExecContext<'_>, args: SetArgs) -> ExecResult {
    let scope_flag_cnt = [args.local, args.function, args.global, args.universal]
        .iter()
        .map(|&b| b as u8)
        .sum::<u8>();
    ensure!(scope_flag_cnt <= 1, "scope flags are mutually exclusive");
    let scope = if args.local {
        Some(VarScope::Local)
    } else if args.function {
        Some(VarScope::Function)
    } else if args.global {
        Some(VarScope::Global)
    } else if args.universal {
        Some(VarScope::Universal)
    } else {
        None
    };

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
            ctx.io().write_stdout(buf)
        }
        Some((name, vals)) => {
            validate_variable_name(name)?;
            let var = Variable {
                value: vals.to_vec(),
                export: args.export,
            };
            ctx.set_var(name, scope.unwrap_or(VarScope::Function), var);
            Ok(ExitStatus::SUCCESS)
        }
    }
}

#[derive(Debug, FromArgs)]
pub struct BuiltinArgs {
    #[argh(switch, short = 'n')]
    names: bool,
    #[argh(switch, short = 'q')]
    query: bool,

    #[argh(positional, greedy)]
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
        ctx.io().write_stdout(out)
    } else if args.query {
        let ok = args.args.iter().any(|name| ctx.get_builtin(name).is_some());
        Ok(ok.into())
    } else {
        ensure!(!args.args.is_empty(), "missing builtin name");
        let cmd = ctx
            .get_builtin(&args.args[0])
            .ok_or_else(|| todo!())?
            .clone();
        cmd.exec(ctx, &args.args).await
    }
}

#[derive(Debug, FromArgs)]
pub struct CommandArgs {
    #[argh(switch, short = 'a')]
    pub all: bool,
    #[argh(switch, short = 'q')]
    pub query: bool,
    #[argh(switch, short = 's')]
    pub search: bool,

    #[argh(positional, greedy)]
    pub args: Vec<String>,
}

pub async fn command(ctx: &mut ExecContext<'_>, args: CommandArgs) -> ExecResult {
    async fn copy_stdio_to_sink(rdr: impl AsyncRead + Unpin, sink: StdioCollectSink) -> ExecResult {
        let mut stdout = tokio::io::BufReader::new(rdr);
        loop {
            let buf = stdout.fill_buf().await.map_err(Error::Io)?;
            if buf.is_empty() {
                return Ok(ExitStatus::SUCCESS);
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
        ctx.list_vars::<()>(None, |name, var| {
            if var.export {
                builder.env(name, var.value.join(" "));
            }
            ControlFlow::Continue(())
        });
        builder
            .spawn()
            .map_err(|err| Error::SpawnProcess(cmd.into(), err))?
    };

    let mut copy_stdout = Either::Left(ready(ExecResult::Ok(ExitStatus::SUCCESS)));
    let mut copy_stderr = Either::Left(ready(ExecResult::Ok(ExitStatus::SUCCESS)));
    if let Some(sink) = stdout_sink {
        copy_stdout = Either::Right(copy_stdio_to_sink(child.stdout.take().unwrap(), sink));
    }
    if let Some(sink) = stderr_sink {
        copy_stderr = Either::Right(copy_stdio_to_sink(child.stderr.take().unwrap(), sink));
    }

    // FIXME: Check forwarding failure?
    let (ret_wait, _ret_stdout, _ret_stderr) = tokio::join!(child.wait(), copy_stdout, copy_stderr);
    let status = ret_wait.map_err(Error::WaitProcess)?;
    if status.success() {
        return Ok(ExitStatus::SUCCESS);
    }

    if let Some(code) = status.code() {
        return Ok(ExitStatus(code));
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            return Ok(ExitStatus(127 + sig));
        }
    }

    todo!();
}
