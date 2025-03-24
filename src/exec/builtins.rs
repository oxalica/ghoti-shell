use std::future::ready;
use std::process;
use std::rc::Rc;

use either::Either;
use tokio::io::{AsyncBufReadExt, AsyncRead};

use crate::exec::{Stdio, StdioCollectSink, validate_variable_name};

use super::{Command, Error, ExecContext, ExecResult, Io};

pub fn all_builtins() -> impl ExactSizeIterator<Item = (&'static str, Command)> {
    macro_rules! wrap_command {
        ($($f:path),* $(,)?) => {
            [
                $(
                    (
                        stringify!($f),
                        Command::new_native($f),
                    ),
                )*
            ]
        };
    }

    wrap_command! {
        set,
        builtin,
    }
    .into_iter()
}

pub async fn set(ctx: &mut ExecContext<'_, '_>, args: &[String], _io: Io) -> ExecResult {
    match args {
        [name, vals @ ..] => {
            validate_variable_name(name)?;
            ctx.set_var(name, vals.iter().cloned());
            Ok(())
        }
        _ => todo!(),
    }
}

pub async fn builtin(ctx: &mut ExecContext<'_, '_>, args: &[String], io: Io) -> ExecResult {
    match args {
        [cmd, args @ ..] if !cmd.starts_with("-") => {
            let cmd = ctx.get_builtin_func(cmd).ok_or_else(|| todo!())?.clone();
            cmd.exec(ctx, args, io).await
        }
        [o] if o == "--names" => {
            let mut names = ctx
                .builtin_funcs()
                .map(|(name, _)| name)
                .collect::<Vec<_>>();
            names.sort_unstable();
            let out = names.iter().flat_map(|&s| [s, "\n"]).collect::<String>();
            io.write_stdout(out)
        }
        [o, names @ ..] if o == "--query" => {
            if names
                .iter()
                .any(|name| ctx.get_builtin_func(name).is_some())
            {
                Ok(())
            } else {
                Err(Error::ExitCode(1))
            }
        }
        _ => todo!(),
    }
}

pub async fn command(_ctx: &mut ExecContext<'_, '_>, args: &[String], io: Io) -> ExecResult {
    async fn copy_stdio_to_sink(rdr: impl AsyncRead + Unpin, sink: StdioCollectSink) -> ExecResult {
        let mut stdout = tokio::io::BufReader::new(rdr);
        loop {
            let buf = stdout.fill_buf().await.map_err(Error::Io)?;
            if buf.is_empty() {
                return Ok(());
            }
            sink(buf)?;
            let len = buf.len();
            stdout.consume(len);
        }
    }

    let (cmd, args) = args.split_first().ok_or(Error::EmptyCommand)?;

    let cvt_stdio = |s: Stdio, is_stdin: bool| {
        Ok(match s {
            Stdio::Inherit => (process::Stdio::inherit(), None),
            Stdio::Close => todo!(),
            Stdio::Collect(sink) => {
                if is_stdin {
                    todo!();
                }
                (process::Stdio::piped(), Some(sink))
            }
            Stdio::Raw(raw) => (
                Rc::try_unwrap(raw)
                    .or_else(|raw| raw.try_clone().map_err(Error::CloneHandle))?
                    .into(),
                None,
            ),
        })
    };

    let (os_stdin, _) = cvt_stdio(io.stdin, true)?;
    let (os_stdout, stdout_sink) = cvt_stdio(io.stdout, false)?;
    let (os_stderr, stderr_sink) = cvt_stdio(io.stderr, false)?;
    let mut child = tokio::process::Command::new(cmd)
        .kill_on_drop(true)
        .args(args)
        .stdin(os_stdin)
        .stdout(os_stdout)
        .stderr(os_stderr)
        .spawn()
        .map_err(|err| Error::SpawnProcess(cmd.into(), err))?;

    let mut copy_stdout = Either::Left(ready(ExecResult::Ok(())));
    let mut copy_stderr = Either::Left(ready(ExecResult::Ok(())));
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
        return Ok(());
    }

    #[cfg(unix)]
    let code = {
        use std::os::unix::process::ExitStatusExt;
        status.into_raw()
    };

    Err(Error::ExitCode(code))
}
