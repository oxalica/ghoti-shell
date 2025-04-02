use std::fmt::Write;
use std::future::ready;
use std::ops::ControlFlow;
use std::str::FromStr;

use clap::Parser;
use either::Either;
use owo_colors::AnsiColors;
use tokio::io::{AsyncBufReadExt, AsyncRead};

use crate::command::{self, BoxCommand, UserFunc};
use crate::io::StdioCollectSink;
use crate::utils::validate_variable_name;
use crate::{Error, ExecContext, ExecResult, Status, VarScope, Variable};

macro_rules! bail {
    ($($msg:tt)+) => {
        return Err(crate::Error::Custom(format!($($msg)+)).into())
    };
}

macro_rules! ensure {
    ($cond:expr, $($msg:tt)+) => {
        if !$cond {
            bail!($($msg)+);
        }
    };
}

pub mod test;

pub fn all_builtins() -> impl ExactSizeIterator<Item = (&'static str, BoxCommand)> {
    [
        (
            "command",
            Box::new(command::parsed_builtin(command)) as BoxCommand,
        ),
        ("set", Box::new(command::parsed_builtin(set))),
        ("builtin", Box::new(command::parsed_builtin(builtin))),
        ("source", Box::new(command::raw_builtin(source))),
        ("test", Box::new(command::raw_builtin(test::test))),
        ("functions", Box::new(command::parsed_builtin(functions))),
        ("set_color", Box::new(command::parsed_builtin(set_color))),
    ]
    .into_iter()
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
            ctx.io().stdout.write_all(buf).await
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

pub async fn builtin(ctx: &mut ExecContext<'_>, args: BuiltinArgs) -> ExecResult<Option<Status>> {
    ensure!(
        !(args.names && args.query),
        "--names and --query are mutually exclusive"
    );
    if args.names {
        let mut names = ctx.builtins().map(|(name, _)| name).collect::<Vec<_>>();
        names.sort_unstable();
        let out = names.iter().flat_map(|&s| [s, "\n"]).collect::<String>();
        ctx.io().stdout.write_all(out).await?;
        Ok(Some(Status::SUCCESS))
    } else if args.query {
        let ok = args.args.iter().any(|name| ctx.get_builtin(name).is_some());
        Ok(Some(ok.into()))
    } else {
        ensure!(!args.args.is_empty(), "missing builtin name");
        let name = &args.args[0];
        let cmd = ctx
            .get_builtin(name)
            .ok_or_else(|| Error::CommandNotFound(name.into()))?;
        cmd.exec(ctx, &args.args).await;
        Ok(None)
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

    let io = ctx.io();
    let (mut os_stdin, stdin_sink) = io.stdin.to_stdio()?;
    if stdin_sink.is_some() {
        os_stdin = std::process::Stdio::null();
    }
    let (os_stdout, stdout_sink) = io.stdout.to_stdio()?;
    let (os_stderr, stderr_sink) = io.stderr.to_stdio()?;

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

    todo!();
}

pub async fn source(ctx: &mut ExecContext<'_>, args: &[String]) -> ExecResult<()> {
    let (path, text, args) = match args[1..].split_first() {
        Some((path, args)) => {
            let text = tokio::task::spawn_blocking({
                let path = path.clone();
                move || std::fs::read_to_string(path)
            })
            .await
            .expect("no panic")
            .map_err(Error::ReadWrite)?;
            (path.clone(), text, args)
        }
        None => {
            let text = ctx.io().stdin.read_to_string().await?;
            ("<stdin>".into(), text, &[][..])
        }
    };

    let scope = &mut **ctx.enter_local_scope();
    scope.set_var("argv", VarScope::Local, args.to_vec());
    scope.exec_source(Some(path), text).await;
    Ok(())
}

#[derive(Debug, Parser)]
pub struct FunctionsOpts {
    #[arg(long, short)]
    pub erase: bool,
    #[arg(long, short)]
    pub query: bool,

    pub funcs: Vec<String>,
}

pub async fn functions(ctx: &mut ExecContext<'_>, args: FunctionsOpts) -> ExecResult {
    ensure!(
        args.erase as u8 + args.query as u8 <= 1,
        "--erase and --query are mutually exclusive",
    );

    if args.erase {
        let fail_cnt = args
            .funcs
            .iter()
            .map(|name| !ctx.remove_global_func(name) as usize)
            .sum::<usize>();
        Ok(fail_cnt.into())
    } else if args.query {
        let fail_cnt = args
            .funcs
            .iter()
            .map(|name| ctx.get_func(name).is_none() as usize)
            .sum::<usize>();
        Ok(fail_cnt.into())
    } else {
        let mut buf = String::new();
        let mut fail_cnt = 0usize;
        for name in &args.funcs {
            if let Some(cmd) = ctx.get_func(name) {
                let func = cmd.as_any().downcast_ref::<UserFunc>().unwrap();
                writeln!(buf, "{:#?}", func.stmt()).unwrap();
            } else {
                fail_cnt += 1;
            }
        }
        let _: ExecResult<_> = ctx.io().stdout.write_all(buf).await;
        Ok(fail_cnt.into())
    }
}

#[derive(Debug, Parser)]
pub struct SetColorOpts {
    #[arg(long, short)]
    background: Option<SetColor>,

    #[arg(long, short = 'c')]
    print_colors: Option<SetColor>,

    #[arg(long, short = 'o')]
    bold: bool,

    #[arg(long, short)]
    dim: bool,

    #[arg(long, short)]
    italics: bool,

    #[arg(long, short)]
    reverse: bool,

    #[arg(long, short)]
    underline: bool,

    color: Option<SetColor>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SetColor {
    Normal,
    Ansi(AnsiColors),
    Rgb(u8, u8, u8),
}

impl FromStr for SetColor {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || format!("invalid color: {s:?}");
        Ok(Self::Ansi(match s {
            "normal" => return Ok(Self::Normal),

            "black" => AnsiColors::Black,
            "red" => AnsiColors::Red,
            "green" => AnsiColors::Green,
            "yellow" => AnsiColors::Yellow,
            "blue" => AnsiColors::Blue,
            "magenta" => AnsiColors::Magenta,
            "cyan" => AnsiColors::Cyan,
            "white" => AnsiColors::White,
            "brblack" => AnsiColors::BrightBlack,
            "brred" => AnsiColors::BrightRed,
            "brgreen" => AnsiColors::BrightGreen,
            "bryellow" => AnsiColors::BrightYellow,
            "brblue" => AnsiColors::BrightBlue,
            "brmagenta" => AnsiColors::BrightMagenta,
            "brcyan" => AnsiColors::BrightCyan,
            "brwhite" => AnsiColors::BrightWhite,

            s if s.len() == 3 => {
                let v = u32::from_str_radix(s, 16).map_err(|_| err())?;
                let f = |x: u32| x as u8 * 11;
                return Ok(Self::Rgb(f(v >> 8), f((v >> 4) & 0xF), f(v & 0xF)));
            }
            s if s.len() == 6 => {
                let v = u32::from_str_radix(s, 16).map_err(|_| err())?;
                let f = |x: u32| x as u8;
                return Ok(Self::Rgb(f(v >> 16), f((v >> 8) & 0xFF), f(v & 0xFF)));
            }
            _ => return Err(err()),
        }))
    }
}

pub async fn set_color(ctx: &mut ExecContext<'_>, args: SetColorOpts) -> ExecResult {
    let mut s = owo_colors::Style::new();
    match args.background {
        Some(SetColor::Normal) => s = s.on_default_color(),
        Some(SetColor::Ansi(c)) => s = s.on_color(c),
        Some(SetColor::Rgb(r, g, b)) => s = s.on_truecolor(r, g, b),
        None => {}
    }
    if args.bold {
        s = s.bold();
    }
    if args.dim {
        s = s.dimmed();
    }
    if args.italics {
        s = s.italic();
    }
    if args.reverse {
        s = s.reversed();
    }
    if args.underline {
        s = s.underline();
    }

    match args.color {
        Some(SetColor::Normal) => s = s.default_color(),
        Some(SetColor::Ansi(c)) => s = s.color(c),
        Some(SetColor::Rgb(r, g, b)) => s = s.truecolor(r, g, b),
        None => {}
    }

    let reset = if args.color == Some(SetColor::Normal) || args.background == Some(SetColor::Normal)
    {
        "\x1B[m"
    } else {
        ""
    };

    let s = format!("{reset}{}", s.prefix_formatter());
    if !s.is_empty() {
        let _: ExecResult<_> = ctx.io().stdout.write_all(s).await;
        Ok(Status::SUCCESS)
    } else {
        Ok(Status::FAILURE)
    }
}
