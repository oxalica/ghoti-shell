use std::fmt::Write;
use std::ops::ControlFlow;
use std::str::FromStr;

use clap::{Args, Parser};
use owo_colors::AnsiColors;

use crate::command::{self, BoxCommand, UserFunc};
use crate::utils::validate_variable_name;
use crate::{Error, ExecContext, ExecResult, LocateExternalCommand, Status, VarScope, Variable};

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
        ("echo", Box::new(command::raw_builtin(echo))),
        ("type", Box::new(command::parsed_builtin(type_))),
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
pub struct SetOpts {
    #[command(flatten)]
    op: SetOp,

    #[command(flatten)]
    scope: SetScope,

    #[command(flatten)]
    attr: SetVarAttr,

    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

#[derive(Debug, Args)]
#[group(required = false, multiple = false)]
pub struct SetScope {
    #[arg(long, short)]
    local: bool,
    #[arg(long, short)]
    function: bool,
    #[arg(long, short)]
    global: bool,
    #[arg(long, short = 'U')]
    universal: bool,
}

#[derive(Debug, Args)]
#[group(required = false, multiple = false)]
pub struct SetOp {
    #[arg(long, short)]
    erase: bool,
    #[arg(long, short)]
    query: bool,
}

#[derive(Debug, Args)]
#[group(required = false, multiple = false)]
pub struct SetVarAttr {
    #[arg(long, short = 'x')]
    export: bool,
    #[arg(long, short)]
    unexport: bool,
}

pub async fn set(ctx: &mut ExecContext<'_>, args: SetOpts) -> ExecResult<Option<Status>> {
    let scope = if args.scope.local {
        VarScope::Local
    } else if args.scope.function {
        VarScope::Function
    } else if args.scope.global {
        VarScope::Global
    } else if args.scope.universal {
        VarScope::Universal
    } else {
        VarScope::Auto
    };

    if args.op.erase || args.op.query {
        ensure!(
            !args.attr.export && !args.attr.unexport,
            "--export or --unexport can only be used for setting or listing variables",
        );
    }

    if args.op.query {
        let mut fail_cnt = 0usize;
        for name in &args.args {
            ensure!(scope == VarScope::Auto, "TODO");
            if ctx.get_var(name).is_none() {
                fail_cnt += 1;
            }
        }
        Ok(Some(fail_cnt.into()))
    } else if args.op.erase {
        for name in &args.args {
            validate_variable_name(name)?;
        }
        let mut fail_cnt = 0usize;
        for name in &args.args {
            if !ctx.remove_var(scope, name) {
                fail_cnt += 1;
            }
        }
        Ok(Some(fail_cnt.into()))
    } else if let Some((name, vals)) = args.args.split_first() {
        validate_variable_name(name)?;
        ensure!(
            !ctx.has_special_var(name),
            "cannot modify special variable: {name:?}",
        );
        let mut var = Variable::new_list(vals.to_vec());
        var.export = args.attr.export;
        ctx.set_var(name, scope, var);
        // Keep previous status.
        Ok(None)
    } else {
        let mut buf = String::new();
        ctx.list_vars::<()>(scope, |name, var| {
            if args.attr.export && !var.export || args.attr.unexport && var.export {
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
        let _: ExecResult<_> = ctx.io().stdout.write_all(buf).await;
        Ok(Some(Status::SUCCESS))
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
    pub query: bool,
    #[arg(long, short)]
    pub search: bool,

    #[arg(trailing_var_arg = true)]
    pub args: Vec<String>,
}

pub async fn command(ctx: &mut ExecContext<'_>, args: CommandArgs) -> ExecResult<Status> {
    if args.query {
        let mut found = false;
        for name in &args.args {
            if let LocateExternalCommand::ExecFile(_) = ctx.locate_external_command(name) {
                found = true;
                break;
            }
        }
        Ok(found.into())
    } else if args.search {
        let mut found = true;
        let mut buf = String::new();
        for name in &args.args {
            if let LocateExternalCommand::ExecFile(path) = ctx.locate_external_command(name) {
                found = true;
                writeln!(buf, "{}", path.display()).unwrap();
            }
        }
        let _: ExecResult<_> = ctx.io().stdout.write_all(buf).await;
        Ok(found.into())
    } else {
        let cmd = args.args.first().ok_or(Error::EmptyCommand)?;
        let exe_path = match ctx.locate_external_command(cmd) {
            LocateExternalCommand::ExecFile(path) => path,
            LocateExternalCommand::NotExecFile(path) | LocateExternalCommand::Dir(path) => {
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
        ctx.exec_external_command(&exe_path, &args.args).await
    }
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
    #[arg(long, short)]
    pub all: bool,

    pub funcs: Vec<String>,
}

pub async fn functions(ctx: &mut ExecContext<'_>, args: FunctionsOpts) -> ExecResult {
    ensure!(
        args.erase as u8 + args.query as u8 <= 1,
        "--erase and --query are mutually exclusive",
    );
    ensure!(
        !args.all || args.funcs.is_empty(),
        "--all can only be used without positional arguments"
    );

    if args.erase {
        let fail_cnt = args
            .funcs
            .iter()
            .map(|name| !ctx.remove_global_func(name) as usize)
            .sum::<usize>();
        Ok(fail_cnt.into())
    } else if args.query {
        let mut fail_cnt = 0usize;
        for name in &args.funcs {
            if ctx.get_or_autoload_func(name).await.is_none() {
                fail_cnt += 1;
            }
        }
        Ok(fail_cnt.into())
    } else if args.funcs.is_empty() {
        let mut buf = String::new();
        ctx.list_funcs::<()>(|name, _cmd| {
            if args.all || !name.starts_with("_") {
                writeln!(buf, "{name}").unwrap();
            }
            ControlFlow::Continue(())
        })
        .await;
        let _: ExecResult<_> = ctx.io().stdout.write_all(buf).await;
        Ok(Status::SUCCESS)
    } else {
        let mut buf = String::new();
        let mut fail_cnt = 0usize;
        for name in &args.funcs {
            if let Some(cmd) = ctx.get_or_autoload_func(name).await {
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

pub async fn echo(ctx: &mut ExecContext<'_>, args: &[String]) -> ExecResult {
    let mut newline = true;
    let mut unescape = false;
    let mut space = true;

    let mut iter = args[1..].iter();
    let mut buf = String::new();
    let mut first = true;
    for el in iter.by_ref() {
        match &**el {
            "-n" => newline = false,
            "-s" => space = false,
            "-E" => unescape = false,
            "-e" => unescape = true,
            "--" => break,
            _ => {
                buf.push_str(el);
                first = false;
                break;
            }
        }
    }

    for el in iter {
        if first {
            first = false;
        } else if space {
            buf.push(' ');
        }
        buf.push_str(el);
    }
    if newline {
        buf.push('\n');
    }

    ensure!(!unescape, "TODO");

    ctx.io().stdout.write_all(buf).await
}

#[derive(Debug, Parser)]
pub struct TypeArgs {
    pub names: Vec<String>,
}

pub async fn type_(ctx: &mut ExecContext<'_>, args: TypeArgs) -> ExecResult {
    let mut buf = String::new();
    let mut failed = 0usize;
    for name in &args.names {
        if ctx.get_global_func(name).is_some() {
            // TODO: function source
            writeln!(buf, "{name} is a function").unwrap();
        } else if ctx.get_builtin(name).is_some() {
            writeln!(buf, "{name} is a builtin").unwrap();
        } else {
            match ctx.locate_external_command(name) {
                LocateExternalCommand::ExecFile(path) => {
                    writeln!(buf, "{name} is {}", path.display()).unwrap();
                }
                LocateExternalCommand::NotExecFile(_)
                | LocateExternalCommand::Dir(_)
                | LocateExternalCommand::NotFound => {
                    writeln!(buf, "type: cannot find {name:?}").unwrap();
                    failed += 1;
                }
            }
        }
    }

    let _: ExecResult<_> = ctx.io().stdout.write_all(buf).await;
    Ok(failed.into())
}
