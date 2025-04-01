//! This is `test(1)`, not a testing mod.
//!
//! We use stricter syntax here and it's not POSIX- or fish-compatible.
//! Specifically,
//! 1. Unary primitives always consume the next argument which must be present.
//!    They also cannot take a compound expression as argument.
//! 2. Cannot use bare strings as booleans, `-z` or `-n` must be used.
//! 3. `!`, `(`, `)` and strings starting with `-` are forbidden in binary operations.
//!
//! ```bnf
//! ; Expr must not contains "]" in either form.
//! FullArgs := "test" Expr
//!           | "[" Expr "]"
//!
//! BoolExpr := ["-f" | "-z" | ..] STR
//!           | STR_NOT_OP_LIKE ["-ne" | "-lt" | ..] STR_NOT_OP_LIKE
//!           | "(" Expr ")"
//! NotExpr  := BoolExpr
//!           | "!" NotExpr
//! AndExpr  := NotExpr
//!           | NotExpr "-a" AndExpr
//! OrExpr   := AndExpr
//!           | AndExpr "-o" OrExpr
//! Expr     := OrExpr
//! ```

use std::fs;
use std::io::IsTerminal;
use std::iter::Peekable;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;

use rustix::fs::{Access, Mode, access};

use crate::{ExecContext, ExecResult};

pub async fn test(_ctx: &mut ExecContext<'_>, mut args: &[String]) -> ExecResult<bool> {
    let rbracket_pos = args.iter().position(|s| s == "]").unwrap_or(args.len());
    if args[0] == "[" {
        ensure!(rbracket_pos + 1 == args.len(), "must have ']' at last");
        args = &args[1..rbracket_pos];
    } else {
        ensure!(rbracket_pos == args.len(), "must not have ']'");
        args = &args[1..];
    }

    let mut iter = args.iter().map(|s| s.as_str()).peekable();
    let ret = eval(&mut iter)?;
    let tail = iter.next();
    ensure!(tail.is_none(), "unexpected tail {tail:?}");
    Ok(ret)
}

fn eval<'i>(iter: &mut Peekable<impl Iterator<Item = &'i str>>) -> ExecResult<bool> {
    let mut fst = eval_and(iter)?;
    while iter.next_if_eq(&"-o").is_some() {
        fst |= eval_and(iter)?;
    }
    Ok(fst)
}

fn eval_and<'i>(iter: &mut Peekable<impl Iterator<Item = &'i str>>) -> ExecResult<bool> {
    let mut fst = eval_atom(iter)?;
    while iter.next_if_eq(&"-a").is_some() {
        fst &= eval_atom(iter)?;
    }
    Ok(fst)
}

fn eval_atom<'i>(iter: &mut Peekable<impl Iterator<Item = &'i str>>) -> ExecResult<bool> {
    let fst = iter.next().ok_or_else(|| "missing argument".to_owned())?;

    let unary = match fst {
        "-b" => path_is_block_dev,
        "-c" => path_is_char_dev,
        "-d" => path_is_dir,
        "-e" => path_exists,
        "-f" => path_is_regular_file,
        "-g" => path_is_setgid,
        "-G" => todo!(),
        "-k" => path_is_sticky,
        "-L" => path_is_symlink,
        "-O" => todo!(),
        "-p" => path_is_fifo,
        "-r" => path_can_read,
        "-s" => path_is_nonempty,
        "-S" => path_is_socket,
        "-t" => fd_is_tty,
        "-u" => path_is_setuid,
        "-w" => path_can_write,
        "-x" => path_can_exec,

        "-z" => str_is_empty,
        "-n" => |a: &_| !str_is_empty(a),

        "(" => {
            let val = eval(iter)?;
            let next = iter.next();
            ensure!(next == Some(")"), "expecting ')' but got {next:?}");
            return Ok(val);
        }
        "!" => return Ok(!eval_atom(iter)?),
        _ if fst == ")" || fst.starts_with('-') => bail!("invalid string operand {fst:?}"),
        _ => {
            let binop = iter
                .next()
                .ok_or_else(|| format!("missing binary op after {fst:?}"))?;
            let binary = match binop {
                "-eq" => num_eq as fn(&str, &str) -> bool,
                "-ne" => |a: &_, b: &_| !num_eq(a, b),
                "-lt" => num_lt,
                "-gt" => |a: &_, b: &_| num_lt(b, a),
                "-ge" => |a: &_, b: &_| !num_lt(a, b),
                "-le" => |a: &_, b: &_| !num_lt(b, a),
                op => bail!("expecting a binary op but got {op:?}"),
            };
            let snd = iter
                .next()
                .ok_or_else(|| format!("missing second operand for '{binop}'"))?;
            ensure!(
                !["(", "!", ")"].contains(&snd) && !snd.starts_with('-'),
                "invalid string operand {snd:?}",
            );
            return Ok(binary(fst, snd));
        }
    };

    let arg = iter
        .next()
        .ok_or_else(|| format!("missing arg for '{fst}'"))?;
    Ok(unary(arg))
}

fn path_exists(path: &str) -> bool {
    Path::new(path).exists()
}

fn path_is_symlink(path: &str) -> bool {
    Path::new(path).is_symlink()
}

fn path_is_dir(path: &str) -> bool {
    Path::new(path).is_dir()
}

fn path_is_regular_file(path: &str) -> bool {
    Path::new(path).is_file()
}

fn path_is_nonempty(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| m.size() > 0)
}

fn path_is_block_dev(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| m.file_type().is_block_device())
}

fn path_is_char_dev(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| m.file_type().is_char_device())
}

fn path_is_fifo(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| m.file_type().is_fifo())
}

fn path_is_socket(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| m.file_type().is_socket())
}

fn path_is_setuid(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| Mode::from_bits_retain(m.mode()).contains(Mode::SUID))
}

fn path_is_setgid(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| Mode::from_bits_retain(m.mode()).contains(Mode::SGID))
}

fn path_is_sticky(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|m| Mode::from_bits_retain(m.mode()).contains(Mode::SVTX))
}

fn path_can_read(path: &str) -> bool {
    access(path, Access::READ_OK).is_ok()
}

fn path_can_write(path: &str) -> bool {
    access(path, Access::WRITE_OK).is_ok()
}

fn path_can_exec(path: &str) -> bool {
    access(path, Access::EXEC_OK).is_ok()
}

fn fd_is_tty(fd: &str) -> bool {
    match fd {
        "0" => std::io::stdin().is_terminal(),
        "1" => std::io::stdout().is_terminal(),
        "2" => std::io::stderr().is_terminal(),
        // Not safe.
        _ => false,
    }
}

fn str_is_empty(s: &str) -> bool {
    s.is_empty()
}

fn num_eq(a: &str, b: &str) -> bool {
    match (a.parse::<f64>(), b.parse::<f64>()) {
        (Ok(a), Ok(b)) => a == b,
        _ => false,
    }
}

fn num_lt(a: &str, b: &str) -> bool {
    match (a.parse::<f64>(), b.parse::<f64>()) {
        (Ok(a), Ok(b)) => a < b,
        _ => false,
    }
}
