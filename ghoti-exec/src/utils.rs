use std::path::Path;

use super::{Error, ExecResult};

/// Builtin function names that prohibits overriding.
/// They will not be searched as autoload functions either.
///
/// From: <https://fishshell.com/docs/current/cmds/function.html>
const RESERVED_FUNC_NAMES: &[&str] = &[
    "[", "_", "and", "argparse", "begin", "break", "builtin", "case", "command", "continue",
    "else", "end", "eval", "exec", "for", "function", "if", "not", "or", "read", "return", "set",
    "status", "string", "switch", "test", "time", "while",
];

pub fn validate_variable_name(s: &str) -> ExecResult<&str> {
    if !s.is_empty() && s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        Ok(s)
    } else {
        Err(Error::InvalidIdentifier(s.to_owned()))
    }
}

pub fn validate_function_name(s: &str) -> ExecResult<&str> {
    if !s.is_empty() && !s.starts_with("-") && !s.contains('/') && !RESERVED_FUNC_NAMES.contains(&s)
    {
        Ok(s)
    } else {
        Err(Error::InvalidIdentifier(s.into()))
    }
}

pub fn access_can_read(p: &Path) -> bool {
    use rustix::fs::{Access, access};
    access(p, Access::READ_OK).is_ok()
}

pub fn access_can_exec(p: &Path) -> bool {
    use rustix::fs::{Access, access};
    access(p, Access::EXEC_OK).is_ok()
}
