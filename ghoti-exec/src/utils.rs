use ghoti_syntax::KEYWORDS;

use super::{Error, ExecResult};

pub fn validate_variable_name(s: &str) -> ExecResult<&str> {
    if !s.is_empty() && s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        Ok(s)
    } else {
        Err(Error::InvalidIdentifier(s.to_owned()))
    }
}

pub fn validate_function_name(s: &str) -> ExecResult<&str> {
    if !s.is_empty() && !s.starts_with("-") && !KEYWORDS.contains(&s) {
        Ok(s)
    } else {
        Err(Error::InvalidIdentifier(s.into()))
    }
}
