use ghoti_syntax::KEYWORDS;

use super::{Error, ExecResult};

pub fn validate_variable_name(s: &str) -> ExecResult<&str> {
    if !s.is_empty() && s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        Ok(s)
    } else {
        Err(Error::InvalidateIdentifier(s.to_owned()))
    }
}

pub fn validate_variable_words(ws: &[String]) -> ExecResult<&str> {
    match ws {
        [s] => validate_variable_name(s),
        _ => Err(Error::InvalidateIdentifierWords(ws.len())),
    }
}

pub fn validate_function_name(s: &str) -> ExecResult<&str> {
    if !s.is_empty() && !s.starts_with("-") && !KEYWORDS.contains(&s) {
        Ok(s)
    } else {
        Err(Error::InvalidateIdentifier(s.into()))
    }
}
