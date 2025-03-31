use std::ops::Range;

use thiserror::Error;

#[derive(Debug, PartialEq, Error)]
#[error("parse error at {start_pos}..{end_pos}: {kind}")]
pub struct ParseError {
    pub start_pos: usize,
    pub end_pos: usize,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn span(&self) -> Range<usize> {
        self.start_pos..self.end_pos
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseErrorKind {
    #[error("invalid token")]
    Lex,
    #[error("unexpected token, expecting: {0}")]
    UnexpectedToken(String),
    #[error("unexpected EOF, expecting: {0}")]
    UnexpectedEof(String),
    #[error("extra tokens")]
    ExtraToken,

    #[error("invalid redirection port")]
    InvalidRedirectPort,

    #[error("invalid '{0}' at this location")]
    Validation(&'static str),
}

impl ParseError {
    pub fn new(start_pos: usize, end_pos: usize, kind: ParseErrorKind) -> Self {
        Self {
            start_pos,
            end_pos,
            kind,
        }
    }
}

impl<T> From<lalrpop_util::ParseError<usize, T, ParseError>> for ParseError {
    fn from(err: lalrpop_util::ParseError<usize, T, ParseError>) -> Self {
        use lalrpop_util::ParseError as E;

        let (start_pos, end_pos, kind) = match err {
            E::UnrecognizedEof { location, expected } => (
                location,
                location,
                ParseErrorKind::UnexpectedEof(expected.join(", ")),
            ),
            E::UnrecognizedToken { token, expected } => (
                token.0,
                token.2,
                ParseErrorKind::UnexpectedToken(expected.join(", ")),
            ),
            E::ExtraToken { token } => (token.0, token.2, ParseErrorKind::ExtraToken),
            E::User { error } => return error,
            E::InvalidToken { .. } => unreachable!(),
        };

        Self::new(start_pos, end_pos, kind)
    }
}
