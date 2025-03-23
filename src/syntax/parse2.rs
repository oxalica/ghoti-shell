use std::ops::Range;

use logos::{Lexer, Logos, Span};
use thiserror::Error;

use super::SourceFile;

lalrpop_util::lalrpop_mod!(grammar, "/syntax/grammar.rs");

type Result<T, E = ParseError> = std::result::Result<T, E>;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
#[logos(skip r"([ \t\v\f]|#[^\r\n]*)+")]
enum Token<'i> {
    #[regex("\r?\n")]
    Newline,

    // Keywords.
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("; and")]
    SemiAnd,
    #[token("; or")]
    SemiOr,
    #[token("not")]
    Not,
    #[token("function")]
    Function,

    // Punctuations.
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(">")]
    Gt,
    #[token(">?")]
    GtQus,
    #[token(">&")]
    GtAmp,
    #[token(">>")]
    GtGt,
    #[token("<")]
    Lt,
    #[token("<?")]
    LtQus,
    #[token("<&")]
    LtAmp,
    #[token("&")]
    Amp,
    #[token("?")]
    Qus,
    #[token("|")]
    Pipe,
    #[token("&|")]
    AmpPipe,
    #[token(";")]
    Semi,
    #[token("*")]
    Star,
    #[token("**")]
    StarStar,
    #[token("~")]
    Tilde,

    // Words.
    #[regex(r"[\w%+,\-./=@^]+")]
    #[regex(r#"\\\r?\n"#, |_| "")]
    #[token(r#"\\"#, |_| "\\")]
    #[token(r#"\""#, |_| "\"")]
    #[token(r#"\'"#, |_| "'")]
    #[token(r#"\a"#, |_| "\x07")]
    #[token(r#"\e"#, |_| "\x1b")]
    #[token(r#"\n"#, |_| "\n")]
    #[token(r#"\r"#, |_| "\r")]
    Word(&'i str),

    // String context.
    #[token("\"")]
    DQuote,
    #[regex(r#"\$\w+"#, |lexer| &lexer.slice()[1..])]
    Variable(&'i str),
    #[token("$(")]
    DollarLParen,
    Verbatim(&'i str),
    #[token("'")]
    SQuote,

    // Pseudo token.
    Join,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum TokenDString<'i> {
    #[token("\"")]
    DQuote,
    #[regex(r#"\$\w+"#)]
    Variable(&'i str),
    #[token("$(")]
    DollarLParen,
    #[regex(r#"[^"\\$]+"#)]
    #[token(r#"\\"#, |_| "\\")]
    #[token(r#"\""#, |_| "\"")]
    #[token(r#"\$"#, |_| "$")]
    Verbatim(&'i str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum TokenSString<'i> {
    #[regex(r#"[^'\\]+"#)]
    #[token(r#"\\"#, |_| "\\")]
    #[token(r#"\'"#, |_| "'")]
    Verbatim(&'i str),
    #[token("'")]
    SQuote,
}

impl<'i> From<TokenDString<'i>> for Token<'i> {
    fn from(tok: TokenDString<'i>) -> Self {
        match tok {
            TokenDString::DQuote => Token::DQuote,
            TokenDString::DollarLParen => Token::DollarLParen,
            TokenDString::Variable(v) => Token::Variable(v),
            TokenDString::Verbatim(v) => Token::Verbatim(v),
        }
    }
}

impl<'i> From<TokenSString<'i>> for Token<'i> {
    fn from(tok: TokenSString<'i>) -> Self {
        match tok {
            TokenSString::Verbatim(v) => Token::Verbatim(v),
            TokenSString::SQuote => Token::SQuote,
        }
    }
}

fn lex(src: &str) -> Result<Vec<(usize, Token<'_>, usize)>> {
    fn can_join_left(tok: Token<'_>) -> bool {
        matches!(
            tok,
            Token::Word(_)
                | Token::Gt
                | Token::GtQus
                | Token::GtAmp
                | Token::GtGt
                | Token::Lt
                | Token::LtAmp
                | Token::LtQus
                | Token::Amp
                | Token::Star
                | Token::Tilde
                | Token::DollarLParen
                | Token::LBrace
                | Token::DQuote
                | Token::SQuote
                | Token::Variable(_)
        )
    }
    fn can_join_right(tok: Token<'_>) -> bool {
        matches!(
            tok,
            Token::Word(_)
                | Token::Gt
                | Token::GtQus
                | Token::GtAmp
                | Token::GtGt
                | Token::Lt
                | Token::LtAmp
                | Token::LtQus
                | Token::Amp
                | Token::Star
                | Token::Tilde
                | Token::RParen
                | Token::RBrace
                | Token::DQuote
                | Token::SQuote
                | Token::Variable(_)
        )
    }

    fn cvt_ret<Tok, E>(opt: Option<(Result<Tok, E>, Span)>) -> Result<Option<(usize, Tok, usize)>> {
        opt.map(|(ret, span)| {
            let tok =
                ret.map_err(|_| ParseError::new(span.start, span.end, ParseErrorKind::Lex))?;
            Ok((span.start, tok, span.end))
        })
        .transpose()
    }

    type Buf<'i> = Vec<(usize, Token<'i>, usize)>;
    type NormalLexer<'i> = Lexer<'i, Token<'i>>;

    fn lex_sstring<'i>(ret: &mut Buf<'i>, outer: &mut NormalLexer<'i>) -> Result<()> {
        let mut lexer = (*outer).clone().morph::<TokenSString<'i>>().spanned();
        while let Some((lpos, token, rpos)) = cvt_ret(lexer.next())? {
            ret.push((lpos, token.into(), rpos));
            if token == TokenSString::SQuote {
                break;
            }
        }
        *outer = (*lexer).clone().morph();
        Ok(())
    }

    fn lex_dstring<'i>(ret: &mut Buf<'i>, outer: &mut NormalLexer<'i>) -> Result<()> {
        let mut lexer = (*outer).clone().morph::<TokenDString<'i>>().spanned();
        while let Some((lpos, token, rpos)) = cvt_ret(lexer.next())? {
            ret.push((lpos, token.into(), rpos));
            match token {
                TokenDString::DQuote => break,
                TokenDString::DollarLParen => {
                    *outer = (*lexer).clone().morph();
                    lex_normal(ret, outer)?;
                    lexer = (*outer).clone().morph::<TokenDString<'i>>().spanned();
                }
                _ => {}
            }
        }
        *outer = (*lexer).clone().morph();
        Ok(())
    }

    fn lex_normal<'i>(ret: &mut Buf<'i>, outer: &mut NormalLexer<'i>) -> Result<()> {
        let mut lexer = (*outer).clone().spanned();
        let mut in_paren = 0usize;
        while let Some((lpos, token, rpos)) = cvt_ret(lexer.next())? {
            if can_join_left(token) {
                if let Some((_, prev, prev_end)) = ret.last() {
                    if *prev_end == lpos && can_join_right(*prev) {
                        ret.push((lpos, Token::Join, lpos));
                    }
                }
            }
            ret.push((lpos, token, rpos));

            match token {
                Token::SQuote => lex_sstring(ret, &mut lexer)?,
                Token::DQuote => lex_dstring(ret, &mut lexer)?,
                Token::DollarLParen | Token::LParen => in_paren += 1,
                Token::RParen => {
                    if in_paren == 0 {
                        break;
                    }
                    in_paren -= 1;
                }
                _ => {}
            }
        }
        *outer = (*lexer).clone();
        Ok(())
    }

    let mut ret = Vec::new();
    let mut lexer = Token::lexer(src);
    lex_normal(&mut ret, &mut lexer)?;
    if lexer.next().is_some() {
        // The previous closing RParen.
        let (lpos, _, rpos) = *ret.last().unwrap();
        return Err(ParseError::new(lpos, rpos, ParseErrorKind::Lex));
    }
    Ok(ret)
}

pub fn parse_source(src: &str) -> Result<SourceFile> {
    let tokens = lex(src)?;
    let parser = grammar::SourceFileParser::new();
    let lexer = tokens.iter().copied();
    let ast = parser.parse(lexer)?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::parse_source;

    #[test]
    fn smoke() {
        let src = r#"
            #!shebang
            echo hello$(world) &2>>o <i | cat; or true
            not $true; and "th$is\$" or that
        "#;
        let ast = parse_source(src).unwrap();
        println!("{ast:?}");
    }
}
