use logos::{Lexer, Logos, Span};

use crate::{ParseError, ParseErrorKind, SourceFile};

lalrpop_util::lalrpop_mod!(grammar, "/grammar.rs");

type Result<T, E = ParseError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
#[logos(skip r"([ \t\v\f]|#[^\r\n]*)+")]
enum Token<'i> {
    #[regex("\r?\n")]
    Newline,

    // Keywords.
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("function")]
    Function,
    #[token("return")]
    Return,
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    // Implemented as builtins: command, builtin, time, exec.

    // Punctuations.
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
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
    #[token("~/")]
    TildeSlash,

    #[token("&>")]
    AmpGt,
    #[regex(r"\d*<")]
    Lt(&'i str),
    #[regex(r"\d*<\?")]
    LtQus(&'i str),
    #[regex(r"\d*<&")]
    LtAmp(&'i str),
    #[regex(r"\d*>")]
    Gt(&'i str),
    #[regex(r"\d*>\?")]
    GtQus(&'i str),
    #[regex(r"\d*>&")]
    GtAmp(&'i str),
    #[regex(r"\d*>>")]
    GtGt(&'i str),
    #[regex(r"\d*>\|")]
    GtPipe(&'i str),

    // Words.
    #[regex(r"[\w%+,\-./=@^:!?\[\]]+")]
    #[regex(r#"\\\r?\n"#, |_| "")]
    #[regex(r#"\\."#, unescape_word_single)]
    Word(&'i str),

    #[regex(r#"\\x[0-9a-fA-F]{2}"#, |lex| unescape_unicode(&lex.slice()[2..], 16))]
    #[regex(r#"\\[0-7]{3}"#, |lex| unescape_unicode(&lex.slice()[2..], 8))]
    #[regex(r#"\\u[0-9a-fA-F]{4}"#, |lex| unescape_unicode(&lex.slice()[2..], 16))]
    #[regex(r#"\\U[0-9a-fA-F]{8}"#, |lex| unescape_unicode(&lex.slice()[2..], 16))]
    Escape(char),

    // String context.
    #[token("\"")]
    DQuote,
    #[regex(r#"\$+\w+"#)]
    Variable(&'i str),
    #[token("$(")]
    DollarLParen,
    Verbatim(&'i str),
    #[token("'")]
    SQuote,

    // Pseudo token.
    Join,
}

fn unescape_unicode(s: &str, radix: u32) -> Option<char> {
    char::from_u32(u32::from_str_radix(s, radix).ok()?)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum TokenDString<'i> {
    #[token("\"")]
    DQuote,
    #[regex(r#"\$+\w+"#)]
    Variable(&'i str),
    #[token("$(")]
    DollarLParen,
    #[regex(r#"[^"\\$]+"#)]
    #[regex(r"\\.", unescape_dquote)]
    Verbatim(&'i str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum TokenSString<'i> {
    #[regex(r"[^'\\]+")]
    #[regex(r"\\.", unescape_squote)]
    Verbatim(&'i str),
    #[token("'")]
    SQuote,
}

fn unescape_word_single<'i>(lex: &mut Lexer<'i, Token<'i>>) -> &'i str {
    match &lex.slice()[1..] {
        "\\" => "\\",
        "\"" => "\"",
        "'" => "'",
        "a" => "\x07",
        "e" => "\x1b",
        "n" => "\n",
        "r" => "\r",
        s => s,
    }
}

fn unescape_dquote<'i>(lex: &mut Lexer<'i, TokenDString<'i>>) -> &'i str {
    match lex.slice() {
        "\\\\" => "\\",
        "\\\"" => "\"",
        "\\$" => "$",
        s => s,
    }
}

fn unescape_squote<'i>(lex: &mut Lexer<'i, TokenSString<'i>>) -> &'i str {
    match lex.slice() {
        r"\'" => r"'",
        r"\\" => r"\",
        s => s,
    }
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
                | Token::Escape(_)
                | Token::Star
                | Token::TildeSlash
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
                | Token::Escape(_)
                | Token::Star
                | Token::TildeSlash
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

pub(crate) fn parse_source(src: &str) -> Result<SourceFile> {
    let tokens = lex(src)?;
    let parser = grammar::SourceFileParser::new();
    let lexer = tokens.iter().copied();
    let ast = parser.parse(lexer)?;
    Ok(ast)
}
