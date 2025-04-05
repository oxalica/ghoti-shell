use logos::{Lexer, Logos, SpannedIter};

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
    #[token("||")]
    PipePipe,
    #[token("&&")]
    AmpAmp,

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

    #[regex(r"\\[0-7]{3}", |lex| unescape_unicode::<8>(&lex.slice()[1..]))]
    #[regex(
        r"\\x[0-9a-fA-F]{2}|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}",
        |lex| unescape_unicode::<16>(&lex.slice()[2..])
    )]
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
    // Only special in braces.
    Comma,
}

impl Token<'_> {
    fn can_join_left(self) -> bool {
        matches!(
            self,
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

    fn can_join_right(self) -> bool {
        matches!(
            self,
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
}

fn unescape_unicode<const RADIX: u32>(s: &str) -> Option<char> {
    char::from_u32(u32::from_str_radix(s, RADIX).ok()?)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum TokenBrace<'i> {
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("\"")]
    DQuote,
    #[token("'")]
    SQuote,
    #[token("$(")]
    DollarLParen,

    #[regex(r"\$+\w+")]
    Variable(&'i str),
    #[token(",")]
    Comma,

    #[regex(r#"[^{,}"'$\\]+"#)]
    Verbatim(&'i str),
    #[regex(r"\\[0-7]{3}", |lex| unescape_unicode::<8>(&lex.slice()[1..]))]
    #[regex(
        r"\\x[0-9a-fA-F]{2}|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}",
        |lex| unescape_unicode::<16>(&lex.slice()[2..])
    )]
    Escape(char),
}

impl<'i> From<TokenBrace<'i>> for Token<'i> {
    fn from(tok: TokenBrace<'i>) -> Self {
        match tok {
            TokenBrace::LBrace => Token::LBrace,
            TokenBrace::Comma => Token::Comma,
            TokenBrace::RBrace => Token::RBrace,
            TokenBrace::DQuote => Token::DQuote,
            TokenBrace::SQuote => Token::SQuote,
            TokenBrace::Variable(v) => Token::Variable(v),
            TokenBrace::DollarLParen => Token::DollarLParen,
            TokenBrace::Verbatim(s) => Token::Verbatim(s),
            TokenBrace::Escape(c) => Token::Escape(c),
        }
    }
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

fn unescape_dquote<'i>(lex: &mut Lexer<'i, TokenDString<'i>>) -> &'i str {
    match lex.slice() {
        "\\\\" => "\\",
        "\\\"" => "\"",
        "\\$" => "$",
        s => s,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum TokenSString<'i> {
    #[regex(r"[^'\\]+")]
    #[regex(r"\\.", unescape_squote)]
    Verbatim(&'i str),
    #[token("'")]
    SQuote,
}

fn unescape_squote<'i>(lex: &mut Lexer<'i, TokenSString<'i>>) -> &'i str {
    match lex.slice() {
        r"\'" => r"'",
        r"\\" => r"\",
        s => s,
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

struct MyLexer<'i, 'o, Tok: Logos<'i>> {
    iter: SpannedIter<'i, Tok>,
    out: &'o mut Vec<(usize, Token<'i>, usize)>,
}

impl<'i, Tok: Logos<'i, Extras = ()> + Into<Token<'i>> + Copy> MyLexer<'i, '_, Tok> {
    fn next(&mut self) -> Result<Option<(usize, Tok, usize)>> {
        match self.iter.next() {
            None => Ok(None),
            Some((Ok(tok), span)) => Ok(Some((span.start, tok, span.end))),
            Some((Err(_), span)) => Err(ParseError::new(span.start, span.end, ParseErrorKind::Lex)),
        }
    }

    fn next_pushed(&mut self) -> Result<Option<Tok>> {
        Ok(match self.next()? {
            None => None,
            Some((lpos, tok, rpos)) => {
                self.out.push((lpos, tok.into(), rpos));
                Some(tok)
            }
        })
    }

    fn with_morphed<Tok2, T, F>(&mut self, f: F) -> T
    where
        Tok2: Logos<'i, Source = Tok::Source, Extras = ()> + Copy,
        F: FnOnce(&mut MyLexer<'i, '_, Tok2>) -> T,
    {
        let mut sublex = MyLexer {
            iter: (*self.iter).clone().morph().spanned(),
            out: self.out,
        };
        let v = f(&mut sublex);
        self.iter = (*sublex.iter).clone().morph().spanned();
        v
    }
}

fn lex_brace<'i>(lexer: &mut MyLexer<'i, '_, TokenBrace<'i>>) -> Result<()> {
    while let Some(tok) = lexer.next_pushed()? {
        match tok {
            TokenBrace::RBrace => break,
            TokenBrace::LBrace => lexer.with_morphed(lex_brace)?,
            TokenBrace::DollarLParen => lexer.with_morphed(lex_normal)?,
            TokenBrace::SQuote => lexer.with_morphed(lex_sstring)?,
            TokenBrace::DQuote => lexer.with_morphed(lex_dstring)?,
            TokenBrace::Variable(_)
            | TokenBrace::Escape(_)
            | TokenBrace::Verbatim(_)
            | TokenBrace::Comma => {}
        }
    }
    Ok(())
}

fn lex_sstring<'i>(lexer: &mut MyLexer<'i, '_, TokenSString<'i>>) -> Result<()> {
    while let Some(tok) = lexer.next_pushed()? {
        match tok {
            TokenSString::SQuote => break,
            TokenSString::Verbatim(_) => {}
        }
    }
    Ok(())
}

fn lex_dstring<'i>(lexer: &mut MyLexer<'i, '_, TokenDString<'i>>) -> Result<()> {
    while let Some(tok) = lexer.next_pushed()? {
        match tok {
            TokenDString::DQuote => break,
            TokenDString::DollarLParen => lexer.with_morphed(lex_normal)?,
            TokenDString::Variable(_) | TokenDString::Verbatim(_) => {}
        }
    }
    Ok(())
}

fn lex_normal<'i>(lexer: &mut MyLexer<'i, '_, Token<'i>>) -> Result<()> {
    while let Some((lpos, token, rpos)) = lexer.next()? {
        if token.can_join_left() {
            if let Some((_, prev, prev_end)) = lexer.out.last() {
                if *prev_end == lpos && prev.can_join_right() {
                    lexer.out.push((lpos, Token::Join, lpos));
                }
            }
        }
        lexer.out.push((lpos, token, rpos));

        match token {
            Token::SQuote => lexer.with_morphed(lex_sstring)?,
            Token::DQuote => lexer.with_morphed(lex_dstring)?,
            Token::LBrace => lexer.with_morphed(lex_brace)?,
            Token::DollarLParen | Token::LParen => lexer.with_morphed(lex_normal)?,
            Token::RParen => break,
            _ => {}
        }
    }
    Ok(())
}

fn lex(src: &str) -> Result<Vec<(usize, Token<'_>, usize)>> {
    let mut ret = Vec::new();
    let mut lexer = MyLexer {
        iter: Token::lexer(src).spanned(),
        out: &mut ret,
    };
    lex_normal(&mut lexer)?;
    if lexer.iter.next().is_some() {
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
