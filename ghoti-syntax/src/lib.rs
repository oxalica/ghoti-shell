use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

mod error;
pub mod parse;
mod validate;
pub mod visit;

#[cfg(test)]
mod tests;

pub use error::{ParseError, ParseErrorKind};

pub const KEYWORDS: &[&str] = &[
    "begin", "end", "if", "else", "switch", "case", "for", "in", "and", "or", "not", "function",
];

pub type Pos = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Command(Pos, Words),
    Block(Pos, Vec<Stmt>),
    If(Pos, Box<Stmt>, Box<Stmt>, Option<Box<Stmt>>),
    For(Pos, Word, Words, Box<Stmt>),
    While(Pos, Box<Stmt>, Box<Stmt>),
    Break(Pos),
    Continue(Pos),
    Function(Pos, Words, Box<Stmt>),
    Return(Pos, Option<Word>),
    Switch(Pos, Word, Vec<SwitchCase>),

    Redirect(Pos, Box<Stmt>, Vec<Redirect>),
    Pipe(Pos, RedirectPort, Box<Stmt>, Box<Stmt>),

    Not(Pos, Box<Stmt>),
    And(Pos, Box<Stmt>),
    Or(Pos, Box<Stmt>),
}

impl Stmt {
    pub fn pos(&self) -> u32 {
        match self {
            Stmt::Command(pos, ..)
            | Stmt::Block(pos, ..)
            | Stmt::If(pos, ..)
            | Stmt::For(pos, ..)
            | Stmt::While(pos, ..)
            | Stmt::Break(pos, ..)
            | Stmt::Continue(pos, ..)
            | Stmt::Function(pos, ..)
            | Stmt::Return(pos, ..)
            | Stmt::Switch(pos, ..)
            | Stmt::Redirect(pos, ..)
            | Stmt::Pipe(pos, ..)
            | Stmt::Not(pos, ..)
            | Stmt::And(pos, ..)
            | Stmt::Or(pos, ..) => *pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    pub globs: Vec<Word>,
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Redirect {
    pub port: RedirectPort,
    pub mode: RedirectMode,
    pub dest: RedirectDest,
}

impl Redirect {
    pub fn new(port: RedirectPort, mode: RedirectMode, dest: RedirectDest) -> Self {
        Self { port, mode, dest }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RedirectPort(u32);

impl RedirectPort {
    pub const STDIN: Self = Self(0);
    pub const STDOUT: Self = Self(1);
    pub const STDERR: Self = Self(2);
    pub const STDOUT_STDERR: Self = Self(!0);
}

impl fmt::Debug for RedirectPort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match *self {
            Self::STDIN => "STDIN",
            Self::STDOUT => "STDOUT",
            Self::STDOUT_STDERR => "STDOUT_STDERR",
            _ => return f.debug_tuple("RedirectPort").field(&self.0).finish(),
        })
    }
}

impl FromStr for RedirectPort {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v = s.parse::<u16>()?;
        Ok(Self(v.into()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RedirectMode {
    Read,
    ReadOrNull,
    Write,
    WriteNoClobber,
    Append,
}

impl RedirectMode {
    #[must_use]
    pub fn default_port(self) -> RedirectPort {
        match self {
            RedirectMode::Read | RedirectMode::ReadOrNull => RedirectPort::STDIN,
            RedirectMode::Write | RedirectMode::WriteNoClobber | RedirectMode::Append => {
                RedirectPort::STDOUT
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirectDest {
    File(Word),
    Fd(Word),
}

pub type Words = Vec<Word>;

#[derive(Debug, Clone, PartialEq)]
pub enum Word {
    Simple(String),
    Complex(Vec<WordFrag>),
}

impl From<WordFrag> for Word {
    fn from(frag: WordFrag) -> Self {
        match frag {
            WordFrag::Literal(s) => Word::Simple(s),
            frag => Word::Complex(vec![frag]),
        }
    }
}

impl Word {
    fn append(self, frag: WordFrag) -> Self {
        let v = match self {
            Word::Simple(lit) => vec![WordFrag::Literal(lit), frag],
            Word::Complex(mut frags) => {
                frags.push(frag);
                frags
            }
        };
        Self::Complex(v)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WordFrag {
    Literal(String),
    Variable(String),
    VariableNoSplit(String),
    Command(Stmt),
    CommandNoSplit(Stmt),

    Brace(Vec<Word>),

    Home { slash: bool },
    Wildcard,
    WildcardRecursive,
}

pub fn parse_source(src: &str) -> Result<SourceFile, Vec<ParseError>> {
    let mut file = parse::parse_source(src).map_err(|err| vec![err])?;
    validate::validate_fixup(&mut file)?;
    Ok(file)
}
