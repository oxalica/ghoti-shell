use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

pub mod parse;

pub const KEYWORDS: &[&str] = &[
    "begin", "end", "if", "else", "switch", "case", "for", "in", "and", "or", "not", "function",
];

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Command(Words),
    Block(Vec<Stmt>),
    If(Box<Stmt>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Stmt>, Box<Stmt>),
    For(Word, Words, Box<Stmt>),
    Function(Words, Box<Stmt>),

    Redirect(Box<Stmt>, Vec<Redirect>),
    Pipe(RedirectPort, Box<Stmt>, Box<Stmt>),

    Not(Box<Stmt>),
    And(Box<Stmt>, Box<Stmt>),
    Or(Box<Stmt>, Box<Stmt>),
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

    TildeSegment,
    Wildcard,
    WildcardRecursive,
}
