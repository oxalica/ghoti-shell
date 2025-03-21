use std::collections::HashMap;
use std::fmt;
use std::fs::OpenOptions;
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::pin::Pin;
use std::rc::Rc;

use tokio::process::Command;

use crate::syntax::{RedirectDest, RedirectMode, RedirectPort, Stmt, Word};

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type Bytes = Vec<u8>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("command exited with {0}")]
    ExitCode(i32),

    #[error("empty command")]
    EmptyCommand,
    #[error("command not found: {0:?}")]
    CommandNotFound(String),
    #[error("invalid redirection target")]
    InvalidRedirectionTarget,
    #[error("failed open redirection file {0:?}: {1}")]
    OpenRedirectionFile(PathBuf, std::io::Error),
    #[error("failed to spawn process {0:?}: {1}")]
    SpawnProcess(PathBuf, std::io::Error),

    #[error("failed to create pipe: {0}")]
    CreatePipe(std::io::Error),
    #[error("failed to clone fd: {0}")]
    CloneHandle(std::io::Error),
    #[error("failed to wait process: {0}")]
    WaitProcess(std::io::Error),
}

pub type BuiltinCmd = Box<dyn Fn(&[String], Io) -> Pin<Box<dyn Future<Output = ExecResult>>>>;

#[derive(Default)]
pub struct Executor {
    builtins: HashMap<String, BuiltinCmd>,
}

impl fmt::Debug for Executor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Executor")
            .field("builtins", &self.builtins.keys())
            .finish()
    }
}

type ExecResult = Result<()>;

#[derive(Default, Debug, Clone)]
pub struct Io {
    pub stdin: Stdio,
    pub stdout: Stdio,
    pub stderr: Stdio,
}

#[derive(Default, Debug, Clone)]
pub enum Stdio {
    #[default]
    Inherit,
    Close,
    Raw(Rc<OwnedFd>),
}

impl Executor {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_builtin(&mut self, name: impl Into<String>, cmd: BuiltinCmd) -> Option<BuiltinCmd> {
        self.builtins.insert(name.into(), cmd)
    }

    pub fn get_builtin(&self, name: &str) -> Option<&BuiltinCmd> {
        self.builtins.get(name)
    }

    fn exec_stmt_boxed(&self, stmt: &Stmt, io: Io) -> impl Future<Output = ExecResult> {
        Box::pin(self.exec_stmt(stmt, io))
    }

    pub async fn exec_stmts(&self, stmts: &[Stmt], io: Io) -> ExecResult {
        let Some((last, init)) = stmts.split_last() else {
            return Ok(());
        };
        for s in init {
            let _: Result<_> = self.exec_stmt_boxed(s, io.clone()).await;
        }
        self.exec_stmt(last, io).await
    }

    pub async fn exec_stmt(&self, stmt: &Stmt, mut io: Io) -> ExecResult {
        match stmt {
            Stmt::Command(words) => {
                let words = self.expand_words(words).await?;
                self.exec_cmd(&words, io).await
            }
            Stmt::Block(stmts) => Box::pin(self.exec_stmts(stmts, io)).await,
            Stmt::If(cond, then, else_) => {
                let cond = self.exec_stmt_boxed(cond, io.clone()).await.is_ok();
                if cond {
                    self.exec_stmt_boxed(then, io).await
                } else if let Some(else_) = else_ {
                    self.exec_stmt_boxed(else_, io).await
                } else {
                    Ok(())
                }
            }
            Stmt::While(cond, body) => loop {
                self.exec_stmt_boxed(cond, io.clone()).await?;
                let _ret = self.exec_stmt_boxed(body, io.clone()).await;
            },
            Stmt::For(..) => todo!(),
            Stmt::Function(..) => todo!(),
            Stmt::Redirect(stmt, redirects) => {
                for redir in redirects {
                    let (RedirectDest::File(file_word) | RedirectDest::Fd(file_word)) = &redir.dest;
                    let expanded = self.expand_words(std::slice::from_ref(file_word)).await?;
                    let [file_path] = &*expanded else {
                        return Err(Error::InvalidRedirectionTarget);
                    };
                    match redir.dest {
                        RedirectDest::File(_) => {}
                        RedirectDest::Fd(_) => todo!(),
                    }
                    let mut opt = OpenOptions::new();
                    match redir.mode {
                        RedirectMode::Read | RedirectMode::ReadOrNull => opt.read(true),
                        RedirectMode::Write => opt.write(true).create(true),
                        RedirectMode::WriteNoClobber => opt.write(true).create_new(true),
                        RedirectMode::Append => opt.append(true).create(true),
                    };
                    let f = opt
                        .open(file_path)
                        .map_err(|err| Error::OpenRedirectionFile(file_path.into(), err))?;
                    let f = Stdio::Raw(Rc::new(OwnedFd::from(f)));

                    match redir.port {
                        RedirectPort::STDIN => io.stdin = f,
                        RedirectPort::STDOUT => io.stdout = f,
                        RedirectPort::STDOUT_STDERR => (io.stdout, io.stderr) = (f.clone(), f),
                        _ => todo!(),
                    }
                }

                self.exec_stmt_boxed(stmt, io).await
            }
            Stmt::Pipe(port, lhs, rhs) => {
                let (pipe_r, pipe_w) = os_pipe::pipe().map_err(Error::CreatePipe)?;
                let pipe_r = Stdio::Raw(Rc::new(pipe_r.into()));
                let pipe_w = Stdio::Raw(Rc::new(pipe_w.into()));

                let mut lhs_io = io.clone();
                match *port {
                    RedirectPort::STDOUT => lhs_io.stdout = pipe_w,
                    RedirectPort::STDERR => lhs_io.stderr = pipe_w,
                    RedirectPort::STDOUT_STDERR => {
                        (lhs_io.stdout, lhs_io.stderr) = (pipe_w.clone(), pipe_w)
                    }
                    _ => todo!(),
                }
                let rhs_io = Io {
                    stdin: pipe_r,
                    ..io.clone()
                };

                let (_ret_l, ret_r) = tokio::join!(
                    self.exec_stmt_boxed(lhs, lhs_io),
                    self.exec_stmt_boxed(rhs, rhs_io),
                );
                ret_r
            }
            Stmt::Not(stmt) => match self.exec_stmt_boxed(stmt, io).await {
                Ok(()) => Err(Error::ExitCode(0)),
                Err(_) => Ok(()),
            },
            Stmt::And(lhs, rhs) => {
                self.exec_stmt_boxed(lhs, io.clone()).await?;
                self.exec_stmt_boxed(rhs, io).await
            }
            Stmt::Or(lhs, rhs) => match self.exec_stmt_boxed(lhs, io.clone()).await {
                Ok(()) => Ok(()),
                Err(_) => self.exec_stmt_boxed(rhs, io).await,
            },
        }
    }

    pub async fn exec_cmd(&self, words: &[String], io: Io) -> ExecResult {
        let cmd = words.first().ok_or(Error::EmptyCommand)?;

        if let Some(builtin) = self.builtins.get(cmd) {
            return builtin(words, io).await;
        }

        self.exec_external_cmd(words, io).await
    }

    pub async fn exec_external_cmd(&self, words: &[String], io: Io) -> ExecResult {
        use std::process::Stdio as ProcessStdio;

        let (cmd, args) = words.split_first().ok_or(Error::EmptyCommand)?;

        let cvt_stdio = |s: Stdio| {
            Ok(match s {
                Stdio::Inherit => ProcessStdio::inherit(),
                Stdio::Close => todo!(),
                Stdio::Raw(raw) => Rc::try_unwrap(raw)
                    .or_else(|raw| raw.try_clone().map_err(Error::CloneHandle))?
                    .into(),
            })
        };

        let mut child = Command::new(cmd)
            .args(args)
            .stdin(cvt_stdio(io.stdin)?)
            .stdout(cvt_stdio(io.stdout)?)
            .stderr(cvt_stdio(io.stderr)?)
            .spawn()
            .map_err(|err| Error::SpawnProcess(cmd.into(), err))?;
        let status = child.wait().await.map_err(Error::WaitProcess)?;

        if status.success() {
            Ok(())
        } else {
            #[cfg(unix)]
            let code = {
                use std::os::unix::process::ExitStatusExt;
                status.into_raw()
            };

            Err(Error::ExitCode(code))
        }
    }

    async fn expand_words(&self, words: &[Word]) -> Result<Vec<String>> {
        let mut ret = Vec::new();

        for w in words {
            match w {
                Word::Simple(w) => ret.push(w.clone()),
                Word::Complex(_) => todo!(),
            }
        }

        Ok(ret)
    }
}
