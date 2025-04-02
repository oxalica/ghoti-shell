use std::cell::RefCell;
use std::fmt;
use std::io::{self as stdio, Read as _, Write};
use std::os::fd::{AsFd, OwnedFd};
use std::pin::Pin;
use std::process::Stdio;
use std::rc::Rc;
use std::sync::Arc;
use std::task::{Context, Poll};

use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, ReadBuf};
use tokio::net::unix::pipe;

use crate::{Error, ExecResult, Status};

#[derive(Debug, Clone)]
pub struct IoConfig {
    pub stdin: Io,
    pub stdout: Io,
    pub stderr: Io,
}

impl Default for IoConfig {
    fn default() -> Self {
        Self {
            stdin: Io::Stdin,
            stdout: Io::Stdout,
            stderr: Io::Stderr,
        }
    }
}

pub(crate) type StdioCollectSink = Rc<dyn Fn(&[u8]) -> ExecResult>;

#[expect(private_interfaces, reason = "TODO")]
#[derive(Clone)]
pub enum Io {
    Stdin,
    Stdout,
    Stderr,
    Close,
    Collect(StdioCollectSink),

    File(Arc<std::fs::File>),
    PipeSender(Rc<AsyncCell<pipe::Sender>>),
    PipeReceiver(Rc<AsyncCell<pipe::Receiver>>),
}

impl fmt::Debug for Io {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stdin => write!(f, "Stdin"),
            Self::Stdout => write!(f, "Stdout"),
            Self::Stderr => write!(f, "Stderr"),
            Self::Close => write!(f, "Close"),
            Self::Collect(_) => f.debug_tuple("Collect").finish_non_exhaustive(),

            Self::File(_) => f.debug_tuple("File").finish_non_exhaustive(),
            Self::PipeSender(_) => f.debug_tuple("PipeSender").finish_non_exhaustive(),
            Self::PipeReceiver(_) => f.debug_tuple("PipeReceiver").finish_non_exhaustive(),
        }
    }
}

impl Io {
    /// Returns: (tx, rx)
    pub fn new_os_pipe() -> Result<(Io, Io), Error> {
        let (r, w) = os_pipe::pipe().map_err(Error::CreatePipe)?;
        let tx = Io::PipeSender(Rc::new(AsyncCell {
            blocking: w.into(),
            non_blocking: RefCell::new(None),
        }));
        let rx = Io::PipeReceiver(Rc::new(AsyncCell {
            blocking: r.into(),
            non_blocking: RefCell::new(None),
        }));
        Ok((tx, rx))
    }

    pub fn to_stdio(&self) -> Result<(Stdio, Option<StdioCollectSink>), Error> {
        let stdio = match self {
            Io::Stdin => stdio::stdin()
                .as_fd()
                .try_clone_to_owned()
                .map_err(Error::CloneHandle)?
                .into(),
            Io::Stdout => stdio::stdout().into(),
            Io::Stderr => stdio::stderr().into(),
            Io::Close => Stdio::null(),
            Io::Collect(f) => return Ok((Stdio::piped(), Some(Rc::clone(f)))),
            Io::File(f) => (**f).try_clone().map_err(Error::CloneHandle)?.into(),
            Io::PipeSender(tx) => tx.blocking.try_clone().map_err(Error::CloneHandle)?.into(),
            Io::PipeReceiver(rx) => rx.blocking.try_clone().map_err(Error::CloneHandle)?.into(),
        };
        Ok((stdio, None))
    }

    pub async fn write_all(&self, bytes: impl AsRef<[u8]> + Send + 'static) -> ExecResult {
        match self {
            Self::Close | Self::PipeReceiver(_) => Err(Error::PipeClosed),
            Self::Collect(sink) => sink(bytes.as_ref()),
            // FIXME
            Self::Stdin => Err(Error::PipeClosed),
            Self::File(f) => {
                tokio::task::spawn_blocking({
                    let f = Arc::clone(f);
                    // `std::fs::File` does not need flush.
                    move || (&*f).write_all(bytes.as_ref())
                })
                .await
                .expect("no panic")
                .map_err(Error::ReadWrite)?;
                Ok(Status::SUCCESS)
            }
            // FIXME: Should bypass std's lock to avoid deadlocks when using print* macros.
            Self::Stdout | Self::Stderr => {
                let is_stdout = matches!(self, Self::Stdout);
                tokio::task::spawn_blocking(move || {
                    let lock = if is_stdout {
                        &mut stdio::stdout().lock() as &mut dyn stdio::Write
                    } else {
                        &mut stdio::stderr().lock()
                    };
                    lock.write_all(bytes.as_ref())?;
                    lock.flush()
                })
                .await
                .expect("no panic")
                .map_err(Error::ReadWrite)?;
                Ok(Status::SUCCESS)
            }
            Self::PipeSender(tx) => {
                (&**tx)
                    .write_all(bytes.as_ref())
                    .await
                    .map_err(Error::ReadWrite)?;
                Ok(Status::SUCCESS)
            }
        }
    }

    pub async fn read_to_string(&self) -> ExecResult<String> {
        self.read_to_end()
            .await
            .and_then(|buf| String::from_utf8(buf).map_err(|_| Error::InvalidUtf8))
    }

    pub async fn read_to_end(&self) -> ExecResult<Vec<u8>> {
        match self {
            Self::Stdin => tokio::task::spawn_blocking(move || {
                let mut buf = Vec::new();
                stdio::stdin().lock().read_to_end(&mut buf)?;
                Ok(buf)
            })
            .await
            .expect("no panic")
            .map_err(Error::ReadWrite),
            Self::File(f) => tokio::task::spawn_blocking({
                let f = Arc::clone(f);
                move || {
                    let mut buf = Vec::new();
                    (&*f).read_to_end(&mut buf)?;
                    Ok(buf)
                }
            })
            .await
            .expect("no panic")
            .map_err(Error::ReadWrite),
            Self::PipeReceiver(rx) => {
                let mut buf = Vec::new();
                (&**rx)
                    .read_to_end(&mut buf)
                    .await
                    .map_err(Error::ReadWrite)?;
                Ok(buf)
            }
            Self::Stdout | Self::Stderr | Self::Close | Self::Collect(_) | Self::PipeSender(_) => {
                Err(Error::PipeClosed)
            }
        }
    }
}

pub(crate) struct AsyncCell<T> {
    blocking: OwnedFd,
    non_blocking: RefCell<Option<T>>,
}

impl AsyncRead for &AsyncCell<pipe::Receiver> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<stdio::Result<()>> {
        let mut rx = self.non_blocking.borrow_mut();
        let rx = match &mut *rx {
            Some(rx) => rx,
            None => rx.insert(pipe::Receiver::from_owned_fd_unchecked(
                self.blocking.try_clone()?,
            )?),
        };
        Pin::new(rx).poll_read(cx, buf)
    }
}

impl AsyncWrite for &AsyncCell<pipe::Sender> {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, stdio::Error>> {
        let mut tx = self.non_blocking.borrow_mut();
        let tx = match &mut *tx {
            Some(tx) => tx,
            None => tx.insert(pipe::Sender::from_owned_fd_unchecked(
                self.blocking.try_clone()?,
            )?),
        };
        Pin::new(tx).poll_write(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<(), stdio::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
    ) -> Poll<Result<(), stdio::Error>> {
        Poll::Ready(Ok(()))
    }
}
