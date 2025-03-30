use std::ops::ControlFlow;

use annotate_snippets::{Level, Renderer, Snippet};
use ghoti_shell::exec::{Error, ExecContext, Executor, Io};
use ghoti_shell::syntax::parse2::parse_source;
use owo_colors::OwoColorize;
use rustyline::completion::Completer;
use rustyline::error::ReadlineError;
use rustyline::hint::Hinter;
use rustyline::history::DefaultHistory;
use rustyline::{Editor, Helper, Highlighter, Validator};

#[derive(Helper, Validator, Highlighter)]
struct ShellHelper<'a> {
    ctx: ExecContext<'a>,
}

impl Hinter for ShellHelper<'_> {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>) -> Option<Self::Hint> {
        let sp_pos = line.find(' ').unwrap_or(line.len());
        if line.is_empty() || pos > sp_pos {
            return None;
        }

        self.ctx
            .list_funcs(|name, _cmd| {
                if let Some(rest) = name.strip_prefix(line) {
                    return ControlFlow::Break(rest.bright_black().to_string());
                }
                ControlFlow::Continue(())
            })
            .break_value()
    }
}

impl Completer for ShellHelper<'_> {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let sp_pos = line.find(' ').unwrap_or(line.len());
        if line.is_empty() || pos > sp_pos {
            return Ok((0, Vec::new()));
        }

        let mut candidates = Vec::new();
        self.ctx.list_funcs::<()>(|name, _cmd| {
            if let Some(rest) = name.strip_prefix(line) {
                candidates.push(rest.to_string());
            }
            ControlFlow::Continue(())
        });

        Ok((line.len(), candidates))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let exec = Executor::default();
    let ctx = ExecContext::new(&exec);
    let renderer = Renderer::styled();

    let mut rl: Editor<ShellHelper, DefaultHistory> = rustyline::Editor::new()?;
    rl.set_helper(Some(ShellHelper { ctx }));

    let mut last_status = 0;
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    loop {
        let prompt = if last_status == 0 {
            "> ".to_owned()
        } else {
            format!("[{last_status}]> ")
        };

        let input = match rl.readline(&prompt) {
            Ok(input) => input,
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => return Err(err.into()),
        };
        let ctx = &mut rl.helper_mut().unwrap().ctx;

        let src = match parse_source(&input) {
            Ok(src) => src,
            Err(err) => {
                let msg = err.kind.to_string();
                let msg = Level::Error
                    .title(&msg)
                    .snippet(Snippet::source(&input).annotation(Level::Error.span(err.span())));
                println!("{}", renderer.render(msg));

                last_status = 127;
                continue;
            }
        };

        let ret = rt.block_on(ctx.exec_stmts(&src.stmts, Io::default()));
        // Prevent next prompt from clobbering the output if it contains no newline.
        println!();

        last_status = match ret {
            Ok(()) => 0,
            Err(Error::ExitCode(code)) => code,
            Err(err) => {
                eprintln!("{err}");
                127
            }
        };
    }
    Ok(())
}
