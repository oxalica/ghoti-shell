use std::ops::ControlFlow;

use annotate_snippets::{Level, Renderer, Snippet};
use ghoti_exec::{ExecContext, Status};
use ghoti_syntax::parse_source;
use owo_colors::OwoColorize;
use rustyline::completion::Completer;
use rustyline::error::ReadlineError;
use rustyline::hint::Hinter;
use rustyline::history::DefaultHistory;
use rustyline::{Editor, Helper, Highlighter, Validator};

#[derive(Helper, Validator, Highlighter)]
struct ShellHelper<'a, 'ctx> {
    ctx: &'a mut ExecContext<'ctx>,
}

impl Hinter for ShellHelper<'_, '_> {
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

impl Completer for ShellHelper<'_, '_> {
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

pub fn run_repl(ctx: &mut ExecContext<'_>) -> Result<(), Box<dyn std::error::Error>> {
    let renderer = Renderer::styled();

    let mut rl: Editor<ShellHelper, DefaultHistory> = rustyline::Editor::new()?;
    rl.set_helper(Some(ShellHelper { ctx }));

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    loop {
        let last_status = rl.helper().unwrap().ctx.last_status();
        let prompt = if last_status.is_success() {
            "> ".to_owned()
        } else {
            format!("[{last_status}]> ")
        };

        let input = match rl.readline(&prompt) {
            Ok(input) => input,
            Err(ReadlineError::Eof) => {
                ctx.set_last_status(Status::SUCCESS);
                return Ok(());
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => return Err(err.into()),
        };
        let ctx = &mut *rl.helper_mut().unwrap().ctx;

        let src = match parse_source(&input) {
            Ok(src) => src,
            Err(errs) => {
                for err in errs {
                    let msg = err.kind.to_string();
                    let msg = Level::Error
                        .title(&msg)
                        .snippet(Snippet::source(&input).annotation(Level::Error.span(err.span())));
                    println!("{}", renderer.render(msg));
                }

                ctx.set_last_status(Status(127));
                continue;
            }
        };

        rt.block_on(ctx.exec_source(&src));
        // Prevent next prompt from clobbering the output if it contains no newline.
        println!();
    }
}
