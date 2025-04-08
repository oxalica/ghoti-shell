use std::ops::ControlFlow;

use ghoti_exec::{ExecContext, Status};
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
            .list_funcs_without_autoload(|name, _cmd| {
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
        self.ctx.list_funcs_without_autoload::<()>(|name, _cmd| {
            if let Some(rest) = name.strip_prefix(line) {
                candidates.push(rest.to_string());
            }
            ControlFlow::Continue(())
        });

        Ok((line.len(), candidates))
    }
}

pub fn run_repl(
    rt: &tokio::runtime::Runtime,
    ctx: &mut ExecContext<'_>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut rl: Editor<ShellHelper, DefaultHistory> = rustyline::Editor::new()?;
    rl.set_helper(Some(ShellHelper { ctx }));

    loop {
        let prompt = {
            let ctx = &mut *rl.helper_mut().unwrap().ctx;
            // For completion
            rt.block_on(ctx.populate_autoload_func_cache());

            let st = ctx.last_status();
            if st.is_success() {
                "ghoti-shell> ".to_owned()
            } else {
                format!("ghoti-shell[{st}]> ")
            }
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

        rt.block_on(ctx.exec_source(None, input));
        // Prevent next prompt from clobbering the output if it contains no newline.
        println!();
    }
}
