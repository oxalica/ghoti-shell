use annotate_snippets::{Level, Renderer, Snippet};
use ghoti_shell::exec::{Error, ExecContext, Executor, Io};
use ghoti_shell::syntax::parse2::parse_source;
use rustyline::error::ReadlineError;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let exec = Executor::default();
    let mut ctx = ExecContext::new(&exec);
    let renderer = Renderer::styled();

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
