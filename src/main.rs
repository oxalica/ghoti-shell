use chumsky::Parser;
use ghoti_shell::exec::{Error, Executor, Io};
use ghoti_shell::syntax::parse::source_file;
use rustyline::error::ReadlineError;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let exec = Executor::new();

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

        let src = match source_file().parse(&input).into_result() {
            Ok(src) => src,
            Err(errs) => {
                for err in errs {
                    println!("no parse: {err}");
                }
                last_status = 127;
                continue;
            }
        };

        let ret = rt.block_on(exec.exec_stmts(&src.stmts, Io::default()));
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
