use clap::Parser;
use ghoti_exec::{ExecContext, Executor, Status, VarScope};

#[derive(Debug, Parser)]
pub struct Args {
    /// The inline script string to execute.
    #[arg(long, short)]
    command: Option<String>,

    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

fn main() -> Result<Status, Box<dyn std::error::Error>> {
    let args = Args::parse();

    let exec = Executor::default_from_env();
    let mut ctx = ExecContext::new(&exec);

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    if let Some(cmd) = args.command {
        rt.block_on(ctx.exec_source(Some("<commandline>".into()), cmd));
    } else if let Some((file, script_args)) = args.args.split_first() {
        let text = std::fs::read_to_string(file)?;
        ctx.set_var("argv", VarScope::Local, script_args.to_vec());
        rt.block_on(ctx.exec_source(Some(file.clone()), text));
    } else {
        ghoti_shell::repl::run_repl(&rt, &mut ctx)?;
    }
    Ok(ctx.last_status())
}
