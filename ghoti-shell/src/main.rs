use ghoti_exec::{ExecContext, Executor, Status};

fn main() -> Result<Status, Box<dyn std::error::Error>> {
    let exec = Executor::default_from_env();
    let mut ctx = ExecContext::new(&exec);
    ghoti_shell::repl::run_repl(&mut ctx)?;
    Ok(ctx.last_status())
}
