use ghoti_exec::{ExecContext, Executor};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let exec = Executor::default();
    let mut ctx = ExecContext::new(&exec);
    ghoti_shell::repl::run_repl(&mut ctx)
}
