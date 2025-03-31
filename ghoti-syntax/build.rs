const GRAMMER_FILE: &str = "src/grammar.lalrpop";

fn main() {
    println!("cargo::rerun-if-changed={GRAMMER_FILE}");
    lalrpop::Configuration::new()
        .always_use_colors()
        .use_cargo_dir_conventions()
        .process_file(GRAMMER_FILE)
        .unwrap();
}
