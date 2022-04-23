pub mod build;
pub mod cli;

use cli::start_cli;

fn main() {
    // Note (Ron): This is a workaround for a stack overflow that we're getting during the `Check` phase.
    // Note (Ron): Are we allocating too much in the stack frame while recursing?
    std::thread::Builder::new()
        .name("main".to_string())
        .stack_size(8 * 1024 * 1024) // 8 MB
        .spawn(|| start_cli())
        .unwrap()
        .join()
        .unwrap();
}
