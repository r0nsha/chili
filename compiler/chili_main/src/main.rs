pub mod build;
pub mod cli;

use cli::start_cli;

fn main() {
    if cfg!(windows) {
        extern "system" {
            pub fn SetConsoleOutputCP(wCodePageID: u32) -> u32;
        }
        const CP_UTF8: u32 = 65001;
        unsafe { SetConsoleOutputCP(CP_UTF8) };
    }

    // Note (Ron): this is a (hopefully temporary) workaround to prevent stack
    // overflows, especially during the check phase. I haven't found any
    // other solution for this.
    std::thread::Builder::new()
        .name(String::from("main"))
        .stack_size(32 * 1024 * 1024)
        .spawn(|| start_cli())
        .unwrap()
        .join()
        .unwrap();
}
