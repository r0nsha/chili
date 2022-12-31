use fs_extra::dir;

fn main() {
    #[cfg(windows)]
    {
        println!("cargo:rustc-link-search=./llvm/windows");
        println!("cargo:rustc-link-lib=dylib=LLVM-C");
    }

    #[cfg(not(windows))]
    {
        println!("cargo:rustc-link-lib=dylib=LLVM-12");
    }

    const STDLIB: &str = "stdlib";

    let workspace_dir = std::env::current_dir().unwrap();

    let stdlib_dir = workspace_dir.join(STDLIB);

    #[cfg(debug_assertions)]
    let target_dir = workspace_dir.join("target/debug");

    #[cfg(not(debug_assertions))]
    let target_dir = workspace_dir.join("target/release");

    if target_dir.exists() {
        let options = dir::CopyOptions {
            overwrite: true,
            skip_exist: false,
            buffer_size: 64000,
            copy_inside: false,
            content_only: false,
            depth: 0,
        };

        dir::copy(stdlib_dir, target_dir, &options).unwrap();
    }

    println!("cargo:rerun-if-changed=./{}", STDLIB);
}
