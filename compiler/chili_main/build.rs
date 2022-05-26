use fs_extra::dir;

fn main() {
    let workspace_dir = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();

    let lib_dir = workspace_dir.with_file_name("lib");

    #[cfg(debug_assertions)]
    let target_dir = workspace_dir.with_file_name("target/debug");

    #[cfg(not(debug_assertions))]
    let target_dir = workspace_dir.with_file_name("target/release");

    dir::copy(lib_dir, target_dir, &Default::default()).unwrap();
    println!("cargo:rerun-if-changed=../../lib");
}
