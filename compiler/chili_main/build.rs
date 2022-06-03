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

    let options = dir::CopyOptions {
        overwrite: true,
        skip_exist: false,
        buffer_size: 64000,
        copy_inside: false,
        content_only: false,
        depth: 0,
    };

    dir::copy(lib_dir, target_dir, &options).unwrap();
    println!("cargo:rerun-if-changed=../../lib");
}
