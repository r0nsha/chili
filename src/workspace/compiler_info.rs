use std::{
    env,
    path::{Path, PathBuf},
};

pub const STD: &str = "std";
pub const SOURCE_FILE_EXT: &str = "chl";

pub fn std_module_root_dir() -> PathBuf {
    let mut dir = env::current_exe().unwrap().parent().unwrap().to_path_buf();
    dir.push("lib");
    dir.push(STD);
    dir
}

pub fn std_module_root_file() -> PathBuf {
    std_module_root_dir().join(Path::new("std.chl"))
}
