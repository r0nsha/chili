use crate::workspace::ModuleInfo;
use std::path::{Path, PathBuf};
use ustr::{ustr, Ustr};

pub const STD: &str = "std";
pub const STD_PREFIX_FW: &str = "std/";
pub const STD_PREFIX_BK: &str = "std\\";
pub const SOURCE_FILE_EXT: &str = "chili";

pub fn root_module_name() -> Ustr {
    ustr("")
}

pub fn is_std_module_path(path: &str) -> bool {
    path == STD
}

pub fn is_std_module_path_start(path: &str) -> bool {
    path.starts_with(STD_PREFIX_FW) || path.starts_with(STD_PREFIX_BK)
}

pub fn std_module_root_dir() -> PathBuf {
    fn root_dir() -> PathBuf {
        std::env::current_exe()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf()
    }

    let mut dir = root_dir();
    dir.push("lib");
    dir.push(STD);
    dir
}

pub fn std_module_root_file() -> PathBuf {
    std_module_root_dir().join(Path::new("std.chili"))
}

pub fn std_module_info() -> ModuleInfo {
    ModuleInfo {
        file_path: ustr(std_module_root_file().to_str().unwrap()),
        name: ustr(STD),
    }
}
