use crate::workspace::ModuleInfo;
use common::builtin::MOD_FILE_NAME;
use std::path::{Path, PathBuf};
use ustr::ustr;

pub const STD: &str = "std";
pub const STD_PREFIX_FW: &str = "std/";
pub const STD_PREFIX_BK: &str = "std\\";
pub const SOURCE_FILE_EXT: &str = "chili";

pub fn is_std_module_path(path: &str) -> bool {
    path == STD
}

pub fn is_std_module_path_start(path: &str) -> bool {
    path.starts_with(STD_PREFIX_FW) || path.starts_with(STD_PREFIX_BK)
}

pub fn std_module_root_dir() -> PathBuf {
    // TODO: maybe this needs to come from a configurable path?
    let mut dir = root_dir();
    dir.push("lib");
    dir.push(STD);
    dir
}

pub fn std_module_root_file() -> PathBuf {
    std_module_root_dir().join(Path::new("std.chili"))
}

pub fn std_module_info() -> ModuleInfo {
    module_info(std_module_root_dir(), STD)
}

fn module_info(mut root_dir: PathBuf, module_name: &str) -> ModuleInfo {
    root_dir.push(format!("{}.{}", MOD_FILE_NAME, SOURCE_FILE_EXT));
    ModuleInfo {
        file_path: ustr(root_dir.to_str().unwrap()),
        name: ustr(module_name),
    }
}

fn root_dir() -> PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}
