use crate::workspace::ModuleInfo;
use common::builtin::{MOD_FILE_NAME, SOURCE_FILE_EXT};
use std::path::PathBuf;
use ustr::ustr;

pub const STD: &str = "std";

pub fn std_module_root_dir() -> PathBuf {
    // TODO: this needs to come from a configurable path
    let mut dir = std::env::current_dir().unwrap();
    dir.push("lib");
    dir.push(STD);
    dir
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
