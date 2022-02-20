use std::path::PathBuf;

use ustr::{ustr, Ustr};

use crate::builtin::{MOD_FILE_NAME, SOURCE_FILE_EXT};

pub const STD: &str = "std";

pub struct IntrinsticModuleInfo {
    pub file_path: Ustr,
    pub name: Ustr,
}

pub fn std_module_root_dir() -> PathBuf {
    // TODO: this needs to come from a configurable path
    let mut dir = std::env::current_dir().unwrap();
    dir.push("lib");
    dir.push("std");
    dir
}

pub fn std_module_info() -> IntrinsticModuleInfo {
    module_info(std_module_root_dir(), STD)
}

fn module_info(
    mut root_dir: PathBuf,
    module_name: &str,
) -> IntrinsticModuleInfo {
    root_dir.push(format!("{}.{}", MOD_FILE_NAME, SOURCE_FILE_EXT));
    IntrinsticModuleInfo {
        file_path: ustr(root_dir.to_str().unwrap()),
        name: ustr(module_name),
    }
}
