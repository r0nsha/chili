use std::{
    env,
    path::{Path, PathBuf},
};
use ustr::{ustr, Ustr};

pub const LIB_NAME_STD: &str = "std";

#[derive(Debug, Clone)]
pub struct Library {
    pub name: Ustr,
    pub root_dir: PathBuf,
    pub root_file: PathBuf,
}

impl Library {
    pub fn std() -> Self {
        let mut root_dir = env::current_exe().unwrap().parent().unwrap().to_path_buf();
        root_dir.push("lib");
        root_dir.push(LIB_NAME_STD);

        let root_file = root_dir.join(Path::new("std.chl"));

        Self {
            name: ustr(LIB_NAME_STD),
            root_dir,
            root_file,
        }
    }
}
