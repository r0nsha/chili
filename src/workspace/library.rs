use std::{
    env,
    path::{Path, PathBuf},
};
use ustr::{ustr, Ustr};

use super::ModulePath;

pub const LIB_NAME_STD: &str = "std";

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Hash)]
pub struct Library {
    pub name: Ustr,
    pub root_file: PathBuf,
    pub is_main: bool,
}

impl Library {
    pub fn std() -> Self {
        const NAME: &str = LIB_NAME_STD;

        let mut root_dir = env::current_exe().unwrap().parent().unwrap().to_path_buf();
        root_dir.push("lib");
        root_dir.push(NAME);

        let root_file = root_dir.join(Path::new("lib.chl"));

        Self {
            name: ustr(NAME),
            root_file,
            is_main: false,
        }
    }

    pub fn as_module_path(&self) -> ModulePath {
        ModulePath::new(self.clone(), vec![ustr(self.root_file_name())])
    }

    pub fn root_module_name(&self) -> Ustr {
        ustr(&format!("{}.{}", self.name, self.root_file_name()))
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_file.parent().unwrap()
    }

    pub fn root_file_name(&self) -> &str {
        self.root_file.file_stem().unwrap().to_str().unwrap()
    }
}
