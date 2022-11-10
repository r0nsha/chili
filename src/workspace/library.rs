use super::{LibraryId, ModuleId, ModulePath};
use crate::common::id_cache::WithId;
use std::{
    env,
    path::{Path, PathBuf},
};
use ustr::{ustr, Ustr};

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Hash)]
pub struct Library {
    pub id: LibraryId, // Populated when inserted to the workspace's libraries cache
    pub name: Ustr,
    pub root_file: PathBuf,
    pub root_module_id: ModuleId, // Populated after parsing finishes
    pub is_main: bool,
}

impl WithId<LibraryId> for Library {
    fn id(&self) -> &LibraryId {
        &self.id
    }

    fn id_mut(&mut self) -> &mut LibraryId {
        &mut self.id
    }
}

impl Library {
    pub fn std() -> Self {
        let mut root_dir = env::current_exe().unwrap().parent().unwrap().to_path_buf();
        root_dir.push("stdlib");

        let root_file = root_dir.join(Path::new("lib.chl"));

        Self {
            id: LibraryId::unknown(),
            name: ustr("std"),
            root_file,
            root_module_id: ModuleId::unknown(),
            is_main: false,
        }
    }

    pub fn as_module_path(&self) -> ModulePath {
        ModulePath::new(self.clone(), vec![ustr(self.root_file_stem())])
    }

    pub fn root_dir(&self) -> &Path {
        self.root_file.parent().unwrap()
    }

    pub fn root_file_stem(&self) -> &str {
        self.root_file.file_stem().unwrap().to_str().unwrap()
    }
}
