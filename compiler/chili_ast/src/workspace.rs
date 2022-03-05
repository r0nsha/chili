use crate::ast::{Ast, Binding, ForeignLibrary, ModuleInfo};
use chili_span::FileId;
use chili_ty::Ty;
use codespan_reporting::files::SimpleFiles;
use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

pub struct Workspace<'w> {
    // Mapping from file id's to their source. Stored for diagnostics
    pub files: SimpleFiles<String, String>,

    // The root source file's id. Resolved during ast generation
    pub root_file_id: FileId,

    // The workspace's root directory
    pub root_dir: &'w Path,

    // Std library's root directory
    pub std_dir: &'w Path,

    pub module_infos: HashMap<ModuleId, ModuleInfo>,
    pub parsed_trees: HashMap<ModuleId, Ast>,

    pub bindings: HashMap<BindingId, BindingDef<'w>>,

    // Foreign libraries needed to be linked. Resolved during name resolution
    pub foreign_libraries: HashSet<ForeignLibrary>,
}

impl<'w> Workspace<'w> {
    pub fn new(root_dir: &'w Path, std_dir: &'w Path) -> Self {
        Self {
            files: SimpleFiles::new(),
            root_file_id: Default::default(),
            root_dir,
            std_dir,
            module_infos: Default::default(),
            parsed_trees: Default::default(),
            bindings: Default::default(),
            foreign_libraries: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ModuleId(usize);

#[derive(Debug, PartialEq, Clone)]
pub struct BindingDef<'w> {
    binding: Binding,
    scope_level: ScopeLevel,
    scope_name: &'w str,
    ty: Ty,
    uses: usize,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct BindingId(usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ScopeLevel(usize);

impl<'w> Workspace<'w> {}
