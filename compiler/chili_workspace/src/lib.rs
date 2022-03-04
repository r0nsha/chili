use chili_ast::ast::{Ast, Binding, ForeignLibrary, ModuleInfo};
use chili_ty::Ty;
use codespan_reporting::files::SimpleFiles;
use common::compiler_info;
use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

pub struct Workspace<'w> {
    files: SimpleFiles<String, String>,
    root_dir: &'w Path,
    std_dir: &'w Path,
    module_infos: HashMap<ModuleId, ModuleInfo>,
    parsed_trees: HashMap<ModuleId, Ast>,
    bindings: HashMap<BindingId, BindingDef<'w>>,
    foreign_libraries: HashSet<ForeignLibrary>,
}

impl<'w> Workspace<'w> {
    pub fn new(root_dir: &'w Path, std_dir: &'w Path) -> Self {
        Self {
            files: SimpleFiles::new(),
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
