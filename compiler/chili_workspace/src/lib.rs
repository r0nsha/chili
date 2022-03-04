use chili_ast::ast::{Binding, ForeignLibrary, ModuleInfo};
use chili_ty::Ty;
use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

struct Workspace<'w> {
    root_directory: &'w Path,
    std_directory: &'w Path,
    module_infos: HashMap<ModuleId, ModuleInfo>,
    bindings: HashMap<BindingId, BindingDef<'w>>,
    foreign_libraries: HashSet<ForeignLibrary>,
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
