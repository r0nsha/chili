use crate::ast::{Ast, ForeignLibrary, Visibility};
use chili_span::{FileId, Span};
use chili_ty::Ty;
use codespan_reporting::files::SimpleFiles;
use std::{collections::HashSet, path::Path};
use ustr::Ustr;

pub struct Workspace<'w> {
    // Mapping from file id's to their source. Stored for diagnostics
    pub files: SimpleFiles<String, String>,

    // The root source file's id. Resolved during ast generation
    pub root_file_id: FileId,

    // The workspace's root directory
    pub root_dir: &'w Path,

    // Std library's root directory
    pub std_dir: &'w Path,

    // Parsed modules/trees, aka Ast's. Resolved during ast generation
    // ModuleId -> Ast
    pub modules: Vec<Ast>,

    // The root module's id. Resolved after ast generation
    pub root_module: ModuleId,

    // Bindings resolved during name resolution
    // BindingInfoId -> BindingInfo
    pub binding_infos: Vec<BindingInfo>,

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
            modules: Default::default(),
            root_module: Default::default(),
            binding_infos: Default::default(),
            foreign_libraries: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BindingInfo {
    pub id: BindingInfoId,
    pub module_id: ModuleId,
    pub symbol: Ustr,
    pub visibility: Visibility,
    pub ty: Ty,
    pub is_mutable: bool,
    pub level: BindingLevel,
    pub scope_name: Ustr,
    pub uses: usize,
    pub span: Span,
}

impl<'w> Workspace<'w> {
    pub fn add_module(&mut self, ast: Ast) -> ModuleId {
        self.modules.push(ast);
        ModuleId(self.modules.len() - 1)
    }

    pub fn get_module(&self, id: ModuleId) -> Option<&Ast> {
        self.modules.get(id.0)
    }

    pub fn next_module_id(&self) -> ModuleId {
        ModuleId(self.modules.len())
    }

    pub fn add_binding_info(
        &mut self,
        module_id: ModuleId,
        symbol: Ustr,
        visibility: Visibility,
        is_mutable: bool,
        level: BindingLevel,
        scope_name: Ustr,
        span: Span,
    ) -> BindingInfoId {
        let id = BindingInfoId(self.binding_infos.len());
        self.binding_infos.push(BindingInfo {
            id,
            module_id,
            symbol,
            visibility,
            ty: Ty::Unknown,
            is_mutable,
            level,
            scope_name,
            uses: 0,
            span,
        });
        id
    }

    pub fn get_binding_info(&self, id: BindingInfoId) -> Option<&BindingInfo> {
        self.binding_infos.get(id.0)
    }

    pub fn next_binding_info_id(&self) -> BindingInfoId {
        BindingInfoId(self.binding_infos.len())
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ModuleId(pub usize);

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct BindingInfoId(pub usize);

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct BindingLevel(pub usize);

impl BindingLevel {
    pub fn is_global(&self) -> bool {
        self.0 == 1
    }
}
