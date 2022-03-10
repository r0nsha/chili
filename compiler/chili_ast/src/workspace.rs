use crate::ast::{ForeignLibrary, ModuleInfo, Visibility};
use chili_span::{FileId, Span};
use chili_ty::Ty;
use codespan_reporting::files::SimpleFiles;
use common::build_options::BuildOptions;
use std::{collections::HashSet, path::Path};
use ustr::Ustr;

pub struct Workspace<'w> {
    pub build_options: BuildOptions,

    // Mapping from file id's to their source. Stored for diagnostics
    pub files: SimpleFiles<String, String>,

    // The root source file's id. Resolved during ast generation
    pub root_file_id: FileId,

    // The workspace's root directory
    pub root_dir: &'w Path,

    // Std library's root directory
    pub std_dir: &'w Path,

    // The root module's id. Resolved after ast generation
    pub root_module_id: ModuleId,

    // Parsed modules/trees info. Resolved during ast generation
    // ModuleId -> ModuleInfo
    pub module_infos: Vec<ModuleInfo>,

    // Bindings resolved during name resolution
    // BindingInfoId -> BindingInfo
    pub binding_infos: Vec<BindingInfo>,

    // Foreign libraries needed to be linked. Resolved during name resolution
    pub foreign_libraries: HashSet<ForeignLibrary>,
}

impl<'w> Workspace<'w> {
    pub fn new(
        build_options: BuildOptions,
        root_dir: &'w Path,
        std_dir: &'w Path,
    ) -> Self {
        Self {
            build_options,
            files: SimpleFiles::new(),
            root_file_id: Default::default(),
            root_dir,
            std_dir,
            module_infos: Default::default(),
            root_module_id: Default::default(),
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
    pub kind: BindingInfoKind,
    pub level: ScopeLevel,
    pub scope_name: Ustr,
    pub uses: usize,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindingInfoKind {
    Let,
    Type,
    Import,
}

impl<'w> Workspace<'w> {
    pub fn add_module_info(&mut self, module_info: ModuleInfo) -> ModuleId {
        self.module_infos.push(module_info);
        ModuleId(self.module_infos.len() - 1)
    }

    pub fn get_module_info(&self, id: ModuleId) -> Option<&ModuleInfo> {
        self.module_infos.get(id.0)
    }

    pub fn find_module_info(
        &self,
        module_info: ModuleInfo,
    ) -> Option<ModuleId> {
        self.module_infos
            .iter()
            .position(|m| *m == module_info)
            .map(|i| ModuleId(i))
    }

    pub fn add_binding_info(
        &mut self,
        module_id: ModuleId,
        symbol: Ustr,
        visibility: Visibility,
        is_mutable: bool,
        kind: BindingInfoKind,
        level: ScopeLevel,
        scope_name: Ustr,
        span: Span,
    ) -> BindingInfoId {
        self.add_typed_binding_info(
            module_id,
            symbol,
            visibility,
            Ty::Unknown,
            is_mutable,
            kind,
            level,
            scope_name,
            span,
        )
    }

    pub fn add_typed_binding_info(
        &mut self,
        module_id: ModuleId,
        symbol: Ustr,
        visibility: Visibility,
        ty: Ty,
        is_mutable: bool,
        kind: BindingInfoKind,
        level: ScopeLevel,
        scope_name: Ustr,
        span: Span,
    ) -> BindingInfoId {
        let id = BindingInfoId(self.binding_infos.len());
        self.binding_infos.push(BindingInfo {
            id,
            module_id,
            symbol,
            visibility,
            ty,
            is_mutable,
            kind,
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

impl ModuleId {
    pub fn invalid() -> Self {
        Self(usize::MAX)
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct BindingInfoId(pub usize);

impl BindingInfoId {
    pub fn invalid() -> Self {
        Self(usize::MAX)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum ScopeLevel {
    Global,
    Scope(usize),
}

impl ScopeLevel {
    pub fn is_global(&self) -> bool {
        match self {
            ScopeLevel::Global => true,
            _ => false,
        }
    }

    pub fn index(&self) -> usize {
        match self {
            ScopeLevel::Global => 0,
            ScopeLevel::Scope(i) => *i,
        }
    }

    pub fn next(&self) -> ScopeLevel {
        match self {
            ScopeLevel::Global => ScopeLevel::Scope(1),
            ScopeLevel::Scope(i) => ScopeLevel::Scope(*i + 1),
        }
    }

    pub fn previous(&self) -> ScopeLevel {
        match self {
            ScopeLevel::Global => {
                panic!("tried to go to previous scope from global scope")
            }
            ScopeLevel::Scope(i) => {
                if *i == 1 {
                    ScopeLevel::Global
                } else {
                    ScopeLevel::Scope(*i - 1)
                }
            }
        }
    }
}
