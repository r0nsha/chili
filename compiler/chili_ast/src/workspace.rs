use crate::ast::{ForeignLibrary, ModuleInfo, Visibility};
use crate::ty::TyKind;
use crate::value::Value;
use bitflags::bitflags;
use chili_span::{FileId, Span};
use codespan_reporting::files::SimpleFiles;
use common::build_options::BuildOptions;
use std::{cmp::Ordering, collections::HashSet, path::Path};
use ustr::{ustr, Ustr};

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
    pub root_module_id: ModuleIdx,

    // Parsed modules/trees info. Resolved during ast generation
    // ModuleId -> ModuleInfo
    pub module_infos: Vec<ModuleInfo>,

    // Bindings resolved during name resolution
    // BindingInfoId -> BindingInfo
    pub binding_infos: Vec<BindingInfo>,

    // The entry point function's id (usually main). Resolved during name resolution
    pub entry_point_function_idx: Option<BindingInfoIdx>,

    // Foreign libraries needed to be linked. Resolved during name resolution
    pub foreign_libraries: HashSet<ForeignLibrary>,
}

impl<'w> Workspace<'w> {
    pub fn new(build_options: BuildOptions, root_dir: &'w Path, std_dir: &'w Path) -> Self {
        Self {
            build_options,
            files: SimpleFiles::new(),
            root_file_id: Default::default(),
            root_dir,
            std_dir,
            module_infos: Default::default(),
            root_module_id: Default::default(),
            binding_infos: Default::default(),
            entry_point_function_idx: None,
            foreign_libraries: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BindingInfo {
    // a reference to the info's own index
    pub idx: BindingInfoIdx,
    // the module where this binding lives
    pub module_idx: ModuleIdx,
    // the symbol(name) used for the binding
    pub symbol: Ustr,
    pub visibility: Visibility,
    pub ty: TyKind,
    pub const_value: Option<Value>,
    // what kind of access the binding has
    pub is_mutable: bool,
    pub kind: BindingInfoKind,
    // the scope depth of the binding
    pub level: ScopeLevel,
    // the scope name of the binding, i.e: `foo._.symbol._._`
    pub scope_name: Ustr,
    // the amount of times this binding was used
    pub uses: usize,
    // whether to codegen this binding or not
    pub codegen: bool,
    pub span: Span,
}

impl<'w> Workspace<'w> {
    pub fn add_module_info(&mut self, module_info: ModuleInfo) -> ModuleIdx {
        self.module_infos.push(module_info);
        ModuleIdx(self.module_infos.len() - 1)
    }

    pub fn get_module_info(&self, idx: ModuleIdx) -> Option<&ModuleInfo> {
        self.module_infos.get(idx.0)
    }

    pub fn find_module_info(&self, module_info: ModuleInfo) -> Option<ModuleIdx> {
        self.module_infos
            .iter()
            .position(|m| *m == module_info)
            .map(|i| ModuleIdx(i))
    }

    pub fn add_binding_info(
        &mut self,
        module_idx: ModuleIdx,
        symbol: Ustr,
        visibility: Visibility,
        is_mutable: bool,
        kind: BindingInfoKind,
        level: ScopeLevel,
        scope_name: Ustr,
        span: Span,
    ) -> BindingInfoIdx {
        self.add_binding_info_ex(
            module_idx,
            symbol,
            visibility,
            TyKind::Unknown,
            None,
            is_mutable,
            kind,
            level,
            scope_name,
            span,
        )
    }

    pub fn add_binding_info_ex(
        &mut self,
        module_idx: ModuleIdx,
        symbol: Ustr,
        visibility: Visibility,
        ty: TyKind,
        const_value: Option<Value>,
        is_mutable: bool,
        kind: BindingInfoKind,
        level: ScopeLevel,
        scope_name: Ustr,
        span: Span,
    ) -> BindingInfoIdx {
        let idx = BindingInfoIdx(self.binding_infos.len());
        self.binding_infos.push(BindingInfo {
            idx,
            module_idx,
            symbol,
            visibility,
            ty,
            const_value,
            is_mutable,
            kind,
            level,
            scope_name,
            uses: 0,
            codegen: false,
            span,
        });
        idx
    }

    pub fn get_binding_info(&self, idx: BindingInfoIdx) -> Option<&BindingInfo> {
        self.binding_infos.get(idx.0)
    }

    pub fn get_binding_info_mut(&mut self, idx: BindingInfoIdx) -> Option<&mut BindingInfo> {
        self.binding_infos.get_mut(idx.0)
    }
}

impl BindingInfo {
    pub fn qualified_name(&self) -> Ustr {
        ustr(&format!("{}.{}", self.scope_name, self.symbol))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindingInfoKind {
    Let,
    Type,
    Import,
}

impl BindingInfoKind {
    pub fn is_type(&self) -> bool {
        match self {
            BindingInfoKind::Type => true,
            _ => false,
        }
    }
}

bitflags! {
    struct Restrictions : u8 {
        const STMT_EXPR = 1 << 0;
        const NO_STRUCT_LITERAL = 1 << 1;
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ModuleIdx(pub usize);

impl ModuleIdx {
    pub fn invalid() -> Self {
        Self(usize::MAX)
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct BindingInfoIdx(pub usize);

impl BindingInfoIdx {
    pub fn invalid() -> Self {
        Self(usize::MAX)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ScopeLevel {
    Global,
    Scope(usize),
}

impl PartialOrd for ScopeLevel {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScopeLevel {
    fn cmp(&self, other: &Self) -> Ordering {
        use ScopeLevel::*;

        match (self, other) {
            (Global, Global) => Ordering::Equal,
            (Global, Scope(_)) => Ordering::Less,
            (Scope(_), Global) => Ordering::Greater,
            (Scope(s1), Scope(s2)) => s1.cmp(s2),
        }
    }
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
