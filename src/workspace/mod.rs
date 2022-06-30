use crate::{
    ast::{BindingKind, ExternLibrary, Visibility},
    common::{
        build_options::{BuildOptions, DiagnosticOptions},
        id_cache::{IdCache, WithId},
    },
    define_id_type,
    error::{emit_diagnostics, emitter::ColorMode, Diagnostics},
    hir::const_value::ConstValue,
    span::{FileId, Span},
    types::TypeId,
};
use bitflags::bitflags;
use std::{
    cmp::Ordering,
    collections::HashSet,
    path::{Path, PathBuf},
};
use ustr::{ustr, Ustr};

pub mod compiler_info;

pub struct Workspace {
    pub name: String,

    // The build options, either passed by the user, or inferred from the host machine
    pub build_options: BuildOptions,

    // Diagnostics are responsible for both keeping errors/warnings and for emitting them
    pub diagnostics: Diagnostics,

    // The workspace's root directory
    pub root_dir: PathBuf,

    // Std library's root directory
    pub std_dir: PathBuf,

    // The root module's id. Resolved after ast generation
    pub root_module_id: ModuleId,

    // Parsed modules/trees info. Resolved during ast generation
    // ModuleId -> ModuleInfo
    pub module_infos: IdCache<ModuleId, ModuleInfo>,

    // Bindings resolved during semantic analysis
    // BindingInfoId -> BindingInfo
    pub binding_infos: IdCache<BindingId, BindingInfo>,

    // The entry point function's id (usually named "main"). Resolved during semantic analysis
    pub entry_point_function_id: Option<BindingId>,

    // Extern libraries needed to be linked. Resolved during semantic analysis
    pub extern_libraries: HashSet<ExternLibrary>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BindingInfo {
    // a reference to the info's own index
    pub id: BindingId,
    // the module where this binding lives
    pub module_id: ModuleId,
    // the name used for the binding
    pub name: Ustr,
    pub visibility: Visibility,
    pub ty: TypeId,
    pub const_value: Option<ConstValue>,
    // what kind of access the binding has
    pub is_mutable: bool,
    pub kind: BindingKind,
    // the scope depth of the binding
    pub scope_level: ScopeLevel,
    // the fully qualified name of the binding,
    // including the module name and the binding name i.e: `module_name.binding_name`
    pub qualified_name: Ustr,
    pub flags: BindingInfoFlags,
    // the amount of times this binding was used
    pub uses: Vec<Span>,
    pub span: Span,
}

impl WithId<BindingId> for BindingInfo {
    fn id(&self) -> &BindingId {
        &self.id
    }

    fn id_mut(&mut self) -> &mut BindingId {
        &mut self.id
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PartialBindingInfo {
    pub module_id: ModuleId,
    pub name: Ustr,
    pub visibility: Visibility,
    pub ty: TypeId,
    pub const_value: Option<ConstValue>,
    pub is_mutable: bool,
    pub kind: BindingKind,
    pub scope_level: ScopeLevel,
    pub qualified_name: Ustr,
    pub span: Span,
    pub flags: BindingInfoFlags,
}

impl PartialBindingInfo {
    pub fn into_binding_info(self) -> BindingInfo {
        BindingInfo {
            id: BindingId::unknown(),
            module_id: self.module_id,
            name: self.name,
            visibility: self.visibility,
            ty: self.ty,
            const_value: self.const_value,
            is_mutable: self.is_mutable,
            kind: self.kind,
            scope_level: self.scope_level,
            qualified_name: self.qualified_name,
            flags: self.flags,
            uses: vec![],
            span: self.span,
        }
    }
}

bitflags! {
    pub struct BindingInfoFlags : u8 {
        // Whether this binding is a builtin type, such as u8, i32, int, etc...
        const BUILTIN_TYPE = 1 << 0;
        // Whether this binding comes from the user's code (this is turned off for compiler-generated bindings).
        const IS_USER_DEFINED = 1 << 1;
        // Whether the type of this binding was inferred. This depends on IS_USER_BINDING.
        const TYPE_WAS_INFERRED = 1 << 2;
        // Whether this the implicit "it" parameter.
        const IS_IMPLICIT_IT_FN_PARAMETER = 1 << 3;
        // Whether this binding should store a constant value
        const NO_CONST_FOLD = 1 << 3;
    }
}

impl Workspace {
    pub fn new(
        name: String,
        build_options: BuildOptions,
        root_dir: PathBuf,
        std_dir: PathBuf,
    ) -> Self {
        Self {
            name,
            diagnostics: Diagnostics::new(),
            build_options,
            root_dir,
            std_dir,
            module_infos: Default::default(),
            root_module_id: Default::default(),
            binding_infos: Default::default(),
            entry_point_function_id: None,
            extern_libraries: Default::default(),
        }
    }

    pub fn emit_diagnostics(&self) {
        match &self.build_options.diagnostic_options {
            DiagnosticOptions::Emit { no_color } => {
                emit_diagnostics(
                    &self.diagnostics,
                    if *no_color {
                        ColorMode::Never
                    } else {
                        ColorMode::Always
                    },
                );
            }
            DiagnosticOptions::DontEmit => (),
        }
    }

    pub fn get_root_module_info(&self) -> &ModuleInfo {
        self.module_infos.get(self.root_module_id).unwrap()
    }

    pub fn find_module_id_by_file_id(&self, file_id: FileId) -> Option<ModuleId> {
        self.module_infos
            .iter()
            .position(|(_, module)| module.file_id == file_id)
            .map(ModuleId::from)
    }

    pub fn add_binding_info_use(&mut self, id: BindingId, span: Span) {
        if let Some(binding_info) = self.binding_infos.get_mut(id) {
            binding_info.add_use(span);
        }
    }

    pub fn entry_point_function(&self) -> Option<&BindingInfo> {
        self.entry_point_function_id
            .and_then(|id| self.binding_infos.get(id))
    }
}

define_id_type!(ModuleId);
define_id_type!(BindingId);

#[derive(Debug, Default, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ModuleInfo {
    pub name: Ustr,
    pub file_path: Ustr,
    pub file_id: FileId,
}

impl ModuleInfo {
    pub fn new(name: Ustr, file_path: Ustr, file_id: FileId) -> Self {
        Self {
            name,
            file_path,
            file_id,
        }
    }

    pub fn dir(&self) -> &Path {
        Path::new(self.file_path.as_str()).parent().unwrap()
    }
}

#[derive(Debug, Default, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PartialModuleInfo {
    pub name: Ustr,
    pub file_path: Ustr,
}

impl PartialModuleInfo {
    pub fn new(name: Ustr, file_path: Ustr) -> Self {
        Self { name, file_path }
    }

    pub fn from_path<'a>(file_path: &'a Path) -> Self {
        Self::new(
            ustr(file_path.file_stem().unwrap().to_str().unwrap()),
            ustr(file_path.to_str().unwrap()),
        )
    }

    #[allow(unused)]
    pub fn dir(&self) -> &Path {
        Path::new(self.file_path.as_str()).parent().unwrap()
    }
}

impl From<PartialModuleInfo> for ModuleInfo {
    fn from(p: PartialModuleInfo) -> Self {
        Self::new(p.name, p.file_path, FileId::MAX)
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
        matches!(self, ScopeLevel::Global)
    }

    #[allow(unused)]
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

impl BindingInfo {
    pub(crate) fn add_use(&mut self, span: Span) {
        self.uses.push(span);
    }
}
