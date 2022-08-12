use self::library::Library;
use crate::{
    ast,
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
    path::{Path, PathBuf},
};
use ustr::{ustr, Ustr, UstrMap};

pub mod library;

const SOURCE_FILE_EXT: &str = "chl";

pub struct Workspace {
    pub name: String,

    // The build options, either passed by the user, or inferred from the host machine
    pub build_options: BuildOptions,

    // Diagnostics are responsible for both keeping errors/warnings and for emitting them
    pub diagnostics: Diagnostics,

    // All libraries used by this workspace
    pub libraries: IdCache<LibraryId, Library>,

    // The root module's id. Resolved after ast generation
    pub root_module_id: ModuleId,

    // Parsed modules/trees info. Resolved during ast generation
    // ModuleId -> ModuleInfo
    pub module_infos: IdCache<ModuleId, ModuleInfo>,

    // Bindings resolved during semantic analysis
    // BindingInfoId -> BindingInfo
    pub binding_infos: IdCache<BindingId, BindingInfo>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BindingInfo {
    // a reference to the info's own index
    pub id: BindingId,
    // the module where this binding lives
    pub module_id: ModuleId,
    // the name used for the binding
    pub name: Ustr,
    pub visibility: ast::Visibility,
    pub ty: TypeId,
    pub const_value: Option<ConstValue>,
    // what kind of access the binding has
    pub is_mutable: bool,
    pub kind: BindingInfoKind,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BindingInfoKind {
    Orphan,
    Static,
    Function,
    ExternFunction,
    ExternVariable,
    Intrinsic,
    Type,
}

impl WithId<BindingId> for BindingInfo {
    fn id(&self) -> &BindingId {
        &self.id
    }

    fn id_mut(&mut self) -> &mut BindingId {
        &mut self.id
    }
}

impl BindingInfo {
    pub(crate) fn add_use(&mut self, span: Span) {
        self.uses.push(span);
    }

    #[inline]
    #[allow(unused)]
    pub fn is_builtin_type(&self) -> bool {
        self.flags.contains(BindingInfoFlags::BUILTIN_TYPE)
    }

    #[inline]
    #[allow(unused)]
    pub fn is_is_user_defined(&self) -> bool {
        self.flags.contains(BindingInfoFlags::IS_USER_DEFINED)
    }

    #[inline]
    #[allow(unused)]
    pub fn is_type_was_inferred(&self) -> bool {
        self.flags.contains(BindingInfoFlags::TYPE_WAS_INFERRED)
    }

    #[inline]
    #[allow(unused)]
    pub fn is_implicit_it_function_param(&self) -> bool {
        self.flags.contains(BindingInfoFlags::IMPLICIT_IT_FUNCTION_PARAM)
    }

    #[inline]
    #[allow(unused)]
    pub fn is_no_const_fold(&self) -> bool {
        self.flags.contains(BindingInfoFlags::NO_CONST_FOLD)
    }

    #[inline]
    #[allow(unused)]
    pub fn is_shadowable(&self) -> bool {
        self.flags.contains(BindingInfoFlags::SHADOWABLE)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PartialBindingInfo {
    pub module_id: ModuleId,
    pub name: Ustr,
    pub visibility: ast::Visibility,
    pub ty: TypeId,
    pub const_value: Option<ConstValue>,
    pub is_mutable: bool,
    pub kind: BindingInfoKind,
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
        // Whether this is the `it` implicitly inserted function parameter
        const IMPLICIT_IT_FUNCTION_PARAM = 1 << 3;
        // Whether this binding should store a constant value
        const NO_CONST_FOLD = 1 << 4;
        // Whether this binding should store a constant value
        const SHADOWABLE = 1 << 5;
        // Whether this binding was ignored using `_`
        const IGNORE = 1 << 6;
    }
}

impl Workspace {
    pub fn new(name: String, build_options: BuildOptions, main_library: Library) -> Self {
        let mut libraries = IdCache::new();

        libraries.insert_with_id(main_library);
        libraries.insert_with_id(Library::std());

        Self {
            name,
            diagnostics: Diagnostics::new(),
            build_options,
            libraries,
            module_infos: Default::default(),
            root_module_id: Default::default(),
            binding_infos: Default::default(),
        }
    }

    pub fn main_library(&self) -> &Library {
        self.libraries.get(LIBRARY_ID_MAIN).unwrap()
    }

    pub fn std_library(&self) -> &Library {
        self.libraries.get(LIBRARY_ID_STD).unwrap()
    }

    pub fn library_map(&self) -> UstrMap<Library> {
        let mut map = UstrMap::default();

        for (_, library) in self.libraries.iter() {
            map.insert(library.name, library.clone());
        }

        map
    }

    pub fn emit_diagnostics(&self) {
        match &self.build_options.diagnostic_options {
            DiagnosticOptions::Emit { no_color } => {
                emit_diagnostics(
                    &self.diagnostics,
                    if *no_color { ColorMode::Never } else { ColorMode::Always },
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
        self.binding_infos.get_mut(id).unwrap().add_use(span);
    }
}

define_id_type!(LibraryId);

const LIBRARY_ID_MAIN: LibraryId = LibraryId(0);
const LIBRARY_ID_STD: LibraryId = LibraryId(1);

define_id_type!(ModuleId);
define_id_type!(BindingId);

#[derive(Debug, Default, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ModuleInfo {
    pub name: Ustr,
    pub file_path: Ustr,
    pub file_id: FileId,
}

impl ModuleInfo {
    pub fn dir(&self) -> &Path {
        Path::new(self.file_path.as_str()).parent().unwrap()
    }
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Hash)]
pub struct ModulePath {
    library: Library,
    components: Vec<Ustr>,
}

impl ModulePath {
    pub fn new(library: Library, components: Vec<Ustr>) -> Self {
        Self {
            library: library.clone(),
            components,
        }
    }

    pub fn push(&mut self, component: Ustr) {
        self.components.push(component)
    }

    pub fn pop(&mut self) -> Option<Ustr> {
        self.components.pop()
    }

    pub fn library(&self) -> &Library {
        &self.library
    }

    pub fn components(&self) -> &[Ustr] {
        &self.components
    }

    pub fn name(&self) -> String {
        [self.library.name]
            .iter()
            .chain(self.components.iter())
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join(".")
    }

    pub fn path(&self) -> PathBuf {
        Self::build_path(self.library.root_dir(), &self.components)
    }

    pub fn build_path(root_dir: &Path, components: &[Ustr]) -> PathBuf {
        let mut path = root_dir.to_path_buf();

        for component in components {
            path.push(component.as_str());
        }

        path.set_extension(SOURCE_FILE_EXT);

        path
    }
}

impl From<&ModulePath> for ModuleInfo {
    fn from(p: &ModulePath) -> Self {
        Self {
            name: ustr(&p.name()),
            file_path: ustr(&p.path().to_str().unwrap()),
            file_id: FileId::MAX,
        }
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
