use crate::ast::{Ast, Binding, ForeignLibrary};
use chili_span::FileId;
use chili_ty::Ty;
use codespan_reporting::files::SimpleFiles;
use std::{collections::HashSet, path::Path};

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
    pub parsed_modules: Vec<Ast>,

    // The root module's id. Resolved after ast generation
    pub root_module: ModuleId,

    // Bindings resolved during name resolution
    // BindingId -> BindingDef
    pub bindings: Vec<BindingDef<'w>>,

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
            parsed_modules: Default::default(),
            root_module: Default::default(),
            bindings: Default::default(),
            foreign_libraries: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BindingDef<'w> {
    binding: Binding,
    scope_level: ScopeLevel,
    scope_name: &'w str,
    uses: usize,
}

impl<'w> Workspace<'w> {
    pub fn add_module(&mut self, ast: Ast) -> ModuleId {
        self.parsed_modules.push(ast);
        ModuleId(self.parsed_modules.len() - 1)
    }

    pub fn get_module(&self, id: ModuleId) -> Option<&Ast> {
        self.parsed_modules.get(id.0)
    }
}

macro_rules! id_struct {
    ($id:ident) => {
        #[derive(
            Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy,
        )]
        pub struct $id(usize);
    };
}

id_struct!(ModuleId);
id_struct!(BindingId);
id_struct!(ScopeLevel);
