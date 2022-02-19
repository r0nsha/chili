use std::collections::HashSet;

use codespan_reporting::files::SimpleFiles;
use common::compiler_info::STD;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    foreign_library::ForeignLibrary,
    func::Fn,
    module::{Module, ModuleInfo},
};

#[derive(Debug, Clone)]
pub struct Ir {
    pub modules: UstrMap<Module>,
    pub startup_fn: Option<Fn>,
    pub foreign_libraries: HashSet<ForeignLibrary>,
    pub files: SimpleFiles<String, String>,
}

impl Ir {
    pub fn new(files: SimpleFiles<String, String>) -> Self {
        Self {
            modules: UstrMap::default(),
            startup_fn: None,
            foreign_libraries: HashSet::new(),
            files,
        }
    }

    #[inline]
    pub fn std_module(&self, m: &str) -> &Module {
        self.module(ustr(&format!("{}.{}", STD, m)))
    }

    #[inline]
    pub fn root_module(&self) -> &Module {
        self.module(common::builtin::root_module())
    }

    #[inline]
    pub fn module(&self, symbol: impl Into<Ustr>) -> &Module {
        let symbol = symbol.into();
        self.modules
            .get(&symbol)
            .expect(&format!("couldn't find `{}`", symbol))
    }

    #[inline]
    pub fn module_info(&self, symbol: impl Into<Ustr>) -> ModuleInfo {
        let symbol = symbol.into();
        self.modules
            .get(&symbol)
            .expect(&format!("couldn't find `{}`", symbol))
            .info
    }
}
