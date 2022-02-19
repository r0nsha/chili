use ustr::Ustr;

use crate::{entity::Entity, use_decl::UseDecl};

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub info: ModuleInfo,
    pub uses: Vec<UseDecl>,
    pub entities: Vec<Entity>,
}

impl Module {
    pub fn new(info: ModuleInfo) -> Self {
        Self {
            info,
            uses: vec![],
            entities: vec![],
        }
    }

    pub fn find_entity(&self, symbol: impl Into<Ustr>) -> Option<&Entity> {
        let symbol = symbol.into();
        self.entities
            .iter()
            .find(|entity| entity.pattern.into_single().symbol == symbol)
    }

    pub fn find_use(&self, symbol: impl Into<Ustr>) -> Option<&UseDecl> {
        let symbol = symbol.into();
        self.uses.iter().find(|use_| use_.alias == symbol)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ModuleInfo {
    pub name: Ustr,
    pub file_path: Ustr,
}

impl ModuleInfo {
    pub fn new(name: Ustr, file_path: Ustr) -> Self {
        Self { name, file_path }
    }
}
