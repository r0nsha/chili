use crate::ast::{
    workspace::{BindingId, ModuleId, ModuleInfo, ScopeLevel},
    FunctionId,
};
use ustr::{ustr, Ustr, UstrMap};

pub struct Env {
    module_id: ModuleId,
    module_info: ModuleInfo,
    scopes: Vec<Scope>,
    scope_level: ScopeLevel,
}

impl Env {
    pub fn new(module_id: ModuleId, module_info: ModuleInfo) -> Self {
        Self {
            module_id,
            module_info,
            scopes: vec![],
            scope_level: ScopeLevel::Global,
        }
    }

    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }

    #[allow(unused)]
    pub fn module_info(&self) -> ModuleInfo {
        self.module_info
    }

    #[allow(unused)]
    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    #[allow(unused)]
    pub fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn scope_level(&self) -> ScopeLevel {
        self.scope_level
    }

    pub fn scope_name(&self) -> Ustr {
        let scopes_str = self
            .scopes
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<&str>>()
            .join(".");

        let str = if self.module_info.name.is_empty() {
            scopes_str
        } else {
            format!("{}.{}", self.module_info.name, scopes_str)
        };

        ustr(&str)
    }

    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.push_named_scope("_", kind);
    }

    pub fn push_named_scope(&mut self, name: impl ToString, kind: ScopeKind) {
        self.scopes.push(Scope::new(name, kind));
        self.scope_level = self.scope_level.next();
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        self.scope_level = self.scope_level.previous();
    }

    pub fn insert_symbol(&mut self, symbol: Ustr, id: BindingId) {
        self.scope_mut().insert_symbol(symbol, id);
    }

    pub fn insert_function(&mut self, symbol: Ustr, id: FunctionId) {
        self.scope_mut().insert_function(symbol, id);
    }

    pub fn find_symbol(&self, symbol: Ustr) -> Option<BindingId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.symbols.get(&symbol) {
                return Some(*id);
            }
        }

        None
    }

    pub fn find_function(&self, symbol: Ustr) -> Option<FunctionId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.functions.get(&symbol) {
                return Some(*id);
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    pub name: String,
    pub symbols: UstrMap<BindingId>,
    pub functions: UstrMap<FunctionId>,
}

impl Scope {
    pub fn new(name: impl ToString, kind: ScopeKind) -> Self {
        Self {
            kind,
            name: name.to_string(),
            symbols: UstrMap::default(),
            functions: UstrMap::default(),
        }
    }

    pub fn insert_symbol(&mut self, symbol: Ustr, id: BindingId) {
        self.symbols.insert(symbol, id);
    }

    pub fn insert_function(&mut self, symbol: Ustr, id: FunctionId) {
        self.functions.insert(symbol, id);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ScopeKind {
    Global,
    Function,
    Block,
    Loop,
}
