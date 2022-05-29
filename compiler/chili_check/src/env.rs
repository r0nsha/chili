use chili_ast::workspace::{BindingInfoId, ModuleId, ModuleInfo, ScopeLevel};
use ustr::{ustr, Ustr, UstrMap};

use crate::defer::DeferStack;

pub(crate) struct Env {
    module_id: ModuleId,
    module_info: ModuleInfo,
    scopes: Vec<Scope>,
    scope_level: ScopeLevel,
}

impl Env {
    pub(crate) fn new(module_id: ModuleId, module_info: ModuleInfo) -> Self {
        Self {
            module_id,
            module_info,
            scopes: vec![],
            scope_level: ScopeLevel::Global,
        }
    }

    pub(crate) fn module_id(&self) -> ModuleId {
        self.module_id
    }

    pub(crate) fn module_info(&self) -> ModuleInfo {
        self.module_info
    }

    pub(crate) fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    #[allow(unused)]
    pub(crate) fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub(crate) fn scope_level(&self) -> ScopeLevel {
        self.scope_level
    }

    pub(crate) fn scope_name(&self) -> Ustr {
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

    pub(crate) fn push_scope(&mut self, kind: ScopeKind) {
        self.push_named_scope("_", kind);
    }

    pub(crate) fn push_named_scope(&mut self, name: impl ToString, kind: ScopeKind) {
        self.scopes.push(Scope::new(name, kind));
        self.scope_level = self.scope_level.next();
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
        self.scope_level = self.scope_level.previous();
    }

    pub(crate) fn find_symbol(&self, symbol: Ustr) -> Option<BindingInfoId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.symbols.get(&symbol) {
                return Some(*id);
            }
        }

        None
    }

    pub(crate) fn insert_symbol(&mut self, symbol: Ustr, id: BindingInfoId) {
        self.scope_mut().insert(symbol, id);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) kind: ScopeKind,
    pub(crate) name: String,
    pub(crate) symbols: UstrMap<BindingInfoId>,
    pub(crate) defer_stack: DeferStack,
}

impl Scope {
    pub(crate) fn new(name: impl ToString, kind: ScopeKind) -> Self {
        Self {
            kind,
            name: name.to_string(),
            symbols: Default::default(),
            defer_stack: DeferStack::new(),
        }
    }

    pub(crate) fn insert(&mut self, symbol: Ustr, id: BindingInfoId) {
        self.symbols.insert(symbol, id);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum ScopeKind {
    Global,
    Function,
    Block,
    Loop,
}
