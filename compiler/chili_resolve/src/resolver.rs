use std::collections::HashMap;

use crate::{import::ModuleExports, scope::Scope};
use chili_ast::{
    ast::ModuleInfo,
    workspace::{BindingInfoId, ModuleId, ScopeLevel, Workspace},
};
use ustr::{Ustr, UstrMap};

pub(crate) struct Resolver {
    // All exports from all modules, used for resolving
    // glob imports, i.e `foo.?`
    pub(crate) exports: ModuleExports,

    // The current module's id and information
    pub(crate) module_id: ModuleId,
    pub(crate) module_info: ModuleInfo,

    // Symbols maps / Scopes
    pub(crate) builtin_types: UstrMap<BindingInfoId>,
    pub(crate) global_scopes: HashMap<ModuleId, Scope>,
    pub(crate) scopes: Vec<Scope>,

    // Scope information
    pub(crate) scope_level: ScopeLevel,
    pub(crate) function_scope_level: ScopeLevel,
}

impl Resolver {
    pub(crate) fn new() -> Self {
        Self {
            exports: Default::default(),
            module_id: Default::default(),
            module_info: Default::default(),
            builtin_types: Default::default(),
            global_scopes: Default::default(),
            scopes: vec![],
            scope_level: ScopeLevel::Global,
            function_scope_level: ScopeLevel::Global,
        }
    }

    pub(crate) fn in_global_scope(&self) -> bool {
        self.scope_level.is_global()
    }

    pub(crate) fn current_scope(&self) -> &Scope {
        if self.scopes.is_empty() {
            self.global_scopes.get(&self.module_id).unwrap()
        } else {
            self.scopes.last().unwrap()
        }
    }

    pub(crate) fn current_scope_mut(&mut self) -> &mut Scope {
        if self.scopes.is_empty() {
            self.global_scopes.get_mut(&self.module_id).unwrap()
        } else {
            self.scopes.last_mut().unwrap()
        }
    }

    pub(crate) fn push_scope(&mut self) {
        self.push_named_scope("_");
    }

    pub(crate) fn push_named_scope(&mut self, name: impl ToString) {
        self.scopes.push(Scope::new(name));
        self.scope_level = self.scope_level.next();
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
        self.scope_level = self.scope_level.previous();
    }

    pub(crate) fn current_scope_name(&mut self) -> String {
        let scopes_str = self
            .scopes
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<&str>>()
            .join(".");

        if self.module_info.name.is_empty() {
            scopes_str
        } else {
            format!("{}.{}", self.module_info.name, scopes_str)
        }
    }

    pub(crate) fn lookup_binding<'w>(
        &self,
        workspace: &mut Workspace<'w>,
        symbol: Ustr,
    ) -> Option<BindingInfoId> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.bindings.get(&symbol) {
                workspace.binding_infos[symbol.id.0].uses += 1;
                return Some(symbol.id);
            }
        }

        let global_scope = self.global_scopes.get(&self.module_id).unwrap();
        if let Some(symbol) = global_scope.bindings.get(&symbol) {
            workspace.binding_infos[symbol.id.0].uses += 1;
            return Some(symbol.id);
        }

        if let Some(id) = self.builtin_types.get(&symbol) {
            return Some(*id);
        }

        None
    }
}
