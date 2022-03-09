use crate::scope::Scope;
use chili_ast::{
    ast::ModuleInfo,
    workspace::{BindingInfoId, ModuleId, ScopeLevel, Workspace},
};
use ustr::Ustr;

pub(crate) struct Resolver {
    pub(crate) module_id: ModuleId,
    pub(crate) module_info: ModuleInfo,
    pub(crate) global_scope: Scope,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) scope_level: ScopeLevel,
    pub(crate) function_scope_level: ScopeLevel,
}

impl Resolver {
    pub(crate) fn new() -> Self {
        Self {
            module_id: Default::default(),
            module_info: Default::default(),
            global_scope: Scope::new(""),
            scopes: vec![],
            scope_level: ScopeLevel::Global,
            function_scope_level: ScopeLevel::Global,
        }
    }

    pub(crate) fn in_global_scope(&self) -> bool {
        self.scope_level.is_global()
    }

    pub(crate) fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
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
        self.scopes
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<&str>>()
            .join(".")
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

        None
    }
}
