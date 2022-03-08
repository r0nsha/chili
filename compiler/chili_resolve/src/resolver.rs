use crate::scope::Scope;
use chili_ast::{
    ast::ModuleInfo,
    workspace::{BindingInfoId, BindingLevel, ModuleId, Workspace},
};
use ustr::Ustr;

pub(crate) struct Resolver {
    pub(crate) module_id: ModuleId,
    pub(crate) module_info: ModuleInfo,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) function_scope: usize,
}

impl Resolver {
    pub(crate) fn new(module_id: ModuleId, module_info: ModuleInfo) -> Self {
        Self {
            module_id,
            module_info,
            scopes: Default::default(),
            function_scope: Default::default(),
        }
    }

    pub(crate) fn in_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }

    pub(crate) fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub(crate) fn push_scope(&mut self) {
        self.scopes.push(Scope::new("_"));
    }

    pub(crate) fn push_named_scope(&mut self, name: impl ToString) {
        self.scopes.push(Scope::new(name));
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn current_binding_level(&mut self) -> BindingLevel {
        BindingLevel(self.scopes.len())
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
            if let Some(id) = scope.bindings.get(&symbol) {
                workspace.binding_infos[id.0].uses += 1;
                return Some(*id);
            }
        }

        None
    }
}
