mod scope;

use chili_ast::{
    ast::{Ast, Binding},
    workspace::{BindingInfoId, BindingLevel, ModuleId, Workspace},
};
use chili_error::DiagnosticResult;
use scope::Scope;
use ustr::Ustr;

pub fn resolve<'w>(
    workspace: &mut Workspace<'w>,
    mut asts: Vec<Ast>,
) -> DiagnosticResult<()> {
    for ast in asts.iter_mut() {
        let mut resolver = Resolver {
            module_id: Default::default(),
            scopes: Default::default(),
        };

        resolver.push_scope();

        ast.declare(&mut resolver, workspace)?;
        ast.define(&mut resolver, workspace)?;

        workspace.add_module(ast.clone());

        resolver.pop_scope();
    }

    Ok(())
}

struct Resolver {
    module_id: ModuleId,
    scopes: Vec<Scope>,
}

impl Resolver {
    fn in_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_binding_level(&mut self) -> BindingLevel {
        BindingLevel(self.scopes.len())
    }

    fn lookup_binding<'w>(
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

trait Resolve<'w> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()>;

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()>;
}

impl<'w, T: Resolve<'w>> Resolve<'w> for Vec<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        for element in self {
            element.declare(resolver, workspace)?;
        }
        Ok(())
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        for element in self {
            element.define(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<'w, T: Resolve<'w>> Resolve<'w> for Option<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.declare(resolver, workspace)?;
        }
        Ok(())
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.define(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<'w, T: Resolve<'w>> Resolve<'w> for Box<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.as_mut().declare(resolver, workspace)
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.as_mut().define(resolver, workspace)
    }
}

impl<'w> Resolve<'w> for Ast {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.bindings.declare(resolver, workspace)?;
        Ok(())
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.bindings.define(resolver, workspace)?;
        Ok(())
    }
}

impl<'w> Resolve<'w> for Binding {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        if resolver.in_global_scope() {
            if workspace
                .binding_infos
                .iter()
                .find(|b| b.symbol == self.pattern.into_single().symbol)
                .is_some()
            {}
        }

        Ok(())
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        todo!()
    }
}
