mod import;
mod scope;

use chili_ast::{
    ast::{Ast, Binding, Import, ModuleInfo},
    workspace::{
        BindingInfo, BindingInfoId, BindingLevel, ModuleId, Workspace,
    },
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_ty::Ty;
use scope::Scope;
use ustr::{ustr, Ustr};

pub fn resolve<'w>(
    workspace: &mut Workspace<'w>,
    mut asts: Vec<Ast>,
) -> DiagnosticResult<()> {
    for ast in asts.iter_mut() {
        let mut resolver = Resolver {
            module_id: workspace.next_module_id(),
            module_info: ast.module_info,
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
    module_info: ModuleInfo,
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
        self.scopes.push(Scope::new("_"));
    }

    fn push_named_scope(&mut self, name: impl ToString) {
        self.scopes.push(Scope::new(name));
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_binding_level(&mut self) -> BindingLevel {
        BindingLevel(self.scopes.len())
    }

    fn current_scope_name(&mut self) -> String {
        "".to_string()
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

impl<'w> Resolve<'w> for Import {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        if resolver.in_global_scope() {
            if let Some(binding) = workspace
                .binding_infos
                .iter()
                .find(|b| b.symbol == self.alias)
            {
                return Err(SyntaxError::duplicate_symbol(
                    binding.span,
                    self.span,
                    binding.symbol,
                ));
            }
        }

        workspace.add_binding_info(
            resolver.module_id,
            self.alias,
            self.visibility,
            false,
            resolver.current_binding_level(),
            ustr(&resolver.current_scope_name()),
            self.span,
        );

        Ok(())
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        Ok(())
    }
}

impl<'w> Resolve<'w> for Binding {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        // TODO: support other patterns
        let pattern = self.pattern.into_single();

        if resolver.in_global_scope() {
            if let Some(binding) = workspace
                .binding_infos
                .iter()
                .find(|b| b.symbol == pattern.symbol)
            {
                return Err(SyntaxError::duplicate_symbol(
                    binding.span,
                    self.pattern.span(),
                    binding.symbol,
                ));
            }
        }

        workspace.add_binding_info(
            resolver.module_id,
            pattern.symbol,
            self.visibility,
            pattern.is_mutable,
            resolver.current_binding_level(),
            ustr(&resolver.current_scope_name()),
            pattern.span,
        );

        Ok(())
    }

    fn define(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        Ok(())
    }
}
