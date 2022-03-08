use crate::Resolver;
use chili_ast::{ast, workspace::Workspace};
use chili_error::{DiagnosticResult, SyntaxError};
use ustr::ustr;

// Trait for declaring top level bindings/imports/etc...
pub(crate) trait Declare<'w> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()>;
}

impl<'w, T: Declare<'w>> Declare<'w> for Vec<T> {
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
}

impl<'w, T: Declare<'w>> Declare<'w> for Option<T> {
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
}

impl<'w, T: Declare<'w>> Declare<'w> for Box<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.as_mut().declare(resolver, workspace)
    }
}

impl<'w> Declare<'w> for ast::Ast {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.bindings.declare(resolver, workspace)?;
        Ok(())
    }
}

impl<'w> Declare<'w> for ast::Import {
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
}

impl<'w> Declare<'w> for ast::Binding {
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
}
