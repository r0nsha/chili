use crate::Resolver;
use chili_ast::{
    ast,
    workspace::{BindingInfoKind, Workspace},
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;
use ustr::{ustr, Ustr};

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
        self.imports.declare(resolver, workspace)?;
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
        check_duplicate_global_symbol(
            resolver, workspace, self.alias, self.span,
        )?;

        workspace.add_binding_info(
            resolver.module_id,
            self.alias,
            self.visibility,
            false,
            BindingInfoKind::Import,
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
        // TODO: support destructor patterns
        let pattern = self.pattern.into_single();

        check_duplicate_global_symbol(
            resolver,
            workspace,
            pattern.symbol,
            pattern.span,
        )?;

        self.binding_info_id = workspace.add_binding_info(
            resolver.module_id,
            pattern.symbol,
            self.visibility,
            pattern.is_mutable,
            match self.kind {
                ast::BindingKind::Let => BindingInfoKind::Let,
                ast::BindingKind::Type => BindingInfoKind::Type,
            },
            resolver.current_binding_level(),
            ustr(&resolver.current_scope_name()),
            pattern.span,
        );

        Ok(())
    }
}

fn check_duplicate_global_symbol<'w>(
    resolver: &Resolver,
    workspace: &Workspace<'w>,
    symbol: Ustr,
    span: Span,
) -> DiagnosticResult<()> {
    if resolver.in_global_scope() {
        if let Some(binding) = workspace
            .binding_infos
            .iter()
            .filter(|b| b.module_id == resolver.module_id)
            .find(|b| b.symbol == symbol)
        {
            return Err(SyntaxError::duplicate_symbol(
                binding.span,
                span,
                binding.symbol,
            ));
        }
    }

    Ok(())
}
