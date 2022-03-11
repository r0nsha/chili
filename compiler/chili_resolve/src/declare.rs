use crate::{scope::ScopeSymbol, Resolver};
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

        let id = workspace.add_binding_info(
            resolver.module_idx,
            self.alias,
            self.visibility,
            false,
            BindingInfoKind::Import,
            resolver.scope_level,
            ustr(&resolver.current_scope_name()),
            self.span,
        );

        resolver
            .current_scope_mut()
            .bindings
            .insert(self.alias, ScopeSymbol::persistent(id));

        Ok(())
    }
}

impl<'w> Declare<'w> for ast::Binding {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        // TODO: support global destructor patterns

        let pat = self.pattern.into_single_mut();

        check_duplicate_global_symbol(
            resolver, workspace, pat.symbol, pat.span,
        )?;

        pat.binding_info_idx = resolver.add_binding_with_symbol_pattern(
            workspace,
            pat,
            self.visibility,
            self.kind,
            false,
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
    match resolver.current_scope().bindings.get(&symbol) {
        Some(symbol) => {
            if symbol.is_shadowable() {
                Ok(())
            } else {
                let binding_info =
                    workspace.get_binding_info(symbol.id).unwrap();

                Err(SyntaxError::duplicate_symbol(
                    binding_info.span,
                    span,
                    binding_info.symbol,
                ))
            }
        }
        None => Ok(()),
    }
}
