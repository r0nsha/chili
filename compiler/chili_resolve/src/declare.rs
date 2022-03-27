use crate::{scope::ScopeSymbol, Resolver};
use chili_ast::{
    ast::{self, BindingKind},
    workspace::Workspace,
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;
use ustr::Ustr;

// Trait for declaring top level bindings/imports/etc...
pub(crate) trait Declare {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()>;
}

impl<T: Declare> Declare for Vec<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        for element in self {
            element.declare(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<T: Declare> Declare for Option<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.declare(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<T: Declare> Declare for Box<T> {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        self.as_mut().declare(resolver, workspace)
    }
}

impl Declare for ast::Ast {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        self.imports.declare(resolver, workspace)?;
        self.bindings.declare(resolver, workspace)?;
        Ok(())
    }
}

impl Declare for ast::Import {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        check_duplicate_global_symbol(resolver, workspace, self.alias, self.span)?;

        self.binding_info_id = resolver.add_binding(
            workspace,
            self.alias,
            self.visibility,
            false,
            BindingKind::Import,
            self.span,
            false,
        );

        resolver
            .current_scope_mut()
            .bindings
            .insert(self.alias, ScopeSymbol::persistent(self.binding_info_id));

        Ok(())
    }
}

impl Declare for ast::Binding {
    fn declare(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        // TODO: support global unpack patterns

        let pat = self.pattern.as_single_mut();

        check_duplicate_global_symbol(resolver, workspace, pat.symbol, pat.span)?;

        pat.binding_info_id = resolver.add_binding_with_symbol_pattern(
            workspace,
            pat,
            self.visibility,
            self.kind,
            false,
        );

        if let Some(value) = &mut self.expr {
            // if this is the entry point function, keep its index in the workspace
            // the entry point function is a binding with the name "main" and a value of `ExprKind::Fn`
            if workspace.entry_point_function_id.is_none() && pat.symbol == "main" && value.is_fn()
            {
                workspace.entry_point_function_id = Some(pat.binding_info_id);

                workspace
                    .get_binding_info_mut(workspace.entry_point_function_id.unwrap())
                    .unwrap()
                    .uses += 1;

                value.as_fn_mut().is_entry_point = true;
            }
        }

        Ok(())
    }
}

fn check_duplicate_global_symbol(
    resolver: &Resolver,
    workspace: &Workspace,
    symbol: Ustr,
    span: Span,
) -> DiagnosticResult<()> {
    match resolver.current_scope().bindings.get(&symbol) {
        Some(symbol) => {
            if symbol.is_shadowable() {
                Ok(())
            } else {
                let binding_info = workspace.get_binding_info(symbol.id).unwrap();

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
