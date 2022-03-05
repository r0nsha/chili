use crate::*;
use chili_ast::ast::{Ast, BindingKind, Visibility};
use chili_error::{DiagnosticResult, SyntaxError};

impl<'w> Parser<'w> {
    pub(crate) fn parse_top_level(
        &mut self,
        ast: &mut Ast,
    ) -> DiagnosticResult<()> {
        self.skip_redundant_tokens();

        let visibility = if eat!(self, Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if eat!(self, Use) {
            let imports = self.parse_import(visibility)?;
            ast.imports.extend(imports);
        } else if eat!(self, Type) {
            let binding =
                self.parse_binding(BindingKind::Type, visibility, true)?;

            ast.bindings.push(binding);
        } else if eat!(self, Let) {
            let binding = if eat!(self, Foreign) {
                self.parse_foreign_single(visibility)?
            } else {
                self.parse_binding(BindingKind::Value, visibility, true)?
            };

            ast.bindings.push(binding);
        } else if eat!(self, Foreign) {
            let bindings = self.parse_foreign_block()?;
            ast.bindings.extend(bindings);
        } else {
            return Err(SyntaxError::expected(
                self.span(),
                &format!("an item, got `{}`", self.peek().lexeme),
            ));
        }

        Ok(())
    }
}
