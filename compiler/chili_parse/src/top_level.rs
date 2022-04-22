use crate::*;
use chili_ast::ast::{Ast, BindingKind, Visibility};
use chili_error::SyntaxError;

impl<'p> Parser<'p> {
    pub(crate) fn parse_top_level(&mut self, ast: &mut Ast) -> Result<(), ()> {
        self.skip_trailing_semicolons();

        let visibility = if eat!(self, Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if eat!(self, Use) {
            let imports = self.parse_import(visibility).or_recover(self)?;
            ast.imports.extend(imports);
        } else if eat!(self, Type) {
            let binding = self
                .parse_binding(BindingKind::Type, visibility, true)
                .or_recover(self)?;
            ast.bindings.push(binding);
        } else if eat!(self, Let) {
            let binding = if eat!(self, Foreign) {
                self.parse_foreign_single(visibility)
            } else {
                self.parse_binding(BindingKind::Value, visibility, true)
            }
            .or_recover(self)?;
            ast.bindings.push(binding);
        } else if eat!(self, Foreign) {
            let bindings = self.parse_foreign_block().or_recover(self)?;
            ast.bindings.extend(bindings);
        } else if eat!(self, At) {
            let token = expect!(self, Ident(_), "ident").or_recover(self)?;
            let symbol = token.symbol();

            if symbol == "run" {
                expect!(self, OpenParen, "(").or_recover(self)?;
                let expr = self.parse_expr().or_recover(self)?;
                ast.run_exprs.push(expr);
                expect!(self, OpenParen, ")").or_recover(self)?;
            } else {
                self.diagnostics
                    .push(SyntaxError::expected(self.previous_span(), "run"));
            }
        } else {
            self.diagnostics.push(SyntaxError::expected(
                self.span(),
                &format!("an item, got `{}`", self.peek().lexeme),
            ));
            self.bump();
        }

        Ok(())
    }
}
