use crate::*;
use chili_ast::ast::{Ast, Visibility};
use chili_error::SyntaxError;

impl Parser {
    pub(crate) fn parse_top_level(&mut self, ast: &mut Ast) -> DiagnosticResult<()> {
        self.skip_trailing_semicolons();

        let visibility = if eat!(self, Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if eat!(self, Use) {
            let imports = self.parse_import(visibility)?;
            ast.imports.extend(imports);

            Ok(())
        } else if eat!(self, Let) {
            let binding = if eat!(self, Foreign) {
                self.parse_extern_single(visibility)
            } else {
                self.parse_binding(visibility, true)
            }?;

            ast.bindings.push(binding);

            Ok(())
        } else if eat!(self, Foreign) {
            let bindings = self.parse_extern_block()?;
            ast.bindings.extend(bindings);

            Ok(())
        } else if eat!(self, At) {
            let token = require!(self, Ident(_), "ident")?;
            let symbol = token.symbol();

            if symbol == "run" {
                require!(self, OpenParen, "(")?;
                let expr = self.parse_expr()?;
                ast.run_exprs.push(expr);
                require!(self, CloseParen, ")")?;

                Ok(())
            } else {
                Err(SyntaxError::expected(self.previous_span(), "run"))
            }
        } else {
            Err(SyntaxError::expected(
                self.span(),
                &format!("an item, got `{}`", self.peek().lexeme),
            ))
        }
    }
}
