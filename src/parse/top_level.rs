use super::*;
use crate::ast::ast::{Ast, Visibility};
use crate::error::SyntaxError;

impl Parser {
    pub fn parse_top_level(&mut self, ast: &mut Ast) -> DiagnosticResult<()> {
        let visibility = if eat!(self, Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if eat!(self, Let) {
            let start_span = self.previous_span();

            let binding = if eat!(self, Extern) {
                self.parse_extern(visibility, start_span)?
            } else if eat!(self, Builtin) {
                self.parse_builtin_binding(visibility, start_span)?
            } else {
                self.parse_binding(visibility, true)?
            };

            ast.bindings.push(binding);

            Ok(())
        } else if eat!(self, Ident(_)) {
            let token = self.previous().clone();
            let symbol = token.symbol();

            require!(self, Bang, "!")?;
            require!(self, OpenParen, "(")?;

            if symbol == "run" {
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
