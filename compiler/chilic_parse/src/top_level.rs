use crate::*;
use chilic_ast::ast::{Ast, EntityKind, Visibility};
use chilic_error::{DiagnosticResult, SyntaxError};

impl Parser {
    pub(crate) fn parse_top_level(
        &mut self,
        ast: &mut Ast,
    ) -> DiagnosticResult<()> {
        self.skip_redundant_tokens();

        let visibility = if match_token!(self, Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if match_token!(self, Use) {
            let uses = self.parse_use(visibility)?;
            ast.uses.extend(uses);
        } else if match_token!(self, Type) {
            let entity =
                self.parse_entity(EntityKind::Type, visibility, true)?;

            ast.entities.push(entity);
        } else if match_token!(self, Let) {
            let entity = if match_token!(self, Foreign) {
                self.parse_foreign_single(visibility)?
            } else {
                self.parse_entity(EntityKind::Value, visibility, true)?
            };

            ast.entities.push(entity);
        } else if match_token!(self, Foreign) {
            let entities = self.parse_foreign_block()?;
            ast.entities.extend(entities);
        } else {
            return Err(SyntaxError::expected(
                self.span(),
                &format!("an item, got `{}`", self.peek().lexeme),
            ));
        }

        Ok(())
    }
}
