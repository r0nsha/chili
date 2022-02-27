use crate::*;
use chilic_ast::ast::{Ast, EntityKind, Visibility};
use chilic_error::{DiagnosticResult, SyntaxError};

impl Parser {
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
            let uses = self.parse_use(visibility)?;
            ast.uses.extend(uses);
        } else if eat!(self, Type) {
            let entity =
                self.parse_entity(EntityKind::Type, visibility, true)?;

            ast.entities.push(entity);
        } else if eat!(self, Let) {
            let entity = if eat!(self, Foreign) {
                self.parse_foreign_single(visibility)?
            } else {
                self.parse_entity(EntityKind::Value, visibility, true)?
            };

            ast.entities.push(entity);
        } else if eat!(self, Foreign) {
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
