use crate::*;
use chilic_ast::{
    ast::{Entity, EntityKind, Visibility},
    pattern::Pattern,
};

impl Parser {
    pub(crate) fn parse_entity(
        &mut self,
        kind: EntityKind,
        visibility: Visibility,
        require_value: bool,
    ) -> DiagnosticResult<Entity> {
        match kind {
            EntityKind::Value => {
                let pattern = self.parse_pattern()?;

                let ty_expr = if eat!(self, Colon) {
                    Some(self.parse_ty()?)
                } else {
                    None
                };

                if require_value {
                    expect!(self, Eq, "=")?;
                } else if !eat!(self, Eq) {
                    return Ok(Entity::new(
                        visibility, kind, pattern, ty_expr, None, None,
                    ));
                }

                let value = if pattern.is_single() {
                    self.parse_decl_expr(pattern.into_single().symbol)?
                } else {
                    self.parse_expr()?
                };

                Ok(Entity::new(
                    visibility,
                    kind,
                    pattern,
                    ty_expr,
                    Some(value),
                    None,
                ))
            }
            EntityKind::Type => {
                let pattern = self.parse_symbol_pattern()?;
                expect!(self, Eq, "=")?;
                let value = self.parse_decl_ty(pattern.symbol)?;

                Ok(Entity::new(
                    visibility,
                    kind,
                    Pattern::Single(pattern),
                    None,
                    Some(value),
                    None,
                ))
            }
        }
    }
}
