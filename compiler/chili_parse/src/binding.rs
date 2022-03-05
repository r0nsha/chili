use crate::*;
use chili_ast::{
    ast::{Binding, BindingKind, Visibility},
    pattern::Pattern,
};

impl<'w> Parser<'w> {
    pub(crate) fn parse_binding(
        &mut self,
        kind: BindingKind,
        visibility: Visibility,
        require_value: bool,
    ) -> DiagnosticResult<Binding> {
        match kind {
            BindingKind::Value => {
                let pattern = self.parse_pattern()?;

                let ty_expr = if eat!(self, Colon) {
                    Some(self.parse_ty()?)
                } else {
                    None
                };

                if require_value {
                    expect!(self, Eq, "=")?;
                } else if !eat!(self, Eq) {
                    return Ok(Binding::new(
                        visibility, kind, pattern, ty_expr, None, None,
                    ));
                }

                let value = if pattern.is_single() {
                    self.parse_decl_expr(pattern.into_single().symbol)?
                } else {
                    self.parse_expr()?
                };

                Ok(Binding::new(
                    visibility,
                    kind,
                    pattern,
                    ty_expr,
                    Some(value),
                    None,
                ))
            }
            BindingKind::Type => {
                let pattern = self.parse_symbol_pattern()?;
                expect!(self, Eq, "=")?;
                let value = self.parse_decl_ty(pattern.symbol)?;

                Ok(Binding::new(
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
