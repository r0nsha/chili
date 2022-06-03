use crate::*;
use chili_ast::{
    ast::{Binding, BindingKind, Visibility},
    ty::Ty,
};
use chili_span::To;

impl Parser {
    pub(crate) fn parse_binding(
        &mut self,
        visibility: Visibility,
        require_value: bool,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        let pattern = self.parse_pattern()?;

        let ty_expr = if eat!(self, Colon) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if require_value {
            require!(self, Eq, "=")?;
        } else if !eat!(self, Eq) {
            return Ok(Binding {
                module_id: Default::default(),
                visibility,
                kind: BindingKind::Value,
                pattern,
                ty: Ty::unknown(),
                ty_expr,
                expr: None,
                lib_name: None,
                span: start_span.to(self.previous_span()),
            });
        }

        let expr = if pattern.is_single() {
            self.parse_decl_expr(pattern.into_single().symbol)?
        } else {
            self.parse_expr()?
        };

        Ok(Binding {
            module_id: Default::default(),
            visibility,
            kind: BindingKind::Value,
            pattern,
            ty: Ty::unknown(),
            ty_expr,
            expr: Some(expr),
            lib_name: None,
            span: start_span.to(self.previous_span()),
        })
    }
}
