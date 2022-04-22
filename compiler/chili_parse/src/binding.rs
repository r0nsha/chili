use crate::*;
use chili_ast::{
    ast::{Binding, BindingKind, Visibility},
    pattern::Pattern,
    ty::Ty,
};
use chili_span::To;

impl<'p> Parser<'p> {
    pub(crate) fn parse_binding(
        &mut self,
        kind: BindingKind,
        visibility: Visibility,
        require_value: bool,
    ) -> DiagnosticResult<Binding> {
        let start_span = self.previous_span();

        match kind {
            BindingKind::Value | BindingKind::Import => {
                let pattern = self.parse_pattern()?;

                let ty_expr = if eat!(self, Colon) {
                    Some(self.parse_ty()?)
                } else {
                    None
                };

                if require_value {
                    expect!(self, Eq, "=")?;
                } else if !eat!(self, Eq) {
                    return Ok(Binding {
                        module_id: Default::default(),
                        visibility,
                        kind,
                        pattern,
                        ty: Ty::unknown(),
                        ty_expr: None,
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
                    kind,
                    pattern,
                    ty: Ty::unknown(),
                    ty_expr,
                    expr: Some(expr),
                    lib_name: None,
                    span: start_span.to(self.previous_span()),
                })
            }
            BindingKind::Type => {
                let pattern = self.parse_symbol_pattern()?;
                expect!(self, Eq, "=")?;
                let expr = self.parse_decl_ty(pattern.symbol)?;

                Ok(Binding {
                    module_id: Default::default(),
                    visibility,
                    kind,
                    pattern: Pattern::Single(pattern),
                    ty: Ty::unknown(),
                    ty_expr: None,
                    expr: Some(expr),
                    lib_name: None,
                    span: start_span.to(self.previous_span()),
                })
            }
        }
    }
}
