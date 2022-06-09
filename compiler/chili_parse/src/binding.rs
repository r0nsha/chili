use crate::*;
use chili_ast::{
    ast::{Binding, BindingKind, Visibility},
    pattern::Pattern,
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
                extern_lib: None,
                span: start_span.to(self.previous_span()),
            });
        }

        let expr = match &pattern {
            Pattern::Symbol(pattern) => self.parse_decl_expr(pattern.symbol)?,
            _ => self.parse_expr()?,
        };

        Ok(Binding {
            module_id: Default::default(),
            visibility,
            kind: BindingKind::Value,
            pattern,
            ty: Ty::unknown(),
            ty_expr,
            expr: Some(expr),
            extern_lib: None,
            span: start_span.to(self.previous_span()),
        })
    }
}
