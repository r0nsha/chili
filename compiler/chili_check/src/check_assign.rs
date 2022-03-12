use crate::{
    CheckedExpr, {CheckFrame, CheckSess},
};
use chili_ast::ast::{Expr, ExprKind};
use chili_ast::ty::*;
use chili_error::DiagnosticResult;
use chili_span::Span;

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_assign_expr(
        &mut self,
        frame: &mut CheckFrame,
        lvalue: &Expr,
        rvalue: &Expr,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let lvalue = self.check_expr(frame, lvalue, None)?;
        let mut rvalue = self.check_expr(frame, rvalue, Some(lvalue.ty.clone()))?;

        self.infcx
            .unify_or_coerce_ty_expr(&lvalue.ty, &mut rvalue.expr)?;

        Ok(CheckedExpr::new(
            ExprKind::Assign {
                lvalue: Box::new(lvalue.expr),
                rvalue: Box::new(rvalue.expr),
            },
            TyKind::Unit,
            None,
            span,
        ))
    }
}
