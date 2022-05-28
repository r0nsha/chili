use crate::{
    normalize::NormalizeTy,
    ty_ctx::TyCtx,
    unify::{UnifyTyErr, UnifyTyResult},
};
use chili_error::DiagnosticResult;
use chili_span::Span;

pub trait DisplayTy {
    fn display(&self, tycx: &TyCtx) -> String;
}

impl<T: NormalizeTy> DisplayTy for T {
    fn display(&self, tycx: &TyCtx) -> String {
        self.normalize(tycx).to_string()
    }
}

pub trait OrReportErr {
    fn or_report_err(
        self,
        tycx: &TyCtx,
        expected: impl DisplayTy,
        expected_span: Option<Span>,
        found: impl DisplayTy,
        found_span: Span,
    ) -> DiagnosticResult<()>;
}

impl OrReportErr for UnifyTyResult {
    fn or_report_err(
        self,
        tycx: &TyCtx,
        expected: impl DisplayTy,
        expected_span: Option<Span>,
        found: impl DisplayTy,
        found_span: Span,
    ) -> DiagnosticResult<()> {
        self.map_err(|e| {
            UnifyTyErr::into_diagnostic(e, tycx, expected, expected_span, found, found_span)
        })
    }
}
