use super::{
    normalize::Normalize,
    type_ctx::TypeCtx,
    unify::{UnifyTypeErr, UnifyTypeResult},
};
use crate::error::DiagnosticResult;
use crate::span::Span;

pub trait DisplayTy {
    fn display(&self, tcx: &TypeCtx) -> String;
}

impl<T: Normalize> DisplayTy for T {
    fn display(&self, tcx: &TypeCtx) -> String {
        self.normalize(tcx).to_string()
    }
}

pub trait OrReportErr {
    fn or_report_err(
        self,
        tcx: &TypeCtx,
        expected: impl DisplayTy,
        expected_span: Option<Span>,
        found: impl DisplayTy,
        found_span: Span,
    ) -> DiagnosticResult<()>;
}

impl OrReportErr for UnifyTypeResult {
    fn or_report_err(
        self,
        tcx: &TypeCtx,
        expected: impl DisplayTy,
        expected_span: Option<Span>,
        found: impl DisplayTy,
        found_span: Span,
    ) -> DiagnosticResult<()> {
        self.map_err(|e| {
            UnifyTypeErr::into_diagnostic(e, tcx, expected, expected_span, found, found_span)
        })
    }
}
