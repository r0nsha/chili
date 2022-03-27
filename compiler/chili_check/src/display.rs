use crate::{normalize::NormalizeTy, ty_ctx::TyCtx, unify::UnifyTyErr};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub(crate) fn map_unify_err(
    e: UnifyTyErr,
    expected: impl NormalizeTy,
    found: impl NormalizeTy,
    span: Span,
    tycx: &TyCtx,
) -> Diagnostic<usize> {
    use UnifyTyErr::*;

    let expected = expected.display(tycx);
    let found = found.display(tycx);

    match e {
        Mismatch => Diagnostic::error()
            .with_message(format!(
                "mismatched types - expected {}, but found {}",
                expected, found
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range().clone())
                .with_message(format!("expected {}", expected))]),
        Occurs => Diagnostic::error()
            .with_message(format!(
                "found recursive type - {} is equal to {}",
                expected, found
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range().clone())
                .with_message(format!("expected {}", expected))]),
    }
}

pub trait DisplayTy {
    fn display(&self, tycx: &TyCtx) -> String;
}

impl<T: NormalizeTy> DisplayTy for T {
    fn display(&self, tycx: &TyCtx) -> String {
        self.normalize(tycx).to_string()
    }
}
