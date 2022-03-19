use crate::{normalize::NormalizeTy, tycx::TyCtx, unify::UnifyTyErr};
use chili_ast::ty::TyKind;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn map_unify_err(
    e: UnifyTyErr,
    expected: impl NormalizeTy,
    found: impl NormalizeTy,
    span: Span,
    tycx: &TyCtx,
) -> Diagnostic<usize> {
    use UnifyTyErr::*;

    let expected = expected.normalize(tycx).display(tycx);
    let found = found.normalize(tycx).display(tycx);

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

impl DisplayTy for TyKind {
    fn display(&self, tycx: &TyCtx) -> String {
        // TODO: this is bad, because i can't know what to display for Var and Struct
        self.to_string()
        // match self {
        //     Ty::Never => todo!(),
        //     Ty::Unit => todo!(),
        //     Ty::Bool => todo!(),
        //     Ty::Int(_) => todo!(),
        //     Ty::UInt(_) => todo!(),
        //     Ty::Float(_) => todo!(),
        //     Ty::Pointer(_, _) => todo!(),
        //     Ty::MultiPointer(_, _) => todo!(),
        //     Ty::Fn(_) => todo!(),
        //     Ty::Array(_, _) => todo!(),
        //     Ty::Slice(_, _) => todo!(),
        //     Ty::Tuple(_) => todo!(),
        //     Ty::Struct(_) => todo!(),
        //     Ty::Module(_) => todo!(),
        //     Ty::Type(_) => todo!(),
        //     Ty::Var(_) => todo!(),
        //     Ty::AnyInt => todo!(),
        //     Ty::AnyFloat => todo!(),
        //     Ty::Unknown => todo!(),
        // }
    }
}
