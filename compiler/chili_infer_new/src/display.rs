use chili_ast::{ty::Ty, workspace::Workspace};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{tyctx::TyContext, unify::TyUnifyErr};

pub(crate) fn map_unify_err(e: TyUnifyErr, span: Span) -> Diagnostic<usize> {
    use TyUnifyErr::*;

    match e {
        Mismatch(expected, found) => Diagnostic::error()
            .with_message(format!(
                "mismatched types - expected {}, but found {}",
                expected, found
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range().clone())
                .with_message(format!("expected {}", expected))]),
        Occurs(expected, found) => Diagnostic::error()
            .with_message(format!(
                "found recursive type - {} is equal to {}",
                expected, found
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range().clone())
                .with_message(format!("expected {}", expected))]),
    }
}

trait DisplayTy {
    fn display(&self, ctx: &TyContext, workspace: &Workspace) -> String;
}

impl DisplayTy for Ty {
    fn display(&self, ctx: &TyContext, workspace: &Workspace) -> String {
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
