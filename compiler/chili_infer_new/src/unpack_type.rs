use crate::tycx::{TyBinding, TyContext};
use chili_ast::ty::*;
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub(crate) fn try_unpack_type(
    ty: &TyKind,
    tycx: &TyContext,
    span: Span,
) -> DiagnosticResult<TyKind> {
    match ty {
        TyKind::Type(ty) => unpack_type(ty, tycx),
        _ => Err(Diagnostic::error()
            .with_message(format!("expected a type, but found {}", ty))
            .with_labels(vec![
                Label::primary(span.file_id, span.range().clone()).with_message("expected a type")
            ])),
    }
}

fn unpack_type(ty: &TyKind, tycx: &TyContext) -> DiagnosticResult<TyKind> {
    match ty {
        TyKind::Var(var) => match tycx.get_binding(*var) {
            TyBinding::Bound(ty) => unpack_type(&ty, tycx),
            TyBinding::Unbound => {
                panic!(
                    "couldn't figure out the type of {}, because it was unbound",
                    *var
                )
            }
        },
        TyKind::Fn(f) => Ok(TyKind::Fn(FnTy {
            params: f
                .params
                .iter()
                .map(|p| {
                    Ok(FnTyParam {
                        symbol: p.symbol,
                        ty: unpack_type(&p.ty, tycx)?,
                    })
                })
                .collect::<DiagnosticResult<Vec<FnTyParam>>>()?,
            ret: Box::new(unpack_type(&f.ret, tycx)?),
            variadic: f.variadic,
            lib_name: f.lib_name,
        })),
        TyKind::Pointer(ty, a) => Ok(TyKind::Pointer(Box::new(unpack_type(ty, tycx)?), *a)),
        TyKind::MultiPointer(ty, a) => {
            Ok(TyKind::MultiPointer(Box::new(unpack_type(ty, tycx)?), *a))
        }
        TyKind::Array(ty, a) => Ok(TyKind::Array(Box::new(unpack_type(ty, tycx)?), *a)),
        TyKind::Slice(ty, a) => Ok(TyKind::Slice(Box::new(unpack_type(ty, tycx)?), *a)),
        TyKind::Tuple(tys) => Ok(TyKind::Tuple(
            tys.iter()
                .map(|ty| unpack_type(&ty, tycx))
                .collect::<DiagnosticResult<Vec<TyKind>>>()?,
        )),
        TyKind::Struct(st) => Ok(TyKind::Struct(StructTy {
            name: st.name,
            qualified_name: st.qualified_name,
            binding_info_id: st.binding_info_id,
            fields: st
                .fields
                .iter()
                .map(|f| {
                    Ok(StructTyField {
                        symbol: f.symbol,
                        ty: unpack_type(&f.ty, tycx)?,
                        span: f.span,
                    })
                })
                .collect::<DiagnosticResult<Vec<StructTyField>>>()?,
            kind: st.kind,
        })),
        _ => Ok(ty.clone()),
    }
}
