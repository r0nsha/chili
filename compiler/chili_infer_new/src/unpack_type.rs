use chili_ast::ty::*;
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::tyctx::{TyBinding, TyContext};

pub(crate) fn try_unpack_type(ty: &Ty, ctx: &TyContext) -> DiagnosticResult<Ty> {
    match ty {
        Ty::Type(ty) => unpack_type(ty, ctx),
        _ => {
            // TODO: use the real span of the ty here!
            let span = Span::unknown();
            Err(Diagnostic::error()
                .with_message(format!("expected a type, but found {}", ty))
                .with_labels(vec![Label::primary(span.file_id, span.range().clone())
                    .with_message("expected a type")]))
        }
    }
}

fn unpack_type(ty: &Ty, ctx: &TyContext) -> DiagnosticResult<Ty> {
    match ty {
        Ty::Var(var) => match ctx.find_type_binding(*var) {
            TyBinding::Bound(ty) => unpack_type(&ty, ctx),
            TyBinding::Unbound => {
                panic!(
                    "couldn't figure out the type of {}, because it was unbound",
                    *var
                )
            }
        },
        Ty::Fn(f) => Ok(Ty::Fn(FnTy {
            params: f
                .params
                .iter()
                .map(|p| {
                    Ok(FnTyParam {
                        symbol: p.symbol,
                        ty: unpack_type(&p.ty, ctx)?,
                    })
                })
                .collect::<DiagnosticResult<Vec<FnTyParam>>>()?,
            ret: Box::new(unpack_type(&f.ret, ctx)?),
            variadic: f.variadic,
            lib_name: f.lib_name,
        })),
        Ty::Pointer(ty, a) => Ok(Ty::Pointer(Box::new(unpack_type(ty, ctx)?), *a)),
        Ty::MultiPointer(ty, a) => Ok(Ty::MultiPointer(Box::new(unpack_type(ty, ctx)?), *a)),
        Ty::Array(ty, a) => Ok(Ty::Array(Box::new(unpack_type(ty, ctx)?), *a)),
        Ty::Slice(ty, a) => Ok(Ty::Slice(Box::new(unpack_type(ty, ctx)?), *a)),
        Ty::Tuple(tys) => Ok(Ty::Tuple(
            tys.iter()
                .map(|ty| unpack_type(&ty, ctx))
                .collect::<DiagnosticResult<Vec<Ty>>>()?,
        )),
        Ty::Struct(st) => Ok(Ty::Struct(StructTy {
            name: st.name,
            qualified_name: st.qualified_name,
            binding_info_id: st.binding_info_id,
            fields: st
                .fields
                .iter()
                .map(|f| {
                    Ok(StructTyField {
                        symbol: f.symbol,
                        ty: unpack_type(&f.ty, ctx)?,
                        span: f.span,
                    })
                })
                .collect::<DiagnosticResult<Vec<StructTyField>>>()?,
            kind: st.kind,
        })),
        _ => Ok(ty.clone()),
    }
}
