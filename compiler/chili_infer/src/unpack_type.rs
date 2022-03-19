use crate::{
    display::DisplayTy,
    normalize::NormalizeTy,
    tycx::{TyBinding, TyCtx},
};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;

pub(crate) fn try_unpack_type(ty: &TyKind, tycx: &TyCtx, span: Span) -> DiagnosticResult<TyKind> {
    match ty {
        TyKind::Type(ty) => try_unpack_type_inner(ty, tycx, span),
        _ => Err(TypeError::expected(
            span,
            ty.normalize(tycx).display(tycx),
            "a type",
        )),
    }
}

fn try_unpack_type_inner(ty: &TyKind, tycx: &TyCtx, span: Span) -> DiagnosticResult<TyKind> {
    match ty {
        TyKind::Var(var) => match tycx.get_binding(*var) {
            TyBinding::Bound(ty) => try_unpack_type(&ty, tycx, span),
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
                        ty: try_unpack_type(&p.ty, tycx, span)?,
                    })
                })
                .collect::<DiagnosticResult<Vec<FnTyParam>>>()?,
            ret: Box::new(try_unpack_type(&f.ret, tycx, span)?),
            variadic: f.variadic,
            lib_name: f.lib_name,
        })),
        TyKind::Pointer(ty, a) => Ok(TyKind::Pointer(
            Box::new(try_unpack_type(ty, tycx, span)?),
            *a,
        )),
        TyKind::MultiPointer(ty, a) => Ok(TyKind::MultiPointer(
            Box::new(try_unpack_type(ty, tycx, span)?),
            *a,
        )),
        TyKind::Array(ty, a) => Ok(TyKind::Array(
            Box::new(try_unpack_type(ty, tycx, span)?),
            *a,
        )),
        TyKind::Slice(ty, a) => Ok(TyKind::Slice(
            Box::new(try_unpack_type(ty, tycx, span)?),
            *a,
        )),
        TyKind::Tuple(tys) => Ok(TyKind::Tuple(
            tys.iter()
                .map(|ty| try_unpack_type(&ty, tycx, span))
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
                        ty: try_unpack_type(&f.ty, tycx, span)?,
                        span: f.span,
                    })
                })
                .collect::<DiagnosticResult<Vec<StructTyField>>>()?,
            kind: st.kind,
        })),
        _ => Ok(ty.clone()),
    }
}

pub(crate) fn unpack_type(ty: &TyKind, tycx: &TyCtx) -> TyKind {
    match ty {
        TyKind::Type(ty) => unpack_type_inner(ty, tycx),
        _ => ty.clone(),
    }
}

fn unpack_type_inner(ty: &TyKind, tycx: &TyCtx) -> TyKind {
    match ty {
        TyKind::Var(var) => match tycx.get_binding(*var) {
            TyBinding::Bound(ty) => unpack_type(&ty, tycx),
            TyBinding::Unbound => TyKind::Var(*var),
        },
        TyKind::Fn(f) => TyKind::Fn(FnTy {
            params: f
                .params
                .iter()
                .map(|p| FnTyParam {
                    symbol: p.symbol,
                    ty: unpack_type(&p.ty, tycx),
                })
                .collect::<Vec<FnTyParam>>(),
            ret: Box::new(unpack_type(&f.ret, tycx)),
            variadic: f.variadic,
            lib_name: f.lib_name,
        }),
        TyKind::Pointer(ty, a) => TyKind::Pointer(Box::new(unpack_type(ty, tycx)), *a),
        TyKind::MultiPointer(ty, a) => TyKind::MultiPointer(Box::new(unpack_type(ty, tycx)), *a),
        TyKind::Array(ty, a) => TyKind::Array(Box::new(unpack_type(ty, tycx)), *a),
        TyKind::Slice(ty, a) => TyKind::Slice(Box::new(unpack_type(ty, tycx)), *a),
        TyKind::Tuple(tys) => TyKind::Tuple(
            tys.iter()
                .map(|ty| unpack_type(&ty, tycx))
                .collect::<Vec<TyKind>>(),
        ),
        TyKind::Struct(st) => TyKind::Struct(StructTy {
            name: st.name,
            qualified_name: st.qualified_name,
            binding_info_id: st.binding_info_id,
            fields: st
                .fields
                .iter()
                .map(|f| StructTyField {
                    symbol: f.symbol,
                    ty: unpack_type(&f.ty, tycx),
                    span: f.span,
                })
                .collect::<Vec<StructTyField>>(),
            kind: st.kind,
        }),
        _ => ty.clone(),
    }
}
