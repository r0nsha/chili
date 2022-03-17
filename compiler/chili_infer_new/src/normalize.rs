use crate::tycx::{TyBinding, TyContext};
use chili_ast::ty::*;

pub trait NormalizeTy {
    fn normalize(&self, tycx: &TyContext) -> TyKind;
}

impl NormalizeTy for Ty {
    fn normalize(&self, tycx: &TyContext) -> TyKind {
        match tycx.get_binding(*self) {
            TyBinding::Bound(ty) => ty.normalize(tycx),
            TyBinding::Unbound => TyKind::Var(*self),
        }
    }
}

impl NormalizeTy for TyKind {
    fn normalize(&self, tycx: &TyContext) -> TyKind {
        match self {
            TyKind::Var(var) => match tycx.get_binding(*var) {
                TyBinding::Bound(ty) => ty.normalize(tycx),
                TyBinding::Unbound => TyKind::Var(*var),
            },
            TyKind::Fn(f) => TyKind::Fn(FnTy {
                params: f
                    .params
                    .iter()
                    .map(|p| FnTyParam {
                        symbol: p.symbol,
                        ty: p.ty.normalize(tycx),
                    })
                    .collect(),
                ret: Box::new(f.ret.normalize(tycx)),
                variadic: f.variadic,
                lib_name: f.lib_name,
            }),
            TyKind::Pointer(ty, a) => TyKind::Pointer(Box::new(ty.normalize(tycx)), *a),
            TyKind::MultiPointer(ty, a) => TyKind::MultiPointer(Box::new(ty.normalize(tycx)), *a),
            TyKind::Array(ty, a) => TyKind::Array(Box::new(ty.normalize(tycx)), *a),
            TyKind::Slice(ty, a) => TyKind::Slice(Box::new(ty.normalize(tycx)), *a),
            TyKind::Tuple(tys) => TyKind::Tuple(tys.iter().map(|ty| ty.normalize(tycx)).collect()),
            TyKind::Struct(st) => TyKind::Struct(StructTy {
                name: st.name,
                qualified_name: st.qualified_name,
                binding_info_id: st.binding_info_id,
                fields: st
                    .fields
                    .iter()
                    .map(|f| StructTyField {
                        symbol: f.symbol,
                        ty: f.ty.normalize(tycx),
                        span: f.span,
                    })
                    .collect(),
                kind: st.kind,
            }),
            TyKind::AnyInt(var) => match tycx.get_binding(*var) {
                TyBinding::Bound(ty) => ty.normalize(tycx),
                TyBinding::Unbound => TyKind::AnyInt(*var),
            },
            TyKind::AnyFloat(var) => match tycx.get_binding(*var) {
                TyBinding::Bound(ty) => ty.normalize(tycx),
                TyBinding::Unbound => TyKind::AnyFloat(*var),
            },
            _ => self.clone(),
        }
    }
}
