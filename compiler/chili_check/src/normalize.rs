use crate::ty_ctx::{InferenceValue, TyCtx};
use chili_ast::{ty::*, workspace::BindingInfoId};

pub trait NormalizeTy {
    fn normalize(&self, tycx: &TyCtx) -> TyKind;
}

impl NormalizeTy for Ty {
    fn normalize(&self, tycx: &TyCtx) -> TyKind {
        Normalize {
            parent_binding_info_id: Default::default(),
        }
        .normalize_ty(tycx, *self)
    }
}

impl NormalizeTy for TyKind {
    fn normalize(&self, tycx: &TyCtx) -> TyKind {
        Normalize {
            parent_binding_info_id: Default::default(),
        }
        .normalize_kind(tycx, self)
    }
}

struct Normalize {
    parent_binding_info_id: BindingInfoId,
}

impl Normalize {
    fn normalize_ty(&mut self, tycx: &TyCtx, ty: Ty) -> TyKind {
        match tycx.value_of(ty) {
            InferenceValue::Bound(kind) => self.normalize_kind(tycx, kind),
            InferenceValue::AnyInt => TyKind::AnyInt,
            InferenceValue::AnyFloat => TyKind::AnyFloat,
            InferenceValue::Unbound => TyKind::Var(ty),
        }
    }

    fn normalize_kind(&mut self, tycx: &TyCtx, kind: &TyKind) -> TyKind {
        match kind {
            TyKind::Var(ty) => self.normalize_ty(tycx, *ty),
            TyKind::Fn(f) => TyKind::Fn(FnTy {
                params: f
                    .params
                    .iter()
                    .map(|p| FnTyParam {
                        symbol: p.symbol,
                        ty: self.normalize_kind(tycx, &p.ty),
                    })
                    .collect(),
                ret: Box::new(f.ret.normalize(tycx)),
                variadic: f.variadic,
                lib_name: f.lib_name,
            }),
            TyKind::Pointer(inner, a) => {
                TyKind::Pointer(Box::new(self.normalize_kind(tycx, inner)), *a)
            }
            TyKind::MultiPointer(inner, a) => {
                TyKind::MultiPointer(Box::new(self.normalize_kind(tycx, inner)), *a)
            }
            TyKind::Array(inner, a) => {
                TyKind::Array(Box::new(self.normalize_kind(tycx, inner)), *a)
            }
            TyKind::Slice(inner, a) => {
                TyKind::Slice(Box::new(self.normalize_kind(tycx, inner)), *a)
            }
            TyKind::Tuple(tys) => TyKind::Tuple(
                tys.iter()
                    .map(|kind| self.normalize_kind(tycx, kind))
                    .collect(),
            ),
            TyKind::Struct(st) => {
                if st.binding_info_id != Default::default()
                    && st.binding_info_id == self.parent_binding_info_id
                {
                    kind.clone()
                } else {
                    let old_id = self.parent_binding_info_id;
                    self.parent_binding_info_id = st.binding_info_id;

                    let st = TyKind::Struct(StructTy {
                        name: st.name,
                        qualified_name: st.qualified_name,
                        binding_info_id: st.binding_info_id,
                        fields: st
                            .fields
                            .iter()
                            .map(|f| StructTyField {
                                symbol: f.symbol,
                                ty: self.normalize_kind(tycx, &f.ty),
                                span: f.span,
                            })
                            .collect(),
                        kind: st.kind,
                    });

                    self.parent_binding_info_id = old_id;

                    st
                }
            }
            TyKind::Type(inner) => self.normalize_kind(tycx, inner).create_type(),
            _ => kind.clone(),
        }
    }
}
