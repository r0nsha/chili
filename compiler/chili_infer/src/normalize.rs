use crate::{inference_value::InferenceValue, ty_ctx::TyCtx};
use chili_ast::{ty::*, workspace::BindingInfoId};
use chili_span::Span;
use indexmap::IndexMap;
use ustr::ustr;

pub trait Normalize {
    fn normalize(&self, tycx: &TyCtx) -> TyKind;
}

impl Normalize for Ty {
    fn normalize(&self, tycx: &TyCtx) -> TyKind {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: false,
        }
        .normalize_ty(tycx, *self)
    }
}

impl Normalize for TyKind {
    fn normalize(&self, tycx: &TyCtx) -> TyKind {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: false,
        }
        .normalize_kind(tycx, self)
    }
}

pub trait Concrete {
    fn concrete(&self, tycx: &TyCtx) -> TyKind;
}

impl Concrete for Ty {
    fn concrete(&self, tycx: &TyCtx) -> TyKind {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: true,
        }
        .normalize_ty(tycx, *self)
    }
}

impl Concrete for TyKind {
    fn concrete(&self, tycx: &TyCtx) -> TyKind {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: true,
        }
        .normalize_kind(tycx, self)
    }
}

struct NormalizeCtx {
    parent_binding_info_id: BindingInfoId,
    concrete: bool,
}

impl NormalizeCtx {
    fn normalize_ty(&mut self, tycx: &TyCtx, ty: Ty) -> TyKind {
        match tycx.value_of(ty) {
            InferenceValue::Bound(kind) => self.normalize_kind(tycx, kind),
            InferenceValue::AnyInt => self.normalize_anyint(ty),
            InferenceValue::AnyFloat => self.normalize_anyfloat(ty),
            InferenceValue::PartialTuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|el| self.normalize_kind(tycx, el))
                    .collect::<Vec<TyKind>>();

                if self.concrete {
                    TyKind::Tuple(elements)
                } else {
                    TyKind::Infer(ty, InferTy::PartialTuple(elements))
                }
            }
            InferenceValue::PartialStruct(st) => {
                if self.concrete {
                    TyKind::Struct(StructTy {
                        name: ustr(""),
                        binding_info_id: BindingInfoId::unknown(),
                        fields: st
                            .iter()
                            .map(|(name, ty)| StructTyField {
                                symbol: *name,
                                ty: self.normalize_kind(tycx, ty),
                                span: Span::unknown(),
                            })
                            .collect(),
                        kind: StructTyKind::Struct,
                    })
                } else {
                    TyKind::Infer(
                        ty,
                        InferTy::PartialStruct(PartialStructTy(
                            st.iter()
                                .map(|(name, ty)| (*name, self.normalize_kind(tycx, ty)))
                                .collect(),
                        )),
                    )
                }
            }
            InferenceValue::Unbound => ty.as_kind(),
        }
    }

    fn normalize_kind(&mut self, tycx: &TyCtx, kind: &TyKind) -> TyKind {
        match kind {
            TyKind::Var(ty) => self.normalize_ty(tycx, *ty),
            TyKind::Function(f) => TyKind::Function(FunctionTy {
                params: f
                    .params
                    .iter()
                    .map(|p| self.normalize_kind(tycx, p))
                    .collect(),
                ret: Box::new(self.normalize_kind(tycx, &f.ret)),
                varargs: f.varargs.clone(),
                extern_lib: f.extern_lib.clone(),
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
            TyKind::Infer(ty, InferTy::AnyInt) => self.normalize_anyint(*ty),
            TyKind::Infer(ty, InferTy::AnyFloat) => self.normalize_anyfloat(*ty),
            TyKind::Infer(ty, InferTy::PartialTuple(elements)) => {
                let elements = elements
                    .iter()
                    .map(|el| self.normalize_kind(tycx, el))
                    .collect::<Vec<TyKind>>();

                if self.concrete {
                    TyKind::Tuple(elements)
                } else {
                    TyKind::Infer(
                        *ty,
                        InferTy::PartialTuple(
                            elements
                                .iter()
                                .map(|ty| self.normalize_kind(tycx, ty))
                                .collect(),
                        ),
                    )
                }
            }
            TyKind::Infer(ty, InferTy::PartialStruct(st)) => {
                if self.concrete {
                    TyKind::Struct(StructTy {
                        name: ustr(""),
                        binding_info_id: BindingInfoId::unknown(),
                        fields: st
                            .iter()
                            .map(|(name, ty)| StructTyField {
                                symbol: *name,
                                ty: self.normalize_kind(tycx, ty),
                                span: Span::unknown(),
                            })
                            .collect(),
                        kind: StructTyKind::Struct,
                    })
                } else {
                    TyKind::Infer(
                        *ty,
                        InferTy::PartialStruct(PartialStructTy(IndexMap::from_iter(
                            st.iter()
                                .map(|(symbol, ty)| (*symbol, self.normalize_kind(tycx, ty))),
                        ))),
                    )
                }
            }
            _ => kind.clone(),
        }
    }

    fn normalize_anyint(&self, ty: Ty) -> TyKind {
        if self.concrete {
            TyKind::Int(IntTy::Int)
        } else {
            TyKind::Infer(ty, InferTy::AnyInt)
        }
    }

    fn normalize_anyfloat(&self, ty: Ty) -> TyKind {
        if self.concrete {
            TyKind::Float(FloatTy::Float)
        } else {
            TyKind::Infer(ty, InferTy::AnyFloat)
        }
    }
}
