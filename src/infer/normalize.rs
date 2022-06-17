use super::{inference_value::InferenceValue, ty_ctx::TyCtx};
use crate::ast::{ty::*, workspace::BindingInfoId};
use crate::span::Span;
use indexmap::IndexMap;
use ustr::ustr;

pub trait Normalize {
    fn normalize(&self, tycx: &TyCtx) -> Type;
}

impl Normalize for TypeId {
    fn normalize(&self, tycx: &TyCtx) -> Type {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: false,
        }
        .normalize_ty(tycx, *self)
    }
}

impl Normalize for Type {
    fn normalize(&self, tycx: &TyCtx) -> Type {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: false,
        }
        .normalize_kind(tycx, self)
    }
}

pub trait Concrete {
    fn concrete(&self, tycx: &TyCtx) -> Type;
}

impl Concrete for TypeId {
    fn concrete(&self, tycx: &TyCtx) -> Type {
        NormalizeCtx {
            parent_binding_info_id: Default::default(),
            concrete: true,
        }
        .normalize_ty(tycx, *self)
    }
}

impl Concrete for Type {
    fn concrete(&self, tycx: &TyCtx) -> Type {
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
    fn normalize_ty(&mut self, tycx: &TyCtx, ty: TypeId) -> Type {
        match tycx.value_of(ty) {
            InferenceValue::Bound(kind) => self.normalize_kind(tycx, kind),
            InferenceValue::AnyInt => self.normalize_anyint(ty),
            InferenceValue::AnyFloat => self.normalize_anyfloat(ty),
            InferenceValue::PartialTuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|el| self.normalize_kind(tycx, el))
                    .collect::<Vec<Type>>();

                if self.concrete {
                    Type::Tuple(elements)
                } else {
                    Type::Infer(ty, InferTy::PartialTuple(elements))
                }
            }
            InferenceValue::PartialStruct(st) => {
                if self.concrete {
                    Type::Struct(StructTy {
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
                    Type::Infer(
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

    fn normalize_kind(&mut self, tycx: &TyCtx, kind: &Type) -> Type {
        match kind {
            Type::Var(ty) => self.normalize_ty(tycx, *ty),
            Type::Function(f) => Type::Function(FunctionTy {
                params: f
                    .params
                    .iter()
                    .map(|p| self.normalize_kind(tycx, p))
                    .collect(),
                ret: Box::new(self.normalize_kind(tycx, &f.ret)),
                varargs: f.varargs.as_ref().map(|v| {
                    Box::new(FunctionTyVarargs {
                        ty: v.ty.as_ref().map(|ty| self.normalize_kind(tycx, ty)),
                    })
                }),
                extern_lib: f.extern_lib.clone(),
            }),
            Type::Pointer(inner, a) => {
                Type::Pointer(Box::new(self.normalize_kind(tycx, inner)), *a)
            }
            Type::MultiPointer(inner, a) => {
                Type::MultiPointer(Box::new(self.normalize_kind(tycx, inner)), *a)
            }
            Type::Array(inner, a) => Type::Array(Box::new(self.normalize_kind(tycx, inner)), *a),
            Type::Slice(inner, a) => Type::Slice(Box::new(self.normalize_kind(tycx, inner)), *a),
            Type::Tuple(tys) => Type::Tuple(
                tys.iter()
                    .map(|kind| self.normalize_kind(tycx, kind))
                    .collect(),
            ),
            Type::Struct(st) => {
                if st.binding_info_id != Default::default()
                    && st.binding_info_id == self.parent_binding_info_id
                {
                    kind.clone()
                } else {
                    let old_id = self.parent_binding_info_id;
                    self.parent_binding_info_id = st.binding_info_id;

                    let st = Type::Struct(StructTy {
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
            Type::Type(inner) => self.normalize_kind(tycx, inner).create_type(),
            Type::Infer(ty, InferTy::AnyInt) => self.normalize_anyint(*ty),
            Type::Infer(ty, InferTy::AnyFloat) => self.normalize_anyfloat(*ty),
            Type::Infer(ty, InferTy::PartialTuple(elements)) => {
                let elements = elements
                    .iter()
                    .map(|el| self.normalize_kind(tycx, el))
                    .collect::<Vec<Type>>();

                if self.concrete {
                    Type::Tuple(elements)
                } else {
                    Type::Infer(
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
            Type::Infer(ty, InferTy::PartialStruct(st)) => {
                if self.concrete {
                    Type::Struct(StructTy {
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
                    Type::Infer(
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

    fn normalize_anyint(&self, ty: TypeId) -> Type {
        if self.concrete {
            Type::Int(IntTy::Int)
        } else {
            Type::Infer(ty, InferTy::AnyInt)
        }
    }

    fn normalize_anyfloat(&self, ty: TypeId) -> Type {
        if self.concrete {
            Type::Float(FloatTy::Float)
        } else {
            Type::Infer(ty, InferTy::AnyFloat)
        }
    }
}
