use super::{inference_value::InferenceValue, type_ctx::TypeCtx};
use crate::{span::Span, types::*, workspace::BindingId};
use indexmap::IndexMap;
use ustr::ustr;

pub trait Normalize {
    fn normalize(&self, tcx: &TypeCtx) -> Type;
}

impl Normalize for TypeId {
    fn normalize(&self, tcx: &TypeCtx) -> Type {
        NormalizeCtx {
            parent_binding_id: Default::default(),
            concrete: false,
        }
        .normalize_ty(tcx, *self)
    }
}

impl Normalize for Type {
    fn normalize(&self, tcx: &TypeCtx) -> Type {
        NormalizeCtx {
            parent_binding_id: Default::default(),
            concrete: false,
        }
        .normalize_kind(tcx, self)
    }
}

pub trait Concrete {
    fn concrete(&self, tcx: &TypeCtx) -> Type;
}

impl Concrete for TypeId {
    fn concrete(&self, tcx: &TypeCtx) -> Type {
        NormalizeCtx {
            parent_binding_id: Default::default(),
            concrete: true,
        }
        .normalize_ty(tcx, *self)
    }
}

impl Concrete for Type {
    fn concrete(&self, tcx: &TypeCtx) -> Type {
        NormalizeCtx {
            parent_binding_id: Default::default(),
            concrete: true,
        }
        .normalize_kind(tcx, self)
    }
}

struct NormalizeCtx {
    parent_binding_id: BindingId,
    concrete: bool,
}

impl NormalizeCtx {
    fn normalize_ty(&mut self, tcx: &TypeCtx, ty: TypeId) -> Type {
        match tcx.value_of(ty) {
            InferenceValue::Bound(kind) => self.normalize_kind(tcx, kind),
            InferenceValue::AnyInt => self.normalize_anyint(ty),
            InferenceValue::AnyFloat => self.normalize_anyfloat(ty),
            InferenceValue::PartialTuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|el| self.normalize_kind(tcx, el))
                    .collect::<Vec<Type>>();

                if self.concrete {
                    Type::Tuple(elements)
                } else {
                    Type::Infer(ty, InferType::PartialTuple(elements))
                }
            }
            InferenceValue::PartialStruct(st) => {
                if self.concrete {
                    Type::Struct(StructType {
                        name: ustr(""),
                        binding_id: BindingId::unknown(),
                        fields: st
                            .iter()
                            .map(|(name, ty)| StructTypeField {
                                name: *name,
                                ty: self.normalize_kind(tcx, ty),
                                span: Span::unknown(),
                            })
                            .collect(),
                        kind: StructTypeKind::Struct,
                    })
                } else {
                    Type::Infer(
                        ty,
                        InferType::PartialStruct(PartialStructType(
                            st.iter()
                                .map(|(name, ty)| (*name, self.normalize_kind(tcx, ty)))
                                .collect(),
                        )),
                    )
                }
            }
            InferenceValue::Unbound => ty.as_kind(),
        }
    }

    fn normalize_kind(&mut self, tcx: &TypeCtx, kind: &Type) -> Type {
        match kind {
            Type::Var(ty) => self.normalize_ty(tcx, *ty),
            Type::Function(f) => Type::Function(FunctionType {
                params: f
                    .params
                    .iter()
                    .map(|p| FunctionTypeParam {
                        name: p.name,
                        ty: self.normalize_kind(tcx, &p.ty),
                        default_value: p.default_value.clone(),
                    })
                    .collect(),
                return_type: Box::new(self.normalize_kind(tcx, &f.return_type)),
                varargs: f.varargs.as_ref().map(|v| {
                    Box::new(FunctionTypeVarargs {
                        name: v.name,
                        ty: v.ty.as_ref().map(|ty| self.normalize_kind(tcx, ty)),
                    })
                }),
                kind: f.kind.clone(),
            }),
            Type::Pointer(inner, a) => Type::Pointer(Box::new(self.normalize_kind(tcx, inner)), *a),
            Type::Array(inner, a) => Type::Array(Box::new(self.normalize_kind(tcx, inner)), *a),
            Type::Slice(inner) => Type::Slice(Box::new(self.normalize_kind(tcx, inner))),
            Type::Tuple(tys) => Type::Tuple(tys.iter().map(|kind| self.normalize_kind(tcx, kind)).collect()),
            Type::Struct(st) => {
                if st.binding_id != Default::default() && st.binding_id == self.parent_binding_id {
                    kind.clone()
                } else {
                    let old_id = self.parent_binding_id;
                    self.parent_binding_id = st.binding_id;

                    let st = Type::Struct(StructType {
                        name: st.name,
                        binding_id: st.binding_id,
                        fields: st
                            .fields
                            .iter()
                            .map(|f| StructTypeField {
                                name: f.name,
                                ty: self.normalize_kind(tcx, &f.ty),
                                span: f.span,
                            })
                            .collect(),
                        kind: st.kind,
                    });

                    self.parent_binding_id = old_id;

                    st
                }
            }
            Type::Type(inner) => self.normalize_kind(tcx, inner).create_type(),
            Type::Infer(ty, InferType::AnyInt) => self.normalize_anyint(*ty),
            Type::Infer(ty, InferType::AnyFloat) => self.normalize_anyfloat(*ty),
            Type::Infer(ty, InferType::PartialTuple(elements)) => {
                let elements = elements
                    .iter()
                    .map(|el| self.normalize_kind(tcx, el))
                    .collect::<Vec<Type>>();

                if self.concrete {
                    Type::Tuple(elements)
                } else {
                    Type::Infer(
                        *ty,
                        InferType::PartialTuple(elements.iter().map(|ty| self.normalize_kind(tcx, ty)).collect()),
                    )
                }
            }
            Type::Infer(ty, InferType::PartialStruct(st)) => {
                if self.concrete {
                    Type::Struct(StructType {
                        name: ustr(""),
                        binding_id: BindingId::unknown(),
                        fields: st
                            .iter()
                            .map(|(name, ty)| StructTypeField {
                                name: *name,
                                ty: self.normalize_kind(tcx, ty),
                                span: Span::unknown(),
                            })
                            .collect(),
                        kind: StructTypeKind::Struct,
                    })
                } else {
                    Type::Infer(
                        *ty,
                        InferType::PartialStruct(PartialStructType(IndexMap::from_iter(
                            st.iter().map(|(symbol, ty)| (*symbol, self.normalize_kind(tcx, ty))),
                        ))),
                    )
                }
            }
            _ => kind.clone(),
        }
    }

    fn normalize_anyint(&self, ty: TypeId) -> Type {
        if self.concrete {
            Type::int()
        } else {
            Type::Infer(ty, InferType::AnyInt)
        }
    }

    fn normalize_anyfloat(&self, ty: TypeId) -> Type {
        if self.concrete {
            Type::float()
        } else {
            Type::Infer(ty, InferType::AnyFloat)
        }
    }
}
