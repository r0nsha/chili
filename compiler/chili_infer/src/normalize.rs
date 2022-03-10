use crate::infer::{InferenceContext, InferenceValue, TyVar};
use chili_ty::*;

impl InferenceContext {
    pub fn normalize_ty(&mut self, ty: &Ty) -> Ty {
        self.normalize_ty_internal(ty, false, false)
    }

    pub fn normalize_ty_and_untyped(&mut self, ty: &Ty) -> Ty {
        self.normalize_ty_internal(ty, false, true)
    }

    pub fn normalize_ty_and_expand_types(&mut self, ty: &Ty) -> Ty {
        self.normalize_ty_internal(ty, true, false)
    }

    fn normalize_ty_internal(
        &mut self,
        ty: &Ty,
        expand_types: bool,
        normalize_untyped: bool,
    ) -> Ty {
        match ty {
            Ty::Type(inner) => {
                if expand_types {
                    self.normalize_ty_internal(
                        inner,
                        expand_types,
                        normalize_untyped,
                    )
                } else {
                    ty.clone()
                }
            }

            Ty::Pointer(ty, is_mutable) => Ty::Pointer(
                Box::new(self.normalize_ty_internal(
                    ty,
                    expand_types,
                    normalize_untyped,
                )),
                *is_mutable,
            ),
            Ty::MultiPointer(ty, is_mutable) => Ty::MultiPointer(
                Box::new(self.normalize_ty_internal(
                    ty,
                    expand_types,
                    normalize_untyped,
                )),
                *is_mutable,
            ),
            Ty::Array(ty, size) => Ty::Array(
                Box::new(self.normalize_ty_internal(
                    ty,
                    expand_types,
                    normalize_untyped,
                )),
                *size,
            ),
            Ty::Slice(ty, is_mutable) => Ty::Slice(
                Box::new(self.normalize_ty_internal(
                    ty,
                    expand_types,
                    normalize_untyped,
                )),
                *is_mutable,
            ),

            Ty::Fn(ty) => {
                let mut params = vec![];

                for param in &ty.params {
                    params.push(FnTyParam {
                        symbol: param.symbol,
                        ty: self.normalize_ty_internal(
                            &param.ty,
                            expand_types,
                            normalize_untyped,
                        ),
                    });
                }

                let ret = Box::new(self.normalize_ty_internal(
                    &ty.ret,
                    expand_types,
                    normalize_untyped,
                ));

                Ty::Fn(FnTy {
                    params,
                    ret,
                    variadic: ty.variadic,
                    lib_name: ty.lib_name,
                })
            }

            Ty::Tuple(tys) => {
                let mut new_tys = vec![];

                for ty in tys {
                    new_tys.push(self.normalize_ty_internal(
                        ty,
                        expand_types,
                        normalize_untyped,
                    ));
                }

                Ty::Tuple(new_tys)
            }

            Ty::Struct(struct_ty) => {
                let mut new_fields = vec![];

                for field in &struct_ty.fields {
                    new_fields.push(StructTyField {
                        symbol: field.symbol,
                        ty: self.normalize_ty_internal(
                            &field.ty,
                            expand_types,
                            normalize_untyped,
                        ),
                        span: field.span,
                    });
                }

                Ty::Struct(StructTy {
                    name: struct_ty.name,
                    qualified_name: struct_ty.qualified_name,
                    kind: struct_ty.kind,
                    fields: new_fields,
                })
            }

            Ty::Var(var) => {
                let value = self.value_of(TyVar::from(*var));

                match value {
                    InferenceValue::Bound(ty) => self.normalize_ty_internal(
                        &ty,
                        expand_types,
                        normalize_untyped,
                    ),
                    InferenceValue::UntypedInt
                    | InferenceValue::UntypedFloat => {
                        if normalize_untyped {
                            Ty::from(value)
                        } else {
                            ty.clone()
                        }
                    }
                    _ => ty.clone(),
                }
            }

            ty => ty.clone(),
        }
    }
}
