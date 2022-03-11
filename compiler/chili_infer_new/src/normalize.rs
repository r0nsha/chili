use crate::{constraint::Constraint, sess::InferSess, ty::TyVar};
use chili_ast::ty::*;

impl InferSess {
    pub(crate) fn normalize_ty(&mut self, ty: &TyKind) -> TyKind {
        self.normalize_ty_internal(ty, false, false)
    }

    pub(crate) fn normalize_ty_and_untyped(&mut self, ty: &TyKind) -> TyKind {
        self.normalize_ty_internal(ty, false, true)
    }

    pub(crate) fn normalize_ty_and_expand_types(&mut self, ty: &TyKind) -> TyKind {
        self.normalize_ty_internal(ty, true, false)
    }

    fn normalize_ty_internal(
        &mut self,
        ty: &TyKind,
        expand_types: bool,
        normalize_untyped: bool,
    ) -> TyKind {
        match ty {
            TyKind::Type(inner) => {
                if expand_types {
                    self.normalize_ty_internal(inner, expand_types, normalize_untyped)
                } else {
                    ty.clone()
                }
            }

            TyKind::Pointer(ty, is_mutable) => TyKind::Pointer(
                Box::new(self.normalize_ty_internal(ty, expand_types, normalize_untyped)),
                *is_mutable,
            ),
            TyKind::MultiPointer(ty, is_mutable) => TyKind::MultiPointer(
                Box::new(self.normalize_ty_internal(ty, expand_types, normalize_untyped)),
                *is_mutable,
            ),
            TyKind::Array(ty, size) => TyKind::Array(
                Box::new(self.normalize_ty_internal(ty, expand_types, normalize_untyped)),
                *size,
            ),
            TyKind::Slice(ty, is_mutable) => TyKind::Slice(
                Box::new(self.normalize_ty_internal(ty, expand_types, normalize_untyped)),
                *is_mutable,
            ),

            TyKind::Fn(ty) => {
                let mut params = vec![];

                for param in &ty.params {
                    params.push(FnTyParam {
                        symbol: param.symbol,
                        ty: self.normalize_ty_internal(&param.ty, expand_types, normalize_untyped),
                    });
                }

                let ret =
                    Box::new(self.normalize_ty_internal(&ty.ret, expand_types, normalize_untyped));

                TyKind::Fn(FnTy {
                    params,
                    ret,
                    variadic: ty.variadic,
                    lib_name: ty.lib_name,
                })
            }

            TyKind::Tuple(tys) => {
                let mut new_tys = vec![];

                for ty in tys {
                    new_tys.push(self.normalize_ty_internal(ty, expand_types, normalize_untyped));
                }

                TyKind::Tuple(new_tys)
            }

            TyKind::Struct(struct_ty) => {
                let mut new_fields = vec![];

                for field in &struct_ty.fields {
                    new_fields.push(StructTyField {
                        symbol: field.symbol,
                        ty: self.normalize_ty_internal(&field.ty, expand_types, normalize_untyped),
                        span: field.span,
                    });
                }

                TyKind::Struct(StructTy {
                    name: struct_ty.name,
                    qualified_name: struct_ty.qualified_name,
                    kind: struct_ty.kind,
                    fields: new_fields,
                })
            }

            TyKind::Var(var) => {
                let value = self.value_of(TyVar::from(*var));

                match value {
                    Constraint::Bound(ty) => {
                        self.normalize_ty_internal(&ty, expand_types, normalize_untyped)
                    }
                    Constraint::AnyInt | Constraint::Float => {
                        if normalize_untyped {
                            todo!()
                            // TyKind::from(value)
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
