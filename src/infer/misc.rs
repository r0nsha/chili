use crate::types::{Type, TypeId};

use super::{normalize::Normalize, type_ctx::TypeCtx};

pub trait IsConcrete {
    fn is_concrete(&self, tcx: &TypeCtx) -> Result<(), TypeId>;
}

impl<T: Normalize> IsConcrete for T {
    fn is_concrete(&self, tcx: &TypeCtx) -> Result<(), TypeId> {
        is_concrete_impl(&self.normalize(tcx))
    }
}

fn is_concrete_impl(ty: &Type) -> Result<(), TypeId> {
    match ty {
        Type::Never | Type::Unit | Type::Bool | Type::Int(_) | Type::Uint(_) | Type::Float(_) => Ok(()),
        Type::Array(inner, _) | Type::Slice(inner) | Type::Str(inner) | Type::Pointer(inner, _) => {
            is_concrete_impl(inner)
        }
        Type::Function(f) => {
            f.params
                .iter()
                .map(|p| is_concrete_impl(&p.ty))
                .collect::<Result<_, _>>()?;

            is_concrete_impl(&f.return_type)?;

            if let Some(Err(ty)) = f
                .varargs
                .as_ref()
                .map(|v| &v.ty)
                .map(|ty| ty.as_ref().map(|ty| is_concrete_impl(ty)))
                .flatten()
            {
                return Err(ty);
            }

            Ok(())
        }
        Type::Tuple(elems) => elems.iter().map(is_concrete_impl).collect::<Result<_, _>>(),
        Type::Struct(st) => st
            .fields
            .iter()
            .map(|f| is_concrete_impl(&f.ty))
            .collect::<Result<_, _>>(),
        Type::Module(_) | Type::Type(_) | Type::AnyType => Ok(()),
        Type::Var(ty) | Type::Infer(ty, _) => Err(*ty),
    }
}
