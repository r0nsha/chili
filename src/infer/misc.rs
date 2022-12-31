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
            f.params.iter().try_for_each(|p| is_concrete_impl(&p.ty))?;

            is_concrete_impl(&f.return_type)?;

            if let Some(Err(ty)) = f
                .varargs
                .as_ref()
                .map(|v| &v.ty)
                .and_then(|ty| ty.as_ref().map(is_concrete_impl))
            {
                return Err(ty);
            }

            Ok(())
        }
        Type::Tuple(elems) => elems.iter().try_for_each(is_concrete_impl),
        Type::Struct(st) => st.fields.iter().try_for_each(|f| is_concrete_impl(&f.ty)),
        Type::Module(_) | Type::Type(_) | Type::AnyType => Ok(()),
        Type::Var(ty) | Type::Infer(ty, _) => Err(*ty),
    }
}
