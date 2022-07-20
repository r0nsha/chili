use super::*;

pub trait IsSized {
    fn is_sized(&self) -> bool;

    fn is_unsized(&self) -> bool {
        !self.is_sized()
    }
}

impl IsSized for Type {
    fn is_sized(&self) -> bool {
        match self {
            Type::Unit
            | Type::Never
            | Type::Bool
            | Type::Int(_)
            | Type::Uint(_)
            | Type::Float(_)
            | Type::Pointer(..)
            | Type::Function(..)
            | Type::Array(..)
            | Type::Infer(_, InferType::AnyInt)
            | Type::Infer(_, InferType::AnyFloat) => true,

            Type::Module(_) | Type::Type(_) | Type::AnyType | Type::Var(_) | Type::Slice(..) => {
                false
            }

            Type::Infer(_, InferType::PartialTuple(elems)) | Type::Tuple(elems) => {
                elems.iter().all(|e| e.is_sized())
            }

            Type::Struct(s) => s.fields.iter().all(|f| f.ty.is_sized()),

            Type::Infer(_, InferType::PartialStruct(s)) => s.iter().all(|(_, ty)| ty.is_sized()),
        }
    }
}
