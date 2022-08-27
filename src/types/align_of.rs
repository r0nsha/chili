use super::*;

pub trait AlignOf {
    fn align_of(&self, word_size: usize) -> usize;
}

impl AlignOf for Type {
    #[track_caller]
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            Type::Unit | Type::Never | Type::Bool => 1,
            Type::Int(ty) => ty.align_of(word_size),
            Type::Uint(ty) => ty.align_of(word_size),
            Type::Float(ty) => ty.align_of(word_size),
            Type::Pointer(..) | Type::Function(..) => word_size,
            Type::Array(ty, ..) => ty.align_of(word_size),
            Type::Tuple(elems) => StructType::temp(
                elems.iter().map(|t| StructTypeField::temp(t.clone())).collect(),
                StructTypeKind::Struct,
            )
            .align_of(word_size),
            Type::Struct(s) => s.align_of(word_size),
            Type::Infer(_, InferType::PartialStruct(partial_struct)) => {
                let mut max_align: usize = 1;
                for (_, field) in partial_struct.iter() {
                    let field_align = field.align_of(word_size);
                    max_align = max_align.max(field_align);
                }
                max_align
            }
            Type::Infer(_, InferType::AnyInt) => IntType::Int.align_of(word_size),
            Type::Infer(_, InferType::AnyFloat) => FloatType::Float.align_of(word_size),
            _ => panic!("type {:?} is unsized", self),
        }
    }
}

impl AlignOf for IntType {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            IntType::I8 => 1,
            IntType::I16 => 2,
            IntType::I32 => 4,
            IntType::I64 => 8,
            IntType::Int => word_size as _,
        }
    }
}

impl AlignOf for UintType {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            UintType::U8 => 1,
            UintType::U16 => 2,
            UintType::U32 => 4,
            UintType::U64 => 8,
            UintType::Uint => word_size as _,
        }
    }
}

impl AlignOf for FloatType {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            FloatType::F16 => 2,
            FloatType::F32 => 4,
            FloatType::F64 => 8,
            FloatType::Float => word_size as _,
        }
    }
}

impl AlignOf for StructType {
    fn align_of(&self, word_size: usize) -> usize {
        match self.kind {
            StructTypeKind::Struct | StructTypeKind::Union => {
                let mut max_align: usize = 1;
                for field in self.fields.iter() {
                    let field_align = field.ty.align_of(word_size);
                    max_align = max_align.max(field_align);
                }
                max_align
            }
            StructTypeKind::PackedStruct => 1,
        }
    }
}
