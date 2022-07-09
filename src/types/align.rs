use super::*;

pub trait AlignOf {
    fn align_of(&self, word_size: usize) -> usize;
}

impl AlignOf for Type {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Int(ty) => ty.align_of(word_size),
            Type::Uint(ty) => ty.align_of(word_size),
            Type::Float(ty) => ty.align_of(word_size),
            Type::Pointer(..) | Type::Function(..) => word_size,
            Type::Array(ty, ..) => ty.align_of(word_size),
            Type::Slice(..) => StructType::temp(
                vec![
                    StructTypeField::temp(Type::raw_pointer(false)),
                    StructTypeField::temp(Type::Uint(UintType::Uint)),
                ],
                StructTypeKind::Struct,
            )
            .align_of(word_size),
            Type::Tuple(tys) => StructType::temp(
                tys.iter()
                    .map(|t| StructTypeField::temp(t.clone()))
                    .collect(),
                StructTypeKind::Struct,
            )
            .align_of(word_size),
            Type::Struct(s) => s.align_of(word_size),
            Type::Infer(_, InferTy::AnyInt) => IntType::Int.align_of(word_size),
            Type::Infer(_, InferTy::AnyFloat) => FloatType::Float.align_of(word_size),
            ty => panic!("got unsized type: {:?}", ty),
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
