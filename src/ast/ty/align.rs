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
            Type::Pointer(..) | Type::MultiPointer(..) | Type::Function(..) => word_size,
            Type::Array(ty, ..) => ty.align_of(word_size),
            Type::Slice(..) => StructTy::temp(
                vec![
                    StructTyField::temp(Type::raw_pointer(false)),
                    StructTyField::temp(Type::Uint(UintTy::Uint)),
                ],
                StructTyKind::Struct,
            )
            .align_of(word_size),
            Type::Tuple(tys) => StructTy::temp(
                tys.iter().map(|t| StructTyField::temp(t.clone())).collect(),
                StructTyKind::Struct,
            )
            .align_of(word_size),
            Type::Struct(s) => s.align_of(word_size),
            Type::Infer(_, InferTy::AnyInt) => IntTy::Int.align_of(word_size),
            Type::Infer(_, InferTy::AnyFloat) => FloatTy::Float.align_of(word_size),
            ty => panic!("got unsized type: {:?}", ty),
        }
    }
}

impl AlignOf for IntTy {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            IntTy::I8 => 1,
            IntTy::I16 => 2,
            IntTy::I32 => 4,
            IntTy::I64 => 8,
            IntTy::Int => word_size as _,
        }
    }
}

impl AlignOf for UintTy {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            UintTy::U8 => 1,
            UintTy::U16 => 2,
            UintTy::U32 => 4,
            UintTy::U64 => 8,
            UintTy::Uint => word_size as _,
        }
    }
}

impl AlignOf for FloatTy {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            FloatTy::F16 => 2,
            FloatTy::F32 => 4,
            FloatTy::F64 => 8,
            FloatTy::Float => word_size as _,
        }
    }
}

impl AlignOf for StructTy {
    fn align_of(&self, word_size: usize) -> usize {
        match self.kind {
            StructTyKind::Struct | StructTyKind::Union => {
                let mut max_align: usize = 1;
                for field in self.fields.iter() {
                    let field_align = field.ty.align_of(word_size);
                    max_align = max_align.max(field_align);
                }
                max_align
            }
            StructTyKind::PackedStruct => 1,
        }
    }
}
