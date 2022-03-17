use super::*;

pub trait AlignOf {
    fn align_of(&self, word_size: usize) -> usize;
}

impl AlignOf for TyKind {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            TyKind::Unit => 0,
            TyKind::Bool => 1,
            TyKind::Int(ty) => ty.align_of(word_size),
            TyKind::UInt(ty) => ty.align_of(word_size),
            TyKind::Float(ty) => ty.align_of(word_size),
            TyKind::Pointer(..) | TyKind::MultiPointer(..) | TyKind::Fn(..) => word_size,
            TyKind::Array(ty, ..) => ty.align_of(word_size),
            TyKind::Slice(..) => StructTy::temp(
                vec![
                    StructTyField::temp(TyKind::raw_pointer(false)),
                    StructTyField::temp(TyKind::UInt(UIntTy::Usize)),
                ],
                StructTyKind::Struct,
            )
            .align_of(word_size),
            TyKind::Tuple(tys) => StructTy::temp(
                tys.iter().map(|t| StructTyField::temp(t.clone())).collect(),
                StructTyKind::Struct,
            )
            .align_of(word_size),
            TyKind::Struct(s) => s.align_of(word_size),
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
            IntTy::Isize => word_size as _,
        }
    }
}

impl AlignOf for UIntTy {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            UIntTy::U8 => 1,
            UIntTy::U16 => 2,
            UIntTy::U32 => 4,
            UIntTy::U64 => 8,
            UIntTy::Usize => word_size as _,
        }
    }
}

impl AlignOf for FloatTy {
    fn align_of(&self, word_size: usize) -> usize {
        match self {
            FloatTy::F16 => 2,
            FloatTy::F32 => 4,
            FloatTy::F64 => 8,
            FloatTy::Fsize => word_size as _,
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
