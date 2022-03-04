use crate::{align::AlignOf, *};

pub trait SizeOf {
    fn size_of(&self, word_size: usize) -> usize;
}

impl SizeOf for Ty {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            Ty::Unit => 0,
            Ty::Bool => 1,
            Ty::Int(ty) => ty.size_of(word_size),
            Ty::UInt(ty) => ty.size_of(word_size),
            Ty::Float(ty) => ty.size_of(word_size),
            Ty::Pointer(..) | Ty::MultiPointer(..) | Ty::Fn(..) => word_size,
            Ty::Array(ty, len) => ty.size_of(word_size) * len,
            Ty::Slice(..) => StructTy::temp(
                vec![
                    StructTyField::temp(Ty::raw_pointer(false)),
                    StructTyField::temp(Ty::UInt(UIntTy::Usize)),
                ],
                StructTyKind::Struct,
            )
            .size_of(word_size),
            Ty::Tuple(tys) => StructTy::temp(
                tys.iter().map(|t| StructTyField::temp(t.clone())).collect(),
                StructTyKind::Struct,
            )
            .size_of(word_size),
            Ty::Struct(s) => s.size_of(word_size),
            ty => panic!("got unsized type: {:?}", ty),
        }
    }
}

impl SizeOf for IntTy {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            IntTy::I8 => 1,
            IntTy::I16 => 2,
            IntTy::I32 => 4,
            IntTy::I64 => 8,
            IntTy::Isize => word_size as _,
        }
    }
}

impl SizeOf for UIntTy {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            UIntTy::U8 => 1,
            UIntTy::U16 => 2,
            UIntTy::U32 => 4,
            UIntTy::U64 => 8,
            UIntTy::Usize => word_size as _,
        }
    }
}

impl SizeOf for FloatTy {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            FloatTy::F16 => 2,
            FloatTy::F32 => 4,
            FloatTy::F64 => 8,
            FloatTy::Fsize => word_size as _,
        }
    }
}

impl SizeOf for StructTy {
    fn size_of(&self, word_size: usize) -> usize {
        match self.kind {
            StructTyKind::Struct => {
                let mut offset = 0;
                for field in self.fields.iter() {
                    let align = field.ty.align_of(word_size);
                    offset = calculate_align_from_offset(offset, align);
                    offset += field.ty.size_of(word_size);
                }
                offset = calculate_align_from_offset(
                    offset,
                    self.align_of(word_size),
                );
                offset
            }
            StructTyKind::PackedStruct => {
                self.fields.iter().map(|f| f.ty.size_of(word_size)).sum()
            }
            StructTyKind::Union => {
                let max_size = self
                    .fields
                    .iter()
                    .map(|f| f.ty.size_of(word_size))
                    .max()
                    .unwrap_or(0);

                let align = self.align_of(word_size);

                (max_size / align) * align
            }
        }
    }
}
