use common::mem::calculate_align_from_offset;

use super::{align::AlignOf, *};

pub trait SizeOf {
    fn size_of(&self, word_size: usize) -> usize;
}

impl SizeOf for TyKind {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            TyKind::Unit => 0,
            TyKind::Bool => 1,
            TyKind::Int(ty) => ty.size_of(word_size),
            TyKind::UInt(ty) => ty.size_of(word_size),
            TyKind::Float(ty) => ty.size_of(word_size),
            TyKind::Pointer(..) | TyKind::MultiPointer(..) | TyKind::Fn(..) => word_size,
            TyKind::Array(ty, len) => ty.size_of(word_size) * len,
            TyKind::Slice(..) => StructTy::temp(
                vec![
                    StructTyField::temp(TyKind::raw_pointer(false)),
                    StructTyField::temp(TyKind::UInt(UIntTy::Usize)),
                ],
                StructTyKind::Struct,
            )
            .size_of(word_size),
            TyKind::Tuple(tys) => StructTy::temp(
                tys.iter().map(|t| StructTyField::temp(t.clone())).collect(),
                StructTyKind::Struct,
            )
            .size_of(word_size),
            TyKind::Struct(s) => s.size_of(word_size),
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
