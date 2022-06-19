use crate::common::mem::calculate_align_from_offset;

use super::{align::AlignOf, *};

pub trait SizeOf {
    fn size_of(&self, word_size: usize) -> usize;
}

impl SizeOf for Type {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Int(ty) => ty.size_of(word_size),
            Type::Uint(ty) => ty.size_of(word_size),
            Type::Float(ty) => ty.size_of(word_size),
            Type::Pointer(..) | Type::MultiPointer(..) | Type::Function(..) => word_size,
            Type::Array(ty, len) => ty.size_of(word_size) * len,
            Type::Slice(..) => StructType::temp(
                vec![
                    StructTypeField::temp(Type::raw_pointer(false)),
                    StructTypeField::temp(Type::Uint(UintType::Uint)),
                ],
                StructTypeKind::Struct,
            )
            .size_of(word_size),
            Type::Tuple(tys) => StructType::temp(
                tys.iter()
                    .map(|t| StructTypeField::temp(t.clone()))
                    .collect(),
                StructTypeKind::Struct,
            )
            .size_of(word_size),
            Type::Struct(s) => s.size_of(word_size),
            Type::Infer(_, InferTy::AnyInt) => IntType::Int.size_of(word_size),
            Type::Infer(_, InferTy::AnyFloat) => FloatType::Float.size_of(word_size),
            ty => panic!("got unsized type: {:?}", ty),
        }
    }
}

impl SizeOf for IntType {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            IntType::I8 => 1,
            IntType::I16 => 2,
            IntType::I32 => 4,
            IntType::I64 => 8,
            IntType::Int => word_size as _,
        }
    }
}

impl SizeOf for UintType {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            UintType::U8 => 1,
            UintType::U16 => 2,
            UintType::U32 => 4,
            UintType::U64 => 8,
            UintType::Uint => word_size as _,
        }
    }
}

impl SizeOf for FloatType {
    fn size_of(&self, word_size: usize) -> usize {
        match self {
            FloatType::F16 => 2,
            FloatType::F32 => 4,
            FloatType::F64 => 8,
            FloatType::Float => word_size as _,
        }
    }
}

impl SizeOf for StructType {
    fn size_of(&self, word_size: usize) -> usize {
        match self.kind {
            StructTypeKind::Struct => {
                let mut offset = 0;

                for field in self.fields.iter() {
                    let align = field.ty.align_of(word_size);
                    offset = calculate_align_from_offset(offset, align);
                    offset += field.ty.size_of(word_size);
                }

                offset = calculate_align_from_offset(offset, self.align_of(word_size));
                offset
            }
            StructTypeKind::PackedStruct => {
                self.fields.iter().map(|f| f.ty.size_of(word_size)).sum()
            }
            StructTypeKind::Union => {
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