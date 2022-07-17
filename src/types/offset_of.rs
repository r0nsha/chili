use crate::common::mem::calculate_align_from_offset;

use super::{align_of::AlignOf, size_of::SizeOf, *};

pub trait OffsetOf {
    fn offset_of(&self, index: usize, word_size: usize) -> usize;
}

impl OffsetOf for Type {
    fn offset_of(&self, index: usize, word_size: usize) -> usize {
        match self {
            Type::Array(ty, ..) => ty.size_of(word_size) * index,
            Type::Slice(..) => match index {
                0 => 0,
                1 => word_size,
                _ => panic!("{}", index),
            },
            Type::Infer(_, InferType::PartialTuple(elems)) | Type::Tuple(elems) => {
                StructType::temp(
                    elems
                        .iter()
                        .map(|t| StructTypeField::temp(t.clone()))
                        .collect(),
                    StructTypeKind::Struct,
                )
                .offset_of(index, word_size)
            }
            Type::Struct(s) => s.offset_of(index, word_size),
            Type::Infer(_, InferType::PartialStruct(partial_struct)) => {
                let mut offset = 0;

                partial_struct.iter().take(index).for_each(|(_, field)| {
                    let align = field.align_of(word_size);
                    offset = calculate_align_from_offset(offset, align);
                    offset += field.size_of(word_size);
                });

                offset
            }
            ty => panic!("{} isn't an aggregate type", ty),
        }
    }
}

impl OffsetOf for StructType {
    fn offset_of(&self, index: usize, word_size: usize) -> usize {
        match self.kind {
            StructTypeKind::Struct => {
                let mut offset = 0;

                self.fields.iter().take(index).for_each(|field| {
                    let align = field.ty.align_of(word_size);
                    offset = calculate_align_from_offset(offset, align);
                    offset += field.ty.size_of(word_size);
                });

                offset
            }
            StructTypeKind::PackedStruct => self
                .fields
                .iter()
                .take(index)
                .map(|f| f.ty.size_of(word_size))
                .sum(),
            StructTypeKind::Union => 0,
        }
    }
}
