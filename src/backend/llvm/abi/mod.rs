mod amd64_system_v;
mod amd64_win64;
mod i386;

use crate::common::{
    mem::{bit_width_to_size, calculate_align_from_offset},
    target::{Arch, Os, TargetMetrics},
};
use inkwell::{
    attributes::Attribute,
    context::Context,
    types::{AnyType, AnyTypeEnum, BasicTypeEnum, FunctionType},
};

pub(super) fn get_abi_compliant_fn<'ctx>(
    context: &'ctx Context,
    target_metrics: &TargetMetrics,
    fn_ty: FunctionType<'ctx>,
) -> AbiFunction<'ctx> {
    let info = AbiInfo {
        context,
        word_size: target_metrics.word_size,
    };

    match &target_metrics.arch {
        Arch::Amd64 => match &target_metrics.os {
            Os::Windows => amd64_win64::get_fn(info, fn_ty),
            Os::Linux => amd64_system_v::get_fn(info, fn_ty),
            os => unimplemented!("{}", os.name()),
        },
        arch => unimplemented!("{}", arch.name()),
    }
}

pub(super) fn size_of<'ctx>(llvm_type: BasicTypeEnum<'ctx>, word_size: usize) -> usize {
    size_of_any(llvm_type.as_any_type_enum(), word_size)
}

pub(super) fn size_of_any<'ctx>(llvm_type: AnyTypeEnum<'ctx>, word_size: usize) -> usize {
    match llvm_type {
        AnyTypeEnum::VoidType(_) => 0,
        AnyTypeEnum::IntType(t) => bit_width_to_size(t.get_bit_width()),
        AnyTypeEnum::FloatType(t) => bit_width_to_size(t.get_bit_width()),
        AnyTypeEnum::PointerType(_) => word_size,
        AnyTypeEnum::ArrayType(t) => {
            let el_ty = t.get_element_type();
            let el_size = size_of(el_ty, word_size);
            let len = t.len();
            el_size * len as usize
        }
        AnyTypeEnum::StructType(t) => {
            let fields = t.get_field_types();
            if t.is_packed() {
                fields.iter().map(|f| size_of(*f, word_size)).sum::<usize>()
            } else {
                let mut offset = 0;
                for field in fields {
                    let align = align_of(field, word_size);
                    offset = calculate_align_from_offset(offset, align);
                    offset += size_of(field, word_size);
                }
                offset = calculate_align_from_offset(offset, align_of_any(llvm_type, word_size));
                offset
            }
        }
        AnyTypeEnum::VectorType(_) => todo!("size of vector type"),
        _ => panic!("got unsized type: {:?}", llvm_type),
    }
}

pub(super) fn align_of<'ctx>(llvm_type: BasicTypeEnum<'ctx>, word_size: usize) -> usize {
    align_of_any(llvm_type.as_any_type_enum(), word_size)
}

pub(super) fn align_of_any<'ctx>(llvm_type: AnyTypeEnum<'ctx>, word_size: usize) -> usize {
    match llvm_type {
        AnyTypeEnum::VoidType(_) => 1,
        AnyTypeEnum::IntType(t) => {
            let size = bit_width_to_size(t.get_bit_width());
            size.clamp(1, word_size)
        }
        AnyTypeEnum::FloatType(t) => bit_width_to_size(t.get_bit_width()),
        AnyTypeEnum::PointerType(_) => word_size,
        AnyTypeEnum::ArrayType(t) => align_of(t.get_element_type(), word_size),
        AnyTypeEnum::StructType(t) => {
            if t.is_packed() {
                1
            } else {
                let fields = t.get_field_types();
                let mut max_align: usize = 1;
                for field in fields {
                    let field_align = align_of(field, word_size);
                    max_align = max_align.max(field_align);
                }
                max_align
            }
        }
        AnyTypeEnum::VectorType(_) => todo!("size of vector type"),
        _ => panic!("got unsized type: {:?}", llvm_type),
    }
}

#[derive(Clone, Copy)]
pub struct AbiInfo<'ctx> {
    pub context: &'ctx Context,
    pub word_size: usize,
}

#[derive(Debug, Clone)]
pub struct AbiFunction<'ctx> {
    pub params: Vec<AbiTy<'ctx>>,
    pub ret: AbiTy<'ctx>,
    pub variadic: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct AbiTy<'ctx> {
    pub ty: BasicTypeEnum<'ctx>,
    pub kind: AbiType,
    pub cast_to: Option<BasicTypeEnum<'ctx>>,
    pub attr: Option<Attribute>,
    pub align_attr: Option<Attribute>,
}

impl<'ctx> AbiTy<'ctx> {
    pub(super) fn direct(ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            ty,
            kind: AbiType::Direct,
            cast_to: None,
            attr: None,
            align_attr: None,
        }
    }

    pub(super) fn indirect(ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            ty,
            kind: AbiType::Indirect,
            cast_to: None,
            attr: None,
            align_attr: None,
        }
    }

    pub(super) fn ignore(ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            ty,
            kind: AbiType::Ignore,
            cast_to: None,
            attr: None,
            align_attr: None,
        }
    }

    pub(super) fn indirect_byval(context: &Context, ty: BasicTypeEnum<'ctx>, word_size: usize) -> Self {
        Self {
            ty,
            kind: AbiType::Indirect,
            cast_to: None,
            attr: Some(
                context.create_type_attribute(Attribute::get_named_enum_kind_id("byval"), ty.as_any_type_enum()),
            ),
            align_attr: Some(context.create_enum_attribute(
                Attribute::get_named_enum_kind_id("align"),
                align_of(ty, word_size).max(8) as u64,
            )),
        }
    }

    pub(super) fn with_cast_to<'a>(&'a mut self, cast_to: BasicTypeEnum<'ctx>) -> &'a mut Self {
        self.cast_to = Some(cast_to);
        self
    }

    pub(super) fn with_attr<'a>(&'a mut self, attr: Attribute) -> &'a mut Self {
        self.attr = Some(attr);
        self
    }

    #[allow(unused)]
    pub(super) fn with_align_attr<'a>(&'a mut self, attr: Attribute) -> &'a mut Self {
        self.align_attr = Some(attr);
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AbiType {
    Direct,
    Indirect,
    Ignore,
}

impl AbiType {
    #[allow(unused)]
    pub(super) fn is_direct(&self) -> bool {
        match self {
            AbiType::Direct => true,
            _ => false,
        }
    }

    pub(super) fn is_indirect(&self) -> bool {
        match self {
            AbiType::Indirect => true,
            _ => false,
        }
    }

    pub(super) fn is_ignore(&self) -> bool {
        match self {
            AbiType::Ignore => true,
            _ => false,
        }
    }
}
