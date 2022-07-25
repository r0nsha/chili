use super::{align_of, size_of, AbiFunction, AbiInfo, AbiTy};
use crate::common::mem::calculate_align_from_offset;
use inkwell::{
    attributes::Attribute,
    types::{AnyType, BasicTypeEnum, FunctionType},
};

pub(super) fn get_fn<'ctx>(info: AbiInfo<'ctx>, fn_ty: FunctionType<'ctx>) -> AbiFunction<'ctx> {
    AbiFunction {
        params: get_params(info, fn_ty.get_param_types()),
        ret: get_return(info, fn_ty.get_return_type().unwrap()),
        variadic: fn_ty.is_var_arg(),
    }
}

pub(super) fn get_params<'ctx>(info: AbiInfo<'ctx>, params: Vec<BasicTypeEnum<'ctx>>) -> Vec<AbiTy<'ctx>> {
    params
        .iter()
        .map(|&param| amd64_sysv_type(info, param, Amd64TypeAttributeKind::ByVal))
        .collect()
}

pub(super) fn get_return<'ctx>(info: AbiInfo<'ctx>, ret_ty: BasicTypeEnum<'ctx>) -> AbiTy<'ctx> {
    amd64_sysv_type(info, ret_ty, Amd64TypeAttributeKind::StructRect)
}

#[allow(unused)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum RegClass {
    NoClass,
    Int,
    SSEFs,
    SSEFv,
    SSEDs,
    SSEDv,
    SSEInt8,
    SSEInt16,
    SSEInt32,
    SSEInt64,
    SSEUp,
    X87,
    X87Up,
    ComplexX87,
    Memory,
}

impl RegClass {
    fn is_sse(&self) -> bool {
        match self {
            RegClass::SSEFs | RegClass::SSEFv | RegClass::SSEDs | RegClass::SSEDv => true,
            RegClass::SSEInt8 | RegClass::SSEInt16 | RegClass::SSEInt32 | RegClass::SSEInt64 => true,
            _ => false,
        }
    }

    fn all_mem(classes: &mut [RegClass]) {
        classes.iter_mut().for_each(|c| *c = RegClass::Memory);
    }
}

#[allow(unused)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Amd64TypeAttributeKind {
    None,
    ByVal,
    StructRect,
}

fn is_mem_class(classes: &[RegClass], attr_kind: Amd64TypeAttributeKind) -> bool {
    match attr_kind {
        Amd64TypeAttributeKind::ByVal => classes
            .first()
            .map(|class| matches!(class, RegClass::Memory | RegClass::X87 | RegClass::ComplexX87))
            .unwrap_or(false),
        Amd64TypeAttributeKind::StructRect => classes
            .first()
            .map(|class| matches!(class, RegClass::Memory))
            .unwrap_or(false),
        _ => false,
    }
}

fn is_register<'ctx>(ty: &BasicTypeEnum<'ctx>) -> bool {
    matches!(
        ty,
        BasicTypeEnum::IntType(_) | BasicTypeEnum::FloatType(_) | BasicTypeEnum::PointerType(_)
    )
}

fn amd64_sysv_type<'ctx>(
    info: AbiInfo<'ctx>,
    ty: BasicTypeEnum<'ctx>,
    attr_kind: Amd64TypeAttributeKind,
) -> AbiTy<'ctx> {
    if is_register(&ty) {
        non_struct(info, ty)
    } else {
        let classes = classify_type(info, ty);

        if is_mem_class(&classes, attr_kind) {
            if attr_kind == Amd64TypeAttributeKind::ByVal {
                AbiTy::indirect_byval(&info.context, ty, info.word_size)
            } else if attr_kind == Amd64TypeAttributeKind::StructRect {
                *AbiTy::indirect(ty).with_attr(
                    info.context
                        .create_type_attribute(Attribute::get_named_enum_kind_id("sret"), ty.as_any_type_enum()),
                )
            } else {
                AbiTy::indirect(ty)
            }
        } else {
            *AbiTy::direct(ty).with_cast_to(classes_to_type(info, &classes))
        }
    }
}

pub(super) fn non_struct<'ctx>(info: AbiInfo<'ctx>, ty: BasicTypeEnum<'ctx>) -> AbiTy<'ctx> {
    let mut abi_ty = AbiTy::direct(ty);

    if ty.is_int_type() && ty.into_int_type().get_bit_width() == 1 {
        abi_ty.attr = Some(
            info.context
                .create_enum_attribute(Attribute::get_named_enum_kind_id("zeroext"), 0),
        );
    }

    abi_ty
}

fn classify_type<'ctx>(info: AbiInfo<'ctx>, ty: BasicTypeEnum<'ctx>) -> Vec<RegClass> {
    let size = size_of(ty, info.word_size);
    let align = align_of(ty, info.word_size);

    let words = (size + 7) / 8;
    let mut classes = vec![RegClass::NoClass; words];

    if words > 4 {
        RegClass::all_mem(&mut classes);
    } else {
        classify_type_with_classes(info, &mut classes, ty, size, align, 0, 0);
        fixup_classes(&mut classes, ty);
    }

    classes
}

fn classify_type_with_classes<'ctx>(
    info: AbiInfo<'ctx>,
    classes: &mut [RegClass],
    ty: BasicTypeEnum<'ctx>,
    ty_size: usize,
    ty_align: usize,
    index: usize,
    offset: usize,
) {
    let misalign = offset % ty_align;

    if misalign > 0 {
        let end = (offset + ty_size + 7) / 8;
        for i in offset / 8..end {
            unify_classes(classes, index + i, RegClass::Memory);
        }
        return;
    }

    match ty {
        BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => {
            unify_classes(classes, index + offset / 8, RegClass::Int);
        }
        BasicTypeEnum::FloatType(ft) if ft.get_bit_width() == 16 => {
            unify_classes(classes, index + offset / 8, RegClass::Int);
        }
        BasicTypeEnum::FloatType(ft) if ft.get_bit_width() == 32 => {
            unify_classes(
                classes,
                index + offset / 8,
                if offset % 8 == 4 {
                    RegClass::SSEFv
                } else {
                    RegClass::SSEFs
                },
            );
        }
        BasicTypeEnum::FloatType(ft) if ft.get_bit_width() == 64 => {
            unify_classes(classes, index + offset / 8, RegClass::SSEDs);
        }
        BasicTypeEnum::ArrayType(at) => {
            let len = at.len() as usize;
            let el_ty = at.get_element_type();
            let el_size = size_of(el_ty, info.word_size);
            let el_align = size_of(el_ty, info.word_size);

            for i in 0..len {
                classify_type_with_classes(info, classes, el_ty, el_size, el_align, index, offset + i * el_size);
            }
        }
        BasicTypeEnum::StructType(st) => {
            let packed = st.is_packed();

            let mut field_offset = offset;

            for field_ty in st.get_field_types() {
                let field_size = size_of(field_ty, info.word_size);
                let field_align = align_of(field_ty, info.word_size);

                if !packed {
                    field_offset = calculate_align_from_offset(field_offset, field_align);
                }

                classify_type_with_classes(info, classes, field_ty, field_size, field_align, index, field_offset);

                field_offset += field_size;
            }
        }
        t => unimplemented!("{:?}", t),
    }
}

fn unify_classes(classes: &mut [RegClass], index: usize, new_class: RegClass) {
    let old_class = classes[index];

    if old_class == new_class {
        return;
    }

    // TODO: Can I simplify this conditional to a match expression
    let class_to_write = if old_class == RegClass::NoClass {
        new_class
    } else if new_class == RegClass::NoClass {
        return;
    } else if old_class == RegClass::Memory || new_class == RegClass::Memory {
        RegClass::Memory
    } else if old_class == RegClass::Int || new_class == RegClass::Int {
        RegClass::Int
    } else if old_class == RegClass::X87 || old_class == RegClass::X87Up || old_class == RegClass::ComplexX87 {
        RegClass::Memory
    } else if new_class == RegClass::X87 || new_class == RegClass::X87Up || new_class == RegClass::ComplexX87 {
        RegClass::Memory
    } else if new_class == RegClass::SSEUp {
        match old_class {
            RegClass::SSEFv
            | RegClass::SSEFs
            | RegClass::SSEDv
            | RegClass::SSEDs
            | RegClass::SSEInt8
            | RegClass::SSEInt16
            | RegClass::SSEInt32
            | RegClass::SSEInt64 => return,
            _ => new_class,
        }
    } else {
        new_class
    };

    classes[index] = class_to_write;
}

fn fixup_classes<'ctx>(classes: &mut [RegClass], ty: BasicTypeEnum<'ctx>) {
    let end = classes.len();

    if end > 2
        && matches!(
            ty,
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) | BasicTypeEnum::VectorType(_)
        )
    {
        let old_class = classes[0];

        if old_class.is_sse() {
            if old_class != RegClass::SSEUp {
                RegClass::all_mem(classes);
            }
        } else {
            RegClass::all_mem(classes);
        }
    } else {
        let mut i = 0;
        while i < end {
            match classes[i] {
                RegClass::Memory | RegClass::X87Up => {
                    RegClass::all_mem(classes);
                    return;
                }
                RegClass::SSEUp => {
                    classes[i] = RegClass::SSEDv;
                }
                RegClass::X87 => {
                    i += 1;
                    while i < end && classes[i] == RegClass::X87Up {
                        i += 1;
                    }
                }
                class if class.is_sse() => {
                    i += 1;
                    while i < end && classes[i] == RegClass::SSEUp {
                        i += 1;
                    }
                }
                _ => i += 1,
            }
        }
    }
}

fn classes_to_type<'ctx>(info: AbiInfo<'ctx>, classes: &[RegClass]) -> BasicTypeEnum<'ctx> {
    let mut types: Vec<BasicTypeEnum> = Vec::with_capacity(classes.len());

    for &class in classes.iter() {
        match class {
            RegClass::Int => types.push(info.context.i64_type().into()),
            RegClass::SSEFs => types.push(info.context.f32_type().into()),
            RegClass::SSEDs => types.push(info.context.f64_type().into()),
            RegClass::SSEFv
            | RegClass::SSEDv
            | RegClass::SSEInt8
            | RegClass::SSEInt16
            | RegClass::SSEInt32
            | RegClass::SSEInt64 => {
                unimplemented!("vector types")
            }
            _ => panic!("unhandled RegClass::{:?}", class),
        }
    }

    info.context.struct_type(&types, false).into()
}
