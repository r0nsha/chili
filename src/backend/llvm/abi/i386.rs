use super::super::traits::IsAggregateType;
use super::{size_of, AbiFunction, AbiInfo, AbiTy};
use inkwell::{
    attributes::Attribute,
    types::{AnyType, BasicTypeEnum, FunctionType},
};

#[allow(unused)]
pub(super) fn get_fn<'ctx>(info: AbiInfo<'ctx>, fn_ty: FunctionType<'ctx>) -> AbiFunction<'ctx> {
    AbiFunction {
        params: get_params(info, fn_ty.get_param_types()),
        ret: get_return(info, fn_ty.get_return_type().unwrap()),
        variadic: fn_ty.is_var_arg(),
    }
}

#[allow(unused)]
pub(super) fn get_params<'ctx>(info: AbiInfo<'ctx>, params: Vec<BasicTypeEnum<'ctx>>) -> Vec<AbiTy<'ctx>> {
    params
        .iter()
        .map(|&param| {
            if param.is_aggregate_type() {
                let size = size_of(param, info.word_size);
                if size == 0 {
                    AbiTy::ignore(param)
                } else {
                    AbiTy::indirect(param)
                }
            } else {
                non_struct(info, param, false)
            }
        })
        .collect()
}

pub(super) fn get_return<'ctx>(info: AbiInfo<'ctx>, ret: BasicTypeEnum<'ctx>) -> AbiTy<'ctx> {
    if ret.is_aggregate_type() {
        let size = size_of(ret, info.word_size);
        match size {
            0 => AbiTy::direct(info.context.struct_type(&[], false).into()),
            1 => AbiTy::direct(info.context.i8_type().into()),
            2 => AbiTy::direct(info.context.i16_type().into()),
            4 => AbiTy::direct(info.context.i32_type().into()),
            8 => AbiTy::direct(info.context.i64_type().into()),
            _ => *AbiTy::indirect(ret).with_attr(
                info.context
                    .create_type_attribute(Attribute::get_named_enum_kind_id("sret"), ret.as_any_type_enum()),
            ),
        }
    } else {
        non_struct(info, ret, true)
    }
}

pub(super) fn non_struct<'ctx>(info: AbiInfo<'ctx>, ty: BasicTypeEnum<'ctx>, is_return: bool) -> AbiTy<'ctx> {
    if !is_return && size_of(ty, info.word_size) > 8 {
        AbiTy::indirect(ty)
    } else {
        let mut abi_ty = AbiTy::direct(ty);

        if ty.is_int_type() && ty.into_int_type().get_bit_width() == 1 {
            abi_ty.attr = Some(
                info.context
                    .create_enum_attribute(Attribute::get_named_enum_kind_id("zeroext"), 0),
            );
        }

        abi_ty
    }
}
