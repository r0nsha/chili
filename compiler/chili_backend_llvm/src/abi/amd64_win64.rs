use super::{i386, size_of, AbiFunction, AbiInfo, AbiTy};
use crate::util::IsAggregateType;
use inkwell::types::{BasicTypeEnum, FunctionType};

pub fn get_fn<'ctx>(info: AbiInfo<'ctx>, fn_ty: FunctionType<'ctx>) -> AbiFunction<'ctx> {
    AbiFunction {
        params: get_params(info, fn_ty.get_param_types()),
        ret: i386::get_return(info, fn_ty.get_return_type().unwrap()),
        variadic: fn_ty.is_var_arg(),
    }
}

pub fn get_params<'ctx>(info: AbiInfo<'ctx>, params: Vec<BasicTypeEnum<'ctx>>) -> Vec<AbiTy<'ctx>> {
    params
        .iter()
        .map(|&param| {
            if param.is_aggregate_type() {
                let size = size_of(param, info.word_size);
                match size {
                    0 | 1 | 2 | 4 | 8 => *AbiTy::direct(param)
                        .with_cast_to(info.context.custom_width_int_type((8 * size) as u32).into()),
                    _ => AbiTy::indirect(param),
                }
            } else {
                i386::non_struct(info, param, false)
            }
        })
        .collect()
}
