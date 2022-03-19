use crate::tycx::TyCtx;
use chili_ast::ty::*;
use ustr::Ustr;

pub(crate) fn get_ty_for_builtin_type(symbol: Ustr, tycx: &mut TyCtx) -> Ty {
    match symbol.as_str() {
        "unit" => tycx.common_types.unit,
        "bool" => tycx.common_types.bool,
        "i8" => tycx.common_types.i8,
        "i16" => tycx.common_types.i16,
        "i32" => tycx.common_types.i32,
        "i64" => tycx.common_types.i64,
        "int" => tycx.common_types.int,
        "u8" => tycx.common_types.u8,
        "u16" => tycx.common_types.u16,
        "u32" => tycx.common_types.u32,
        "u64" => tycx.common_types.u64,
        "uint" => tycx.common_types.uint,
        "f16" => tycx.common_types.f16,
        "f32" => tycx.common_types.f32,
        "f64" => tycx.common_types.f64,
        "float" => tycx.common_types.float,
        "str" => tycx.common_types.str,
        "never" => tycx.common_types.never,
        s => panic!("got `{}`", s),
    }
}
