use crate::tycx::TyCtx;
use chili_ast::ty::*;
use chili_resolve::builtin;
use ustr::Ustr;

pub(crate) fn get_type_for_builtin_type(symbol: Ustr, tycx: &mut TyCtx) -> Ty {
    match symbol.as_str() {
        builtin::SYM_UNIT => tycx.common_types.unit,
        builtin::SYM_BOOL => tycx.common_types.bool,
        builtin::SYM_I8 => tycx.common_types.i8,
        builtin::SYM_I16 => tycx.common_types.i16,
        builtin::SYM_I32 => tycx.common_types.i32,
        builtin::SYM_I64 => tycx.common_types.i64,
        builtin::SYM_INT => tycx.common_types.int,
        builtin::SYM_U8 => tycx.common_types.u8,
        builtin::SYM_U16 => tycx.common_types.u16,
        builtin::SYM_U32 => tycx.common_types.u32,
        builtin::SYM_U64 => tycx.common_types.u64,
        builtin::SYM_UINT => tycx.common_types.uint,
        builtin::SYM_F16 => tycx.common_types.f16,
        builtin::SYM_F32 => tycx.common_types.f32,
        builtin::SYM_F64 => tycx.common_types.f64,
        builtin::SYM_FLOAT => tycx.common_types.float,
        builtin::SYM_STR => tycx.common_types.str,
        builtin::SYM_NEVER => tycx.common_types.never,
        s => panic!("got `{}`", s),
    }
}
