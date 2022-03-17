use crate::tycx::TyCtx;
use chili_ast::ty::*;
use ustr::Ustr;

pub(crate) fn get_ty_for_builtin_type(symbol: Ustr, tycx: &mut TyCtx) -> Ty {
    match symbol.as_str() {
        "bool" => tycx.new_bound_variable(TyKind::Bool.create_type()),
        "i8" => tycx.new_bound_variable(TyKind::Int(IntTy::I8).create_type()),
        "i16" => tycx.new_bound_variable(TyKind::Int(IntTy::I16).create_type()),
        "i32" => tycx.new_bound_variable(TyKind::Int(IntTy::I32).create_type()),
        "i64" => tycx.new_bound_variable(TyKind::Int(IntTy::I64).create_type()),
        "int" => tycx.new_bound_variable(TyKind::Int(IntTy::Isize).create_type()),
        "u8" => tycx.new_bound_variable(TyKind::UInt(UIntTy::U8).create_type()),
        "u16" => tycx.new_bound_variable(TyKind::UInt(UIntTy::U16).create_type()),
        "u32" => tycx.new_bound_variable(TyKind::UInt(UIntTy::U32).create_type()),
        "u64" => tycx.new_bound_variable(TyKind::UInt(UIntTy::U64).create_type()),
        "uint" => tycx.new_bound_variable(TyKind::UInt(UIntTy::Usize).create_type()),
        "f16" => tycx.new_bound_variable(TyKind::Float(FloatTy::F16).create_type()),
        "f32" => tycx.new_bound_variable(TyKind::Float(FloatTy::F32).create_type()),
        "f64" => tycx.new_bound_variable(TyKind::Float(FloatTy::F64).create_type()),
        "float" => tycx.new_bound_variable(TyKind::Float(FloatTy::Fsize).create_type()),
        "str" => tycx.str_primitive(),
        s => panic!("got `{}`", s),
    }
}
