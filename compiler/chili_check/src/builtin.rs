use chili_ast::ty::{FloatTy, IntTy, TyKind, UIntTy};
use ustr::{ustr, UstrMap};

pub(crate) fn get_builtin_types() -> UstrMap<TyKind> {
    let mut builtin_types = UstrMap::default();

    let mut add_builtin_type =
        |name: &str, ty: TyKind| builtin_types.insert(ustr(name), ty.create_type());

    add_builtin_type("bool", TyKind::Bool);

    add_builtin_type("i8", TyKind::Int(IntTy::I8));
    add_builtin_type("i16", TyKind::Int(IntTy::I16));
    add_builtin_type("i32", TyKind::Int(IntTy::I32));
    add_builtin_type("i64", TyKind::Int(IntTy::I64));
    add_builtin_type("int", TyKind::Int(IntTy::Isize));

    add_builtin_type("u8", TyKind::UInt(UIntTy::U8));
    add_builtin_type("u16", TyKind::UInt(UIntTy::U16));
    add_builtin_type("u32", TyKind::UInt(UIntTy::U32));
    add_builtin_type("u64", TyKind::UInt(UIntTy::U64));
    add_builtin_type("uint", TyKind::UInt(UIntTy::Usize));

    add_builtin_type("f16", TyKind::Float(FloatTy::F16));
    add_builtin_type("f32", TyKind::Float(FloatTy::F32));
    add_builtin_type("f64", TyKind::Float(FloatTy::F64));
    add_builtin_type("float", TyKind::Float(FloatTy::Fsize));

    add_builtin_type("str", TyKind::str());

    builtin_types
}
