use chilic_ty::{FloatTy, IntTy, Ty, UIntTy};
use ustr::{ustr, UstrMap};

pub(crate) fn get_builtin_types() -> UstrMap<Ty> {
    let mut builtin_types = UstrMap::default();

    let mut add_builtin_type =
        |name: &str, ty: Ty| builtin_types.insert(ustr(name), ty.create_type());

    add_builtin_type("bool", Ty::Bool);

    add_builtin_type("i8", Ty::Int(IntTy::I8));
    add_builtin_type("i16", Ty::Int(IntTy::I16));
    add_builtin_type("i32", Ty::Int(IntTy::I32));
    add_builtin_type("i64", Ty::Int(IntTy::I64));
    add_builtin_type("int", Ty::Int(IntTy::ISize));

    add_builtin_type("u8", Ty::UInt(UIntTy::U8));
    add_builtin_type("u16", Ty::UInt(UIntTy::U16));
    add_builtin_type("u32", Ty::UInt(UIntTy::U32));
    add_builtin_type("u64", Ty::UInt(UIntTy::U64));
    add_builtin_type("uint", Ty::UInt(UIntTy::USize));

    add_builtin_type("f16", Ty::Float(FloatTy::F16));
    add_builtin_type("f32", Ty::Float(FloatTy::F32));
    add_builtin_type("f64", Ty::Float(FloatTy::F64));
    add_builtin_type("float", Ty::Float(FloatTy::FSize));

    add_builtin_type("str", Ty::str());

    builtin_types
}
