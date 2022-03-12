use chili_ast::ty::{FloatTy, IntTy, Ty, UIntTy};
use chili_ast::value::Value;
use chili_ast::{
    ast,
    workspace::{BindingInfoKind, ScopeLevel, Workspace},
};
use chili_span::Span;
use ustr::ustr;

use crate::resolver::Resolver;

impl Resolver {
    pub(crate) fn add_builtin_types(&mut self, workspace: &mut Workspace) {
        let mut add_builtin_type = |name: &str, ty: Ty| {
            let symbol = ustr(name);
            let id = workspace.add_binding_info_ex(
                Default::default(),
                symbol,
                ast::Visibility::Private,
                ty.clone().create_type(),
                Some(Value::Type(ty)),
                false,
                BindingInfoKind::Type,
                ScopeLevel::Global,
                ustr(""),
                Span::unknown(),
            );

            self.builtin_types.insert(symbol, id);
        };

        add_builtin_type("bool", Ty::Bool);

        add_builtin_type("i8", Ty::Int(IntTy::I8));
        add_builtin_type("i16", Ty::Int(IntTy::I16));
        add_builtin_type("i32", Ty::Int(IntTy::I32));
        add_builtin_type("i64", Ty::Int(IntTy::I64));
        add_builtin_type("int", Ty::Int(IntTy::Isize));

        add_builtin_type("u8", Ty::UInt(UIntTy::U8));
        add_builtin_type("u16", Ty::UInt(UIntTy::U16));
        add_builtin_type("u32", Ty::UInt(UIntTy::U32));
        add_builtin_type("u64", Ty::UInt(UIntTy::U64));
        add_builtin_type("uint", Ty::UInt(UIntTy::Usize));

        add_builtin_type("f16", Ty::Float(FloatTy::F16));
        add_builtin_type("f32", Ty::Float(FloatTy::F32));
        add_builtin_type("f64", Ty::Float(FloatTy::F64));
        add_builtin_type("float", Ty::Float(FloatTy::Fsize));

        add_builtin_type("str", Ty::str());
    }
}
