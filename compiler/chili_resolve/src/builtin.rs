use chili_ast::ty::{FloatTy, IntTy, TyKind, UIntTy};
use chili_ast::value::Value;
use chili_ast::{
    ast,
    workspace::{BindingInfoKind, ScopeLevel, Workspace},
};
use chili_span::Span;
use ustr::ustr;

use crate::resolver::Resolver;

impl Resolver {
    pub(crate) fn add_builtin_types<'w>(&mut self, workspace: &mut Workspace<'w>) {
        let mut add_builtin_type = |name: &str, ty: TyKind| {
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
    }
}
