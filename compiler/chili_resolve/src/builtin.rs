use chili_ast::{
    ast,
    workspace::{BindingInfoFlags, BindingInfoKind, ScopeLevel, Workspace},
};
use chili_span::Span;
use ustr::ustr;

use crate::resolver::Resolver;

impl Resolver {
    pub(crate) fn add_builtin_types(&mut self, workspace: &mut Workspace) {
        let mut add_builtin_type = |name: &str| {
            let symbol = ustr(name);
            let id = workspace.add_builtin_binding_info(
                Default::default(),
                symbol,
                ast::Visibility::Private,
                false,
                BindingInfoKind::Type,
                ScopeLevel::Global,
                ustr(""),
                Span::unknown(),
            );

            workspace
                .get_binding_info_mut(id)
                .unwrap()
                .flags
                .insert(BindingInfoFlags::BUILTIN_TYPE);

            self.builtin_types.insert(symbol, id);
        };

        add_builtin_type("bool");

        add_builtin_type("i8");
        add_builtin_type("i16");
        add_builtin_type("i32");
        add_builtin_type("i64");
        add_builtin_type("int");

        add_builtin_type("u8");
        add_builtin_type("u16");
        add_builtin_type("u32");
        add_builtin_type("u64");
        add_builtin_type("uint");

        add_builtin_type("f16");
        add_builtin_type("f32");
        add_builtin_type("f64");
        add_builtin_type("float");

        add_builtin_type("str");
    }
}
