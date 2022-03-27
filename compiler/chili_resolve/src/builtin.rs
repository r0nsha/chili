use chili_ast::{
    ast,
    workspace::{BindingInfoFlags, ScopeLevel, Workspace},
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
                ast::Visibility::Public,
                false,
                ast::BindingKind::Type,
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

        add_builtin_type(SYM_UNIT);
        add_builtin_type(SYM_BOOL);

        add_builtin_type(SYM_I8);
        add_builtin_type(SYM_I16);
        add_builtin_type(SYM_I32);
        add_builtin_type(SYM_I64);
        add_builtin_type(SYM_INT);

        add_builtin_type(SYM_U8);
        add_builtin_type(SYM_U16);
        add_builtin_type(SYM_U32);
        add_builtin_type(SYM_U64);
        add_builtin_type(SYM_UINT);

        add_builtin_type(SYM_F16);
        add_builtin_type(SYM_F32);
        add_builtin_type(SYM_F64);
        add_builtin_type(SYM_FLOAT);

        add_builtin_type(SYM_STR);

        add_builtin_type(SYM_NEVER);
    }
}

pub const SYM_UNIT: &str = "unit";
pub const SYM_BOOL: &str = "bool";
pub const SYM_I8: &str = "i8";
pub const SYM_I16: &str = "i16";
pub const SYM_I32: &str = "i32";
pub const SYM_I64: &str = "i64";
pub const SYM_INT: &str = "int";
pub const SYM_U8: &str = "u8";
pub const SYM_U16: &str = "u16";
pub const SYM_U32: &str = "u32";
pub const SYM_U64: &str = "u64";
pub const SYM_UINT: &str = "uint";
pub const SYM_F16: &str = "f16";
pub const SYM_F32: &str = "f32";
pub const SYM_F64: &str = "f64";
pub const SYM_FLOAT: &str = "float";
pub const SYM_STR: &str = "str";
pub const SYM_NEVER: &str = "never";
