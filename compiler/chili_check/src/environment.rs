use std::collections::HashMap;

use chili_ast::{
    ast,
    ty::Ty,
    value::Value,
    workspace::{BindingInfoFlags, BindingInfoId, ModuleId, ModuleInfo, ScopeLevel, Workspace},
};
use chili_resolve::builtin;
use chili_span::Span;
use ustr::{ustr, Ustr, UstrMap};

use crate::ty_ctx::TyCtx;

pub(crate) struct Environment {
    // The current module's id and information
    pub(crate) module_id: ModuleId,
    pub(crate) module_info: ModuleInfo,

    // Symbols maps / Scopes
    pub(crate) builtin_types: UstrMap<BindingInfoId>,
    pub(crate) global_scopes: HashMap<ModuleId, Scope>,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) const_bindings: HashMap<BindingInfoId, Value>,

    // Scope information
    pub(crate) scope_level: ScopeLevel,
}

impl Environment {
    pub(crate) fn new(workspace: &mut Workspace, tycx: &mut TyCtx) -> Self {
        let mut inst = Self {
            module_id: ModuleId::default(),
            module_info: ModuleInfo::default(),
            builtin_types: UstrMap::default(),
            global_scopes: HashMap::default(),
            scopes: vec![],
            const_bindings: HashMap::default(),
            scope_level: ScopeLevel::Global,
        };

        inst.add_builtin_types(workspace, tycx);

        inst
    }

    pub(crate) fn set_module(&mut self, id: ModuleId, info: ModuleInfo) {
        self.module_id = id;
        self.module_info = info;
    }

    pub(crate) fn module_info(&self) -> (ModuleId, ModuleInfo) {
        (self.module_id, self.module_info)
    }

    pub(crate) fn scope(&self) -> &Scope {
        if self.scopes.is_empty() {
            self.global_scopes.get(&self.module_id).unwrap()
        } else {
            self.scopes.last().unwrap()
        }
    }

    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        if self.scopes.is_empty() {
            self.global_scopes.get_mut(&self.module_id).unwrap()
        } else {
            self.scopes.last_mut().unwrap()
        }
    }

    pub(crate) fn scope_level(&self) -> ScopeLevel {
        self.scope_level
    }

    pub(crate) fn scope_name(&self) -> String {
        let scopes_str = self
            .scopes
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<&str>>()
            .join(".");

        if self.module_info.name.is_empty() {
            scopes_str
        } else {
            format!("{}.{}", self.module_info.name, scopes_str)
        }
    }

    pub(crate) fn push_scope(&mut self) {
        self.push_named_scope("_");
    }

    pub(crate) fn push_named_scope(&mut self, name: impl ToString) {
        self.scopes.push(Scope::new(name));
        self.scope_level = self.scope_level.next();
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
        self.scope_level = self.scope_level.previous();
    }

    pub(crate) fn lookup_binding<'w>(
        &self,
        workspace: &mut Workspace,
        symbol: Ustr,
    ) -> Option<BindingInfoId> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.bindings.get(&symbol) {
                workspace.binding_infos[symbol.id.0].uses += 1;
                return Some(symbol.id);
            }
        }

        let global_scope = self.global_scopes.get(&self.module_id).unwrap();
        if let Some(symbol) = global_scope.bindings.get(&symbol) {
            workspace.binding_infos[symbol.id.0].uses += 1;
            return Some(symbol.id);
        }

        if let Some(id) = self.builtin_types.get(&symbol) {
            return Some(*id);
        }

        None
    }

    // pub(crate) fn add_binding<'w>(
    //     &mut self,
    //     workspace: &mut Workspace,
    //     symbol: Ustr,
    //     visibility: Visibility,
    //     is_mutable: bool,
    //     kind: BindingKind,
    //     span: Span,
    //     shadowable: bool,
    // ) -> BindingInfoId {
    //     let id = workspace.add_binding_info(
    //         self.module_id,
    //         symbol,
    //         visibility,
    //         is_mutable,
    //         kind,
    //         self.scope_level,
    //         ustr(&self.current_scope_name()),
    //         span,
    //     );

    //     self.current_scope_mut().bindings.insert(
    //         symbol,
    //         if shadowable {
    //             ScopeSymbol::shadowable(id)
    //         } else {
    //             ScopeSymbol::persistent(id)
    //         },
    //     );

    //     id
    // }

    // pub(crate) fn add_binding_with_symbol_pattern<'w>(
    //     &mut self,
    //     workspace: &mut Workspace,
    //     pattern: &SymbolPattern,
    //     visibility: Visibility,
    //     kind: BindingKind,
    //     shadowable: bool,
    // ) -> BindingInfoId {
    //     self.add_binding(
    //         workspace,
    //         pattern.symbol,
    //         visibility,
    //         pattern.is_mutable,
    //         kind,
    //         pattern.span,
    //         shadowable,
    //     )
    // }

    // pub(crate) fn add_binding_with_pattern<'w>(
    //     &mut self,
    //     workspace: &mut Workspace,
    //     pattern: &mut Pattern,
    //     visibility: Visibility,
    //     kind: BindingKind,
    //     shadowable: bool,
    // ) {
    //     match pattern {
    //         Pattern::Single(pat) => {
    //             pat.binding_info_id = self
    //                 .add_binding_with_symbol_pattern(workspace, pat, visibility, kind, shadowable);
    //         }
    //         Pattern::StructUnpack(pat) | Pattern::TupleUnpack(pat) => {
    //             for pat in pat.symbols.iter_mut() {
    //                 self.add_binding_with_symbol_pattern(
    //                     workspace, pat, visibility, kind, shadowable,
    //                 );
    //             }
    //         }
    //     }
    // }

    fn add_builtin_types(&mut self, workspace: &mut Workspace, tycx: &mut TyCtx) {
        let mut mk = |workspace: &mut Workspace, tycx: &mut TyCtx, symbol: &str, ty: Ty| {
            let symbol = ustr(symbol);

            let id = workspace.add_binding_info(
                Default::default(),
                symbol,
                ast::Visibility::Public,
                false,
                ast::BindingKind::Type,
                ScopeLevel::Global,
                ustr(""),
                Span::unknown(),
            );

            let info = workspace.get_binding_info_mut(id).unwrap();

            info.ty = tycx.bound(ty.kind().create_type());
            info.flags.insert(BindingInfoFlags::BUILTIN_TYPE);

            self.const_bindings.insert(id, Value::Type(ty));

            self.builtin_types.insert(symbol, id);
        };

        mk(workspace, tycx, builtin::SYM_UNIT, tycx.common_types.unit);
        mk(workspace, tycx, builtin::SYM_BOOL, tycx.common_types.bool);

        mk(workspace, tycx, builtin::SYM_I8, tycx.common_types.i8);
        mk(workspace, tycx, builtin::SYM_I16, tycx.common_types.i16);
        mk(workspace, tycx, builtin::SYM_I32, tycx.common_types.i32);
        mk(workspace, tycx, builtin::SYM_I64, tycx.common_types.i64);
        mk(workspace, tycx, builtin::SYM_INT, tycx.common_types.int);

        mk(workspace, tycx, builtin::SYM_U8, tycx.common_types.u8);
        mk(workspace, tycx, builtin::SYM_U16, tycx.common_types.u16);
        mk(workspace, tycx, builtin::SYM_U32, tycx.common_types.u32);
        mk(workspace, tycx, builtin::SYM_U64, tycx.common_types.u64);
        mk(workspace, tycx, builtin::SYM_UINT, tycx.common_types.uint);

        mk(workspace, tycx, builtin::SYM_F16, tycx.common_types.f16);
        mk(workspace, tycx, builtin::SYM_F32, tycx.common_types.f32);
        mk(workspace, tycx, builtin::SYM_F64, tycx.common_types.f64);
        mk(workspace, tycx, builtin::SYM_FLOAT, tycx.common_types.float);

        mk(workspace, tycx, builtin::SYM_STR, tycx.common_types.str);

        mk(workspace, tycx, builtin::SYM_NEVER, tycx.common_types.never);
    }
}

pub(crate) struct Scope {
    pub(crate) name: String,
    pub(crate) bindings: UstrMap<ScopeSymbol<BindingInfoId>>,
}

impl Scope {
    pub(crate) fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            bindings: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ScopeSymbol<Id: Copy> {
    pub(crate) id: Id,
    shadowable: bool,
}

impl<Id: Copy> ScopeSymbol<Id> {
    pub(crate) fn persistent(id: Id) -> Self {
        Self {
            id,
            shadowable: false,
        }
    }

    pub(crate) fn shadowable(id: Id) -> Self {
        Self {
            id,
            shadowable: true,
        }
    }

    pub(crate) fn is_shadowable(&self) -> bool {
        self.shadowable
    }
}
