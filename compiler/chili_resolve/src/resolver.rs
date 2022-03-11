use std::collections::HashMap;

use crate::{
    import::ModuleExports,
    scope::{Scope, ScopeSymbol},
};
use chili_ast::{
    ast::{self, BindingKind, ModuleInfo, Visibility},
    pattern::{Pattern, SymbolPattern},
    workspace::{
        BindingInfoIdx, BindingInfoKind, ModuleIdx, ScopeLevel, Workspace,
    },
};
use chili_span::Span;
use ustr::{ustr, Ustr, UstrMap};

pub(crate) struct Resolver {
    // All exports from all modules, used for resolving
    // glob imports, i.e `foo.?`
    pub(crate) exports: ModuleExports,

    // The current module's id and information
    pub(crate) module_idx: ModuleIdx,
    pub(crate) module_info: ModuleInfo,

    // Symbols maps / Scopes
    pub(crate) builtin_types: UstrMap<BindingInfoIdx>,
    pub(crate) global_scopes: HashMap<ModuleIdx, Scope>,
    pub(crate) scopes: Vec<Scope>,

    // Scope information
    pub(crate) scope_level: ScopeLevel,
    pub(crate) function_scope_level: ScopeLevel,
}

impl Resolver {
    pub(crate) fn new() -> Self {
        Self {
            exports: Default::default(),
            module_idx: Default::default(),
            module_info: Default::default(),
            builtin_types: Default::default(),
            global_scopes: Default::default(),
            scopes: vec![],
            scope_level: ScopeLevel::Global,
            function_scope_level: ScopeLevel::Global,
        }
    }

    pub(crate) fn in_global_scope(&self) -> bool {
        self.scope_level.is_global()
    }

    pub(crate) fn current_scope(&self) -> &Scope {
        if self.scopes.is_empty() {
            self.global_scopes.get(&self.module_idx).unwrap()
        } else {
            self.scopes.last().unwrap()
        }
    }

    pub(crate) fn current_scope_mut(&mut self) -> &mut Scope {
        if self.scopes.is_empty() {
            self.global_scopes.get_mut(&self.module_idx).unwrap()
        } else {
            self.scopes.last_mut().unwrap()
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

    pub(crate) fn current_scope_name(&mut self) -> String {
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

    pub(crate) fn lookup_binding<'w>(
        &self,
        workspace: &mut Workspace<'w>,
        symbol: Ustr,
    ) -> Option<BindingInfoIdx> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.bindings.get(&symbol) {
                workspace.binding_infos[symbol.id.0].uses += 1;
                return Some(symbol.id);
            }
        }

        let global_scope = self.global_scopes.get(&self.module_idx).unwrap();
        if let Some(symbol) = global_scope.bindings.get(&symbol) {
            workspace.binding_infos[symbol.id.0].uses += 1;
            return Some(symbol.id);
        }

        if let Some(id) = self.builtin_types.get(&symbol) {
            return Some(*id);
        }

        None
    }

    pub(crate) fn add_binding<'w>(
        &mut self,
        workspace: &mut Workspace<'w>,
        symbol: Ustr,
        visibility: Visibility,
        is_mutable: bool,
        kind: BindingKind,
        span: Span,
        shadowable: bool,
    ) -> BindingInfoIdx {
        let id = workspace.add_binding_info(
            self.module_idx,
            symbol,
            visibility,
            is_mutable,
            match kind {
                ast::BindingKind::Let => BindingInfoKind::Let,
                ast::BindingKind::Type => BindingInfoKind::Type,
                ast::BindingKind::Import => BindingInfoKind::Import,
            },
            self.scope_level,
            ustr(&self.current_scope_name()),
            span,
        );

        self.current_scope_mut().bindings.insert(
            symbol,
            if shadowable {
                ScopeSymbol::shadowable(id)
            } else {
                ScopeSymbol::persistent(id)
            },
        );

        id
    }

    pub(crate) fn add_binding_with_symbol_pattern<'w>(
        &mut self,
        workspace: &mut Workspace<'w>,
        pattern: &SymbolPattern,
        visibility: Visibility,
        kind: BindingKind,
        shadowable: bool,
    ) -> BindingInfoIdx {
        self.add_binding(
            workspace,
            pattern.symbol,
            visibility,
            pattern.is_mutable,
            kind,
            pattern.span,
            shadowable,
        )
    }

    pub(crate) fn add_binding_with_pattern<'w>(
        &mut self,
        workspace: &mut Workspace<'w>,
        pattern: &mut Pattern,
        visibility: Visibility,
        kind: BindingKind,
        shadowable: bool,
    ) {
        match pattern {
            Pattern::Single(pat) => {
                pat.binding_info_idx = self.add_binding_with_symbol_pattern(
                    workspace, pat, visibility, kind, shadowable,
                );
            }
            Pattern::StructDestructor(pat) | Pattern::TupleDestructor(pat) => {
                for pat in pat.symbols.iter_mut() {
                    self.add_binding_with_symbol_pattern(
                        workspace, pat, visibility, kind, shadowable,
                    );
                }
            }
        }
    }
}
