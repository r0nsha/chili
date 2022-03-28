use crate::{
    env::{Env, Scope},
    CheckSess, Res,
};
use chili_ast::{
    ast,
    pattern::{Pattern, SymbolPattern},
    ty::Ty,
    workspace::{BindingInfoId, ModuleId},
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;
use ustr::Ustr;

impl<'s> CheckSess<'s> {
    pub(crate) fn get_binding_res(&self, id: BindingInfoId) -> Option<Res> {
        self.workspace.get_binding_info(id).map(|binding_info| {
            Res::new_maybe_const(binding_info.ty, self.const_bindings.get(&id).cloned())
        })
    }

    pub(crate) fn get_global_symbol(
        &self,
        module_id: ModuleId,
        symbol: Ustr,
    ) -> Option<BindingInfoId> {
        self.global_scopes
            .get(&module_id)
            .map(|module| module.symbols.get(&symbol).cloned())
            .flatten()
    }

    pub(crate) fn insert_global_symbol(
        &mut self,
        module_id: ModuleId,
        symbol: Ustr,
        id: BindingInfoId,
    ) {
        self.global_scopes
            .entry(module_id)
            .or_insert(Scope::new(
                self.workspace.get_module_info(module_id).unwrap().name,
            ))
            .symbols
            .insert(symbol, id);
    }

    pub(crate) fn bind_symbol(
        &mut self,
        env: &mut Env,
        symbol: Ustr,
        visibility: ast::Visibility,
        ty: Ty,
        is_mutable: bool,
        kind: ast::BindingKind,
        span: Span,
    ) -> DiagnosticResult<BindingInfoId> {
        let module_id = env.module_id();
        let scope_level = env.scope_level();

        if scope_level.is_global() {
            // check if there's already a binding with this symbol
            if let Some(id) = self.get_global_symbol(module_id, symbol) {
                let dup = self.workspace.get_binding_info(id).unwrap();
                return Err(SyntaxError::duplicate_symbol(dup.span, span, dup.symbol));
            }

            let id = self.workspace.add_binding_info(
                module_id,
                symbol,
                visibility,
                ty,
                is_mutable,
                kind,
                scope_level,
                env.scope_name(),
                span,
            );

            // insert symbol into its module's global scope
            self.insert_global_symbol(module_id, symbol, id);

            Ok(id)
        } else {
            // insert symbol into local scope

            let id = self.workspace.add_binding_info(
                module_id,
                symbol,
                visibility,
                ty,
                is_mutable,
                kind,
                scope_level,
                env.scope_name(),
                span,
            );

            env.insert_symbol(symbol, id);

            Ok(id)
        }
    }

    pub(crate) fn bind_symbol_pattern(
        &mut self,
        env: &mut Env,
        pattern: &mut SymbolPattern,
        visibility: ast::Visibility,
        ty: Ty,
        kind: ast::BindingKind,
    ) -> DiagnosticResult<BindingInfoId> {
        pattern.binding_info_id = self.bind_symbol(
            env,
            pattern.alias.unwrap_or(pattern.symbol),
            visibility,
            ty,
            pattern.is_mutable,
            kind,
            pattern.span,
        )?;

        Ok(pattern.binding_info_id)
    }

    pub(crate) fn bind_pattern(
        &mut self,
        env: &mut Env,
        pattern: &mut Pattern,
        visibility: ast::Visibility,
        ty: Ty,
        kind: ast::BindingKind,
    ) -> DiagnosticResult<()> {
        match pattern {
            Pattern::Single(pat) => {
                self.bind_symbol_pattern(env, pat, visibility, ty, kind)?;
            }
            // TODO: Need InferenceValue::PartialStruct(Vec<(Ustr, Ty)>)
            Pattern::StructUnpack(_) => {
                todo!()
                // for pat in pat.symbols.iter_mut() {
                //     self.bind_symbol_pattern(workspace, pat, visibility, ty, kind)?;
                // }
            }
            // TODO: Need InferenceValue::PartialTuple(Vec<Ty>)
            Pattern::TupleUnpack(_) => {
                todo!()
                // for pat in pat.symbols.iter_mut() {
                //     self.bind_symbol_pattern(workspace, pat, visibility, ty, kind)?;
                // }
            }
        }

        Ok(())
    }
}
