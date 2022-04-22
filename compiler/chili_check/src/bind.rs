use std::collections::HashMap;

use crate::{
    env::{Env, Scope},
    CheckSess,
};
use chili_ast::{
    ast,
    pattern::{Pattern, SymbolPattern},
    ty::{PartialStructTy, Ty, TyKind},
    value::Value,
    workspace::{BindingInfoId, ModuleId, PartialBindingInfo},
};
use chili_error::{DiagnosticResult, SyntaxError};
use chili_infer::{display::OrReportErr, unify::UnifyTy};
use chili_span::Span;
use ustr::Ustr;

impl<'s> CheckSess<'s> {
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

    pub(crate) fn get_symbol(&self, env: &Env, symbol: Ustr) -> Option<BindingInfoId> {
        if env.scope_level().is_global() {
            let module_id = env.module_id();
            self.get_global_symbol(module_id, symbol)
        } else {
            env.find_symbol(symbol)
        }
    }

    pub(crate) fn bind_symbol(
        &mut self,
        env: &mut Env,
        symbol: Ustr,
        visibility: ast::Visibility,
        ty: Ty,
        const_value: Option<Value>,
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
        }

        let id = self.workspace.add_binding_info(PartialBindingInfo {
            module_id,
            symbol,
            visibility,
            ty,
            const_value,
            is_mutable,
            kind,
            scope_level,
            scope_name: env.scope_name(),
            span,
        });

        if scope_level.is_global() {
            // insert the symbol into its module's global scope
            self.insert_global_symbol(module_id, symbol, id);
        } else {
            // insert the symbol into local scope
            env.insert_symbol(symbol, id);
        }

        Ok(id)
    }

    pub(crate) fn bind_symbol_pattern(
        &mut self,
        env: &mut Env,
        pattern: &mut SymbolPattern,
        visibility: ast::Visibility,
        ty: Ty,
        const_value: Option<Value>,
        kind: ast::BindingKind,
    ) -> DiagnosticResult<BindingInfoId> {
        pattern.binding_info_id = self.bind_symbol(
            env,
            pattern.alias.unwrap_or(pattern.symbol),
            visibility,
            ty,
            const_value,
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
        const_value: Option<Value>,
        kind: ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<()> {
        match pattern {
            Pattern::Single(pat) => {
                self.bind_symbol_pattern(env, pat, visibility, ty, const_value, kind)?;
            }
            // TODO: Need InferenceValue::PartialStruct(Vec<(Ustr, Ty)>)
            Pattern::StructUnpack(pat) => {
                let partial_struct = PartialStructTy(HashMap::from_iter(
                    pat.symbols
                        .iter()
                        .map(|symbol| (symbol.symbol, self.tycx.var(symbol.span).kind()))
                        .collect::<HashMap<Ustr, TyKind>>(),
                ));

                let partial_struct_ty = self.tycx.partial_struct(partial_struct.clone(), pat.span);

                ty.unify(&partial_struct_ty, &mut self.tycx).or_report_err(
                    &self.tycx,
                    partial_struct_ty,
                    Some(pat.span),
                    ty,
                    ty_origin_span,
                )?;

                for pat in pat.symbols.iter_mut() {
                    let ty = self
                        .tycx
                        .bound(partial_struct[&pat.symbol].clone(), pat.span);
                    self.bind_symbol_pattern(env, pat, visibility, ty, const_value, kind)?;
                }
            }
            Pattern::TupleUnpack(pat) => {
                let elements = pat
                    .symbols
                    .iter()
                    .map(|symbol| self.tycx.var(symbol.span).kind())
                    .collect::<Vec<TyKind>>();

                let partial_tuple = self.tycx.partial_tuple(elements.clone(), pat.span);

                ty.unify(&partial_tuple, &mut self.tycx).or_report_err(
                    &self.tycx,
                    partial_tuple,
                    Some(pat.span),
                    ty,
                    ty_origin_span,
                )?;

                for (index, pat) in pat.symbols.iter_mut().enumerate() {
                    let ty = self.tycx.bound(elements[index].clone(), pat.span);
                    self.bind_symbol_pattern(env, pat, visibility, ty, const_value, kind)?;
                }
            }
        }

        Ok(())
    }
}
