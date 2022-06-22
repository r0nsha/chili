use super::{
    env::{Env, Scope, ScopeKind},
    top_level::{CallerInfo, CheckTopLevel},
    CheckSess,
};
use crate::ast::{
    ast,
    const_value::ConstValue,
    pattern::{Pattern, SymbolPattern, UnpackPattern, UnpackPatternKind},
    ty::{InferTy, PartialStructType, Type, TypeId},
    workspace::{BindingId, ModuleId, PartialBindingInfo},
};
use crate::error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult, SyntaxError,
};
use crate::infer::{display::OrReportErr, normalize::Normalize, unify::UnifyTy};
use crate::span::Span;
use indexmap::IndexMap;
use ustr::Ustr;

impl<'s> CheckSess<'s> {
    pub fn get_global_symbol(&self, module_id: ModuleId, symbol: Ustr) -> Option<BindingId> {
        self.global_scopes
            .get(&module_id)
            .and_then(|module| module.symbols.get(&symbol).cloned())
    }

    pub fn insert_global_symbol(&mut self, module_id: ModuleId, symbol: Ustr, id: BindingId) {
        self.global_scopes
            .entry(module_id)
            .or_insert(Scope::new(
                self.workspace.module_infos.get(module_id).unwrap().name,
                ScopeKind::Global,
            ))
            .symbols
            .insert(symbol, id);
    }

    pub fn get_symbol(&self, env: &Env, symbol: Ustr) -> Option<BindingId> {
        env.find_symbol(symbol)
            .or_else(|| self.get_global_symbol(env.module_id(), symbol))
    }

    pub fn bind_symbol(
        &mut self,
        env: &mut Env,
        symbol: Ustr,
        visibility: ast::Visibility,
        ty: TypeId,
        const_value: Option<ConstValue>,
        is_mutable: bool,
        kind: ast::BindingKind,
        span: Span,
    ) -> DiagnosticResult<BindingId> {
        let module_id = env.module_id();
        let scope_level = env.scope_level();
        let is_global = scope_level.is_global();

        if is_global {
            // check if there's already a binding with this symbol
            if let Some(id) = self.get_global_symbol(module_id, symbol) {
                let already_defined = self.workspace.binding_infos.get(id).unwrap();
                return Err(SyntaxError::duplicate_symbol(
                    already_defined.span,
                    span,
                    already_defined.symbol,
                ));
            }
        }

        let partial_binding_info = PartialBindingInfo {
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
        };

        let id = self
            .workspace
            .binding_infos
            .insert_with_id(partial_binding_info.into_binding_info());

        if is_global {
            // insert the symbol into its module's global scope
            self.insert_global_symbol(module_id, symbol, id);
        } else {
            // insert the symbol into local scope
            env.insert_symbol(symbol, id);
        }

        Ok(id)
    }

    pub fn bind_symbol_pattern(
        &mut self,
        env: &mut Env,
        pattern: &mut SymbolPattern,
        visibility: ast::Visibility,
        ty: TypeId,
        const_value: Option<ConstValue>,
        kind: &ast::BindingKind,
    ) -> DiagnosticResult<()> {
        if !pattern.ignore {
            pattern.id = self.bind_symbol(
                env,
                pattern.alias.unwrap_or(pattern.symbol),
                visibility,
                ty,
                if pattern.is_mutable {
                    None
                } else {
                    const_value
                },
                pattern.is_mutable,
                kind.clone(),
                pattern.span,
            )?;
        }

        Ok(())
    }

    pub fn bind_pattern(
        &mut self,
        env: &mut Env,
        pattern: &mut Pattern,
        visibility: ast::Visibility,
        ty: TypeId,
        const_value: Option<ConstValue>,
        kind: &ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<()> {
        match pattern {
            Pattern::Symbol(pattern) => {
                self.bind_symbol_pattern(env, pattern, visibility, ty, const_value, kind)
            }
            Pattern::StructUnpack(pattern) => self.bind_struct_unpack_pattern(
                env,
                pattern,
                visibility,
                ty,
                const_value,
                kind,
                ty_origin_span,
            ),
            Pattern::TupleUnpack(pattern) => self.bind_tuple_unpack_pattern(
                env,
                pattern,
                visibility,
                ty,
                const_value,
                kind,
                ty_origin_span,
            ),
            Pattern::Hybrid(pattern) => {
                self.bind_symbol_pattern(
                    env,
                    &mut pattern.symbol,
                    visibility,
                    ty,
                    const_value.clone(),
                    kind,
                )?;

                match &mut pattern.unpack {
                    UnpackPatternKind::Struct(pattern) => self.bind_struct_unpack_pattern(
                        env,
                        pattern,
                        visibility,
                        ty,
                        const_value,
                        kind,
                        ty_origin_span,
                    )?,
                    UnpackPatternKind::Tuple(pattern) => self.bind_tuple_unpack_pattern(
                        env,
                        pattern,
                        visibility,
                        ty,
                        const_value,
                        kind,
                        ty_origin_span,
                    )?,
                }

                Ok(())
            }
        }
    }

    fn bind_struct_unpack_pattern(
        &mut self,
        env: &mut Env,
        unpack_pattern: &mut UnpackPattern,
        visibility: ast::Visibility,
        ty: TypeId,
        const_value: Option<ConstValue>,
        kind: &ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<()> {
        match ty.normalize(&self.tycx) {
            Type::Module(module_id) => {
                for pattern in unpack_pattern.symbols.iter_mut() {
                    if pattern.ignore {
                        continue;
                    }

                    let (res, top_level_symbol_id) = self.check_top_level_symbol(
                        CallerInfo {
                            module_id: env.module_id(),
                            span: pattern.span,
                        },
                        module_id,
                        pattern.symbol,
                    )?;

                    // Note (Ron 19/06/2022):
                    // This is a HACK, to avoid binding the same symbol twice.
                    // When two mutually recursive modules are checking each other,
                    // the original symbol that started the check cycle is bound twice - causing a false `duplicate symbol error`
                    if env.scope_level().is_global()
                        && self
                            .get_global_symbol(env.module_id(), pattern.symbol)
                            .is_some()
                    {
                        continue;
                    }

                    self.workspace
                        .add_binding_info_use(top_level_symbol_id, pattern.span);

                    self.bind_symbol_pattern(
                        env,
                        pattern,
                        visibility,
                        res.ty,
                        res.const_value,
                        kind,
                    )?;

                    self.workspace
                        .set_binding_info_redirect(pattern.id, top_level_symbol_id);
                }

                if let Some(wildcard_symbol) = unpack_pattern.wildcard_symbol {
                    let module = self
                        .modules
                        .iter()
                        .find(|m| m.module_id == module_id)
                        .unwrap_or_else(|| panic!("couldn't find {:?}", module_id));

                    for binding in &module.bindings {
                        if binding.visibility.is_private() {
                            continue;
                        }

                        let binding_pattern = match binding.pattern.iter().next() {
                            Some(pattern) => {
                                // check if the binding has already been checked
                                if let Some(id) = self.get_global_symbol(module_id, pattern.symbol)
                                {
                                    self.typed_ast.get_binding(id).unwrap().pattern.clone()
                                } else {
                                    let mut binding = binding.clone();
                                    binding.check_top_level(self)?;
                                    binding.pattern
                                }
                            }
                            None => {
                                let mut binding = binding.clone();
                                binding.check_top_level(self)?;
                                binding.pattern
                            }
                        };

                        for pattern in binding_pattern.iter() {
                            let binding_info =
                                self.workspace.binding_infos.get(pattern.id).unwrap();

                            let mut new_pattern = SymbolPattern {
                                id: BindingId::unknown(),
                                symbol: pattern.symbol,
                                alias: None,
                                span: wildcard_symbol,
                                is_mutable: false,
                                ignore: false,
                            };

                            self.bind_symbol_pattern(
                                env,
                                &mut new_pattern,
                                visibility,
                                binding_info.ty,
                                binding_info.const_value.clone(),
                                kind,
                            )?;

                            self.workspace
                                .set_binding_info_redirect(new_pattern.id, pattern.id);

                            unpack_pattern.symbols.push(new_pattern);
                        }
                    }
                }

                Ok(())
            }
            ty_kind => {
                let partial_struct = PartialStructType(IndexMap::from_iter(
                    unpack_pattern
                        .symbols
                        .iter()
                        .map(|symbol| (symbol.symbol, self.tycx.var(symbol.span).as_kind())),
                ));

                let partial_struct_ty = self
                    .tycx
                    .partial_struct(partial_struct.clone(), unpack_pattern.span);

                ty.unify(&partial_struct_ty, &mut self.tycx).or_report_err(
                    &self.tycx,
                    partial_struct_ty,
                    Some(unpack_pattern.span),
                    ty,
                    ty_origin_span,
                )?;

                for pattern in unpack_pattern.symbols.iter_mut() {
                    if pattern.ignore {
                        continue;
                    }

                    let ty = self
                        .tycx
                        .bound(partial_struct[&pattern.symbol].clone(), pattern.span);

                    let field_const_value = if pattern.is_mutable {
                        None
                    } else {
                        const_value
                            .as_ref()
                            .map(|v| v.as_struct().get(&pattern.symbol).unwrap().clone().value)
                    };

                    self.bind_symbol_pattern(
                        env,
                        pattern,
                        visibility,
                        ty,
                        field_const_value,
                        kind,
                    )?;
                }

                if let Some(wildcard_symbol) = unpack_pattern.wildcard_symbol {
                    match ty_kind.maybe_deref_once() {
                        Type::Struct(struct_ty) => {
                            for field in struct_ty.fields.iter() {
                                if unpack_pattern
                                    .symbols
                                    .iter()
                                    .find(|p| field.symbol == p.symbol)
                                    .is_some()
                                {
                                    // skip explicitly unpacked fields
                                    continue;
                                }

                                let field_const_value = const_value.as_ref().map(|v| {
                                    v.as_struct().get(&field.symbol).unwrap().clone().value
                                });

                                let mut field_pattern = SymbolPattern {
                                    id: BindingId::unknown(),
                                    symbol: field.symbol,
                                    alias: None,
                                    span: wildcard_symbol,
                                    is_mutable: false,
                                    ignore: false,
                                };

                                let ty = self.tycx.bound(field.ty.clone(), field.span);

                                self.bind_symbol_pattern(
                                    env,
                                    &mut field_pattern,
                                    visibility,
                                    ty,
                                    field_const_value,
                                    kind,
                                )?;

                                unpack_pattern.symbols.push(field_pattern);
                            }
                        }
                        Type::Infer(_, InferTy::PartialStruct(_)) => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use wildcard unpack on partial struct type - {}",
                                    ty_kind
                                ))
                                .with_label(Label::primary(
                                    wildcard_symbol,
                                    "illegal wildcard unpack",
                                )))
                        }
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use wildcard unpack on type {}",
                                    ty_kind
                                ))
                                .with_label(Label::primary(
                                    wildcard_symbol,
                                    "illegal wildcard unpack",
                                )))
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn bind_tuple_unpack_pattern(
        &mut self,
        env: &mut Env,
        pattern: &mut UnpackPattern,
        visibility: ast::Visibility,
        ty: TypeId,
        const_value: Option<ConstValue>,
        kind: &ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<()> {
        let elements = pattern
            .symbols
            .iter()
            .map(|symbol| self.tycx.var(symbol.span).as_kind())
            .collect::<Vec<Type>>();

        let partial_tuple = self.tycx.partial_tuple(elements.clone(), pattern.span);

        ty.unify(&partial_tuple, &mut self.tycx).or_report_err(
            &self.tycx,
            partial_tuple,
            Some(pattern.span),
            ty,
            ty_origin_span,
        )?;

        for (index, pattern) in pattern.symbols.iter_mut().enumerate() {
            if pattern.ignore {
                continue;
            }

            let ty = self.tycx.bound(elements[index].clone(), pattern.span);

            let element_const_value = if pattern.is_mutable {
                None
            } else {
                const_value
                    .as_ref()
                    .map(|v| v.as_tuple()[index].clone().value)
            };

            self.bind_symbol_pattern(env, pattern, visibility, ty, element_const_value, kind)?;
        }

        Ok(())
    }
}
