use crate::{
    BindingInfo, CheckContext, CheckFrame, ProcessedItem, TopLevelLookupKind,
};
use chili_ast::{
    ast::{Binding, BindingKind, Import, Module, ModuleInfo, Visibility},
    pattern::{Pattern, SymbolPattern},
    value::Value,
};
use chili_error::{DiagnosticResult, TypeError};
use chili_infer::substitute::Substitute;
use chili_span::Span;
use chili_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::env::Env;
use ustr::{ustr, Ustr};

impl<'a> CheckContext<'a> {
    pub(crate) fn check_binding(
        &mut self,
        frame: &mut CheckFrame,
        binding: &Binding,
    ) -> DiagnosticResult<Binding> {
        let (ty_expr, expected_var) = match &binding.ty_expr {
            Some(expr) => {
                let type_expr = self.check_type_expr(frame, expr)?;
                let ty = type_expr.value.unwrap().into_type();
                (Some(type_expr.expr), self.infcx.fresh_bound_type_var(ty))
            }
            None => (None, self.infcx.fresh_type_var()),
        };

        let (value, const_value) = if let Some(value) = &binding.value {
            let mut result =
                self.check_expr(frame, value, Some(expected_var.into()))?;

            let is_a_type =
                result.value.as_ref().map_or(false, |v| v.is_type());

            match &binding.kind {
                BindingKind::Let => {
                    if is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            &self.infcx.normalize_ty_and_untyped(&result.ty),
                            "a value",
                        ));
                    }
                }
                BindingKind::Type => {
                    if !is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            &self.infcx.normalize_ty_and_untyped(&result.ty),
                            "a type",
                        ));
                    }
                }
            }

            let span = result.expr.span;
            self.infcx.unify_or_coerce_ty_expr(
                &Ty::from(expected_var),
                &mut result.expr,
                span,
            )?;

            (Some(result.expr), result.value)
        } else {
            (None, None)
        };

        // * don't allow types to be bounded to mutable bindings
        if const_value.as_ref().map_or(false, |v| v.is_type()) {
            match &binding.pattern {
                Pattern::Single(SymbolPattern {
                    span, is_mutable, ..
                }) => {
                    if *is_mutable {
                        return Err(Diagnostic::error()
                            .with_message(
                                "variable of type `type` must be immutable",
                            )
                            .with_labels(vec![Label::primary(
                                span.file_id,
                                span.range().clone(),
                            )])
                            .with_notes(vec![String::from(
                                "try removing the `mut` from the declaration",
                            )]));
                    }
                }
                Pattern::StructDestructor(_) | Pattern::TupleDestructor(_) => {
                    unreachable!()
                }
            }
        }

        // * don't allow const values with mutable bindings
        let const_value = match &binding.pattern {
            Pattern::Single(SymbolPattern { is_mutable, .. }) => {
                if *is_mutable {
                    None
                } else {
                    const_value
                }
            }
            Pattern::StructDestructor(_) | Pattern::TupleDestructor(_) => None,
        };

        let ty = self.infcx.normalize_ty(&expected_var.into());

        self.check_binding_pattern(
            frame,
            &binding.pattern,
            ty,
            const_value.clone(),
            value.is_some() || const_value.is_some(),
        )?;

        Ok(Binding {
            binding_info_id: binding.binding_info_id,
            kind: binding.kind,
            pattern: binding.pattern.clone(),
            ty_expr,
            ty: expected_var.into(),
            value,
            visibility: binding.visibility,
            const_value,
            should_codegen: self.in_main_path
                || frame.module_info.name.starts_with("std"), /* TODO: this
                                                               * could cause
                                                               * bugs */
            lib_name: binding.lib_name,
        })
    }

    pub(crate) fn check_top_level_binding(
        &mut self,
        module_info: ModuleInfo,
        calling_module: Ustr,
        symbol: Ustr,
        symbol_span: Span,
        lookup_kind: TopLevelLookupKind,
    ) -> DiagnosticResult<BindingInfo> {
        // if the binding is already in new_ir, get its type instead
        match self.new_ir.modules.get(&module_info.name) {
            Some(module) => match module.find_binding(symbol) {
                Some(binding) => {
                    let SymbolPattern { symbol, span, .. } =
                        binding.pattern.into_single();

                    self.is_item_accessible(
                        binding.visibility,
                        symbol,
                        span,
                        module_info,
                        calling_module,
                        symbol_span,
                    )?;

                    return Ok(BindingInfo::from_binding(binding));
                }
                None => (),
            },
            None => (),
        };

        // binding has not been checked yet
        let module = self
            .old_ir
            .modules
            .get(&module_info.name)
            .expect(&format!("couldn't find module `{}`", module_info.name));

        match module.find_binding(symbol) {
            Some(binding) => self.check_top_level_binding_internal(
                module_info,
                binding,
                calling_module,
                symbol_span,
            ),
            None => {
                match module.find_import(symbol) {
                    Some(import) => {
                        let new_module =
                            self.get_or_insert_new_module(module_info);

                        if let None = new_module.find_import(import.alias) {
                            new_module.imports.push(import.clone());
                        }

                        self.is_item_accessible(
                            import.visibility,
                            import.alias,
                            import.span(),
                            module_info,
                            calling_module,
                            symbol_span,
                        )?;

                        return self.check_import(calling_module, &import);
                    }
                    None => (),
                }

                // * search builtin symbols
                match self.builtin_types.get(&symbol) {
                    Some(ty) => Ok(BindingInfo {
                        ty: ty.clone(),
                        const_value: Some(Value::Type(ty.clone())),
                        is_mutable: false,
                        is_init: true,
                        span: symbol_span,
                    }),
                    None => Err(match lookup_kind {
                        TopLevelLookupKind::CurrentModule => {
                            Diagnostic::error()
                                .with_message(format!(
                                    "cannot find value `{}` in this scope",
                                    symbol
                                ))
                                .with_labels(vec![Label::primary(
                                    symbol_span.file_id,
                                    symbol_span.range(),
                                )
                                .with_message("not found in this scope")])
                        }
                        TopLevelLookupKind::OtherModule => Diagnostic::error()
                            .with_message(format!(
                                "cannot find value `{}` in module `{}`",
                                symbol, module_info.name,
                            ))
                            .with_labels(vec![Label::primary(
                                symbol_span.file_id,
                                symbol_span.range(),
                            )
                            .with_message(format!(
                                "not found in `{}`",
                                module_info.name
                            ))]),
                    }),
                }
            }
        }
    }

    #[inline]
    pub(crate) fn get_or_insert_new_module(
        &mut self,
        module_info: ModuleInfo,
    ) -> &mut Module {
        self.new_ir
            .modules
            .entry(module_info.name)
            .or_insert_with(|| Module::new(module_info))
    }

    #[inline]
    pub(crate) fn check_top_level_binding_internal(
        &mut self,
        module_info: ModuleInfo,
        binding: &Binding,
        calling_module: Ustr,
        calling_span: Span,
    ) -> DiagnosticResult<BindingInfo> {
        let SymbolPattern { symbol, span, .. } = binding.pattern.into_single();

        if !self.processed_items_stack.is_empty() {
            if self.processed_items_stack.iter().any(|item| {
                module_info.name == item.module_name && symbol == item.symbol
            }) {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "item `{}` cannot refer to itself",
                        symbol
                    ))
                    .with_labels(vec![
                        Label::primary(
                            calling_span.file_id,
                            calling_span.range(),
                        )
                        .with_message("reference is here"),
                        Label::secondary(span.file_id, span.range().clone())
                            .with_message(format!(
                                "`{}` declared here",
                                symbol
                            )),
                    ]));
            }
        }

        self.processed_items_stack.push(ProcessedItem {
            module_name: module_info.name,
            symbol,
        });

        self.is_item_accessible(
            binding.visibility,
            symbol,
            span,
            module_info,
            calling_module,
            calling_span,
        )?;

        let mut frame = CheckFrame::new(module_info, None, Env::new());

        let mut binding = self.check_binding(&mut frame, &binding)?;
        binding.substitute(self.infcx.get_table_mut())?;

        let new_module = self.get_or_insert_new_module(module_info);
        let binding_info = BindingInfo::from_binding(&binding);
        new_module.bindings.push(binding);

        self.processed_items_stack.pop();

        Ok(binding_info)
    }

    pub(crate) fn check_import(
        &mut self,
        calling_module: Ustr,
        import: &Import,
    ) -> DiagnosticResult<BindingInfo> {
        let mut ty = Ty::Module {
            name: import.module_info.name,
            file_path: import.module_info.file_path,
        };
        let mut const_value = None;
        let mut span = Span::unknown();

        if !import.import_path.is_empty() {
            // go over the import_path, and get the relevant symbol
            let mut current_module = import.module_info;

            for (index, symbol) in import.import_path.iter().enumerate() {
                let binding_info = self.check_top_level_binding(
                    current_module,
                    calling_module,
                    symbol.value.into_symbol(),
                    symbol.span,
                    TopLevelLookupKind::OtherModule,
                )?;

                ty = binding_info.ty;
                const_value = binding_info.const_value;
                span = binding_info.span;

                match ty {
                    Ty::Module { name, file_path } => {
                        current_module = ModuleInfo::new(name, file_path)
                    }
                    _ => {
                        if index < import.import_path.len() - 1 {
                            return Err(TypeError::type_mismatch(
                                symbol.span,
                                &Ty::Module {
                                    name: ustr(""),
                                    file_path: ustr(""),
                                },
                                &ty,
                            ));
                        }
                    }
                }
            }
        }

        Ok(BindingInfo {
            ty,
            const_value,
            is_mutable: false,
            is_init: true,
            span,
        })
    }

    fn is_item_accessible(
        &self,
        visibility: Visibility,
        symbol: Ustr,
        symbol_span: Span,
        module_info: ModuleInfo,
        calling_module: Ustr,
        calling_span: Span,
    ) -> DiagnosticResult<()> {
        if visibility == Visibility::Private
            && module_info.name != calling_module
        {
            Err(Diagnostic::error()
                .with_message(format!(
                    "associated symbol `{}` is private",
                    symbol
                ))
                .with_labels(vec![
                    Label::primary(calling_span.file_id, calling_span.range())
                        .with_message("symbol is private"),
                    Label::secondary(symbol_span.file_id, symbol_span.range())
                        .with_message("symbol defined here"),
                ]))
        } else {
            Ok(())
        }
    }
}
