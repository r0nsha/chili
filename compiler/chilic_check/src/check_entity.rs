use crate::{
    AnalysisContext, AnalysisFrame, EntityInfo, ProcessedItem,
    TopLevelLookupKind,
};
use chilic_ast::{
    ast::{Entity, EntityKind, Module, ModuleInfo, Use, Visibility},
    pattern::{Pattern, SymbolPattern},
    value::Value,
};
use chilic_error::{DiagnosticResult, TypeError};
use chilic_infer::substitute::Substitute;
use chilic_span::Span;
use chilic_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::env::Env;
use ustr::{ustr, Ustr};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_entity(
        &mut self,
        frame: &mut AnalysisFrame,
        entity: &Entity,
    ) -> DiagnosticResult<Entity> {
        let (ty_expr, expected_var) = match &entity.ty_expr {
            Some(expr) => {
                let type_expr = self.check_type_expr(frame, expr)?;
                let ty = type_expr.value.unwrap().into_type();
                (Some(type_expr.expr), self.infcx.fresh_bound_type_var(ty))
            }
            None => (None, self.infcx.fresh_type_var()),
        };

        let (value, const_value) = if let Some(value) = &entity.value {
            let mut result =
                self.check_expr(frame, value, Some(expected_var.into()))?;

            let is_a_type =
                result.value.as_ref().map_or(false, |v| v.is_type());

            match &entity.kind {
                EntityKind::Value => {
                    if is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            &self.infcx.normalize_ty_and_untyped(&result.ty),
                            "a value",
                        ));
                    }
                }
                EntityKind::Type => {
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

        // * don't allow types to be bounded to mutable entities
        if const_value.as_ref().map_or(false, |v| v.is_type()) {
            match &entity.pattern {
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

        // * don't allow const values with mutable entities
        let const_value = match &entity.pattern {
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

        self.check_entity_pattern(
            frame,
            &entity.pattern,
            ty,
            const_value.clone(),
            value.is_some() || const_value.is_some(),
        )?;

        Ok(Entity {
            kind: entity.kind,
            pattern: entity.pattern.clone(),
            ty_expr,
            ty: expected_var.into(),
            value,
            visibility: entity.visibility,
            const_value,
            should_codegen: self.in_main_path
                || frame.module_info.name.starts_with("std"), /* TODO: this
                                                               * could cause
                                                               * bugs */
            lib_name: entity.lib_name,
        })
    }

    pub(crate) fn check_top_level_entity(
        &mut self,
        module_info: ModuleInfo,
        calling_module: Ustr,
        symbol: Ustr,
        symbol_span: Span,
        lookup_kind: TopLevelLookupKind,
    ) -> DiagnosticResult<EntityInfo> {
        // if the entity is already in new_ir, get its type instead
        match self.new_ir.modules.get(&module_info.name) {
            Some(module) => match module.find_entity(symbol) {
                Some(entity) => {
                    let SymbolPattern { symbol, span, .. } =
                        entity.pattern.into_single();

                    self.is_item_accessible(
                        entity.visibility,
                        symbol,
                        span,
                        module_info,
                        calling_module,
                        symbol_span,
                    )?;

                    return Ok(EntityInfo::from_entity(entity));
                }
                None => (),
            },
            None => (),
        };

        // entity has not been checked yet
        let module = self
            .old_ir
            .modules
            .get(&module_info.name)
            .expect(&format!("couldn't find module `{}`", module_info.name));

        match module.find_entity(symbol) {
            Some(entity) => self.check_top_level_entity_internal(
                module_info,
                entity,
                calling_module,
                symbol_span,
            ),
            None => {
                match module.find_use(symbol) {
                    Some(use_) => {
                        let new_module =
                            self.get_or_insert_new_module(module_info);

                        if let None = new_module.find_use(use_.alias) {
                            new_module.uses.push(use_.clone());
                        }

                        self.is_item_accessible(
                            use_.visibility,
                            use_.alias,
                            use_.span(),
                            module_info,
                            calling_module,
                            symbol_span,
                        )?;

                        return self.check_use(calling_module, &use_);
                    }
                    None => (),
                }

                // * search builtin symbols
                match self.builtin_types.get(&symbol) {
                    Some(ty) => Ok(EntityInfo {
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
    pub(crate) fn check_top_level_entity_internal(
        &mut self,
        module_info: ModuleInfo,
        entity: &Entity,
        calling_module: Ustr,
        calling_span: Span,
    ) -> DiagnosticResult<EntityInfo> {
        let SymbolPattern { symbol, span, .. } = entity.pattern.into_single();

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
            entity.visibility,
            symbol,
            span,
            module_info,
            calling_module,
            calling_span,
        )?;

        let mut frame = AnalysisFrame::new(module_info, None, Env::new());

        let mut entity = self.check_entity(&mut frame, &entity)?;
        entity.substitute(self.infcx.get_table_mut())?;

        let new_module = self.get_or_insert_new_module(module_info);
        let entity_info = EntityInfo::from_entity(&entity);
        new_module.entities.push(entity);

        self.processed_items_stack.pop();

        Ok(entity_info)
    }

    pub(crate) fn check_use(
        &mut self,
        calling_module: Ustr,
        use_: &Use,
    ) -> DiagnosticResult<EntityInfo> {
        let mut ty = Ty::Module {
            name: use_.module_info.name,
            file_path: use_.module_info.file_path,
        };
        let mut const_value = None;
        let mut span = Span::unknown();

        if !use_.use_path.is_empty() {
            // go over the use_path, and get the relevant symbol
            let mut current_module = use_.module_info;

            for (index, symbol) in use_.use_path.iter().enumerate() {
                let entity_info = self.check_top_level_entity(
                    current_module,
                    calling_module,
                    symbol.value.into_symbol(),
                    symbol.span,
                    TopLevelLookupKind::OtherModule,
                )?;

                ty = entity_info.ty;
                const_value = entity_info.const_value;
                span = entity_info.span;

                match ty {
                    Ty::Module { name, file_path } => {
                        current_module = ModuleInfo::new(name, file_path)
                    }
                    _ => {
                        if index < use_.use_path.len() - 1 {
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

        Ok(EntityInfo {
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
