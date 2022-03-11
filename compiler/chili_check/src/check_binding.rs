use crate::{CheckFrame, CheckSess, InitState};
use chili_ast::ty::*;
use chili_ast::workspace::ModuleIdx;
use chili_ast::{
    ast::{Binding, BindingKind, Import, Visibility},
    pattern::{Pattern, SymbolPattern},
    workspace::BindingInfo,
};
use chili_error::{DiagnosticResult, TypeError};
use chili_infer::substitute::Substitute;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_binding(
        &mut self,
        frame: &mut CheckFrame,
        binding: &mut Binding,
    ) -> DiagnosticResult<Ty> {
        let expected_var = match &mut binding.ty_expr {
            Some(expr) => {
                let ty = self.check_type_expr(frame, expr)?;
                self.infcx.fresh_bound_type_var(ty)
            }
            None => self.infcx.fresh_type_var(),
        };

        for symbol in binding.pattern.symbols_mut() {
            let var = self.infcx.fresh_type_var();
            self.update_binding_info_ty(symbol.binding_info_idx, var.into());
            self.init_scopes.insert(
                symbol.binding_info_idx,
                if binding.value.is_some() {
                    InitState::Init
                } else {
                    InitState::NotInit
                },
            );
        }

        let mut is_a_type = binding.kind == BindingKind::Type;

        if let Some(value) = &mut binding.value {
            value.ty = self.check_expr(frame, value, Some(expected_var.into()))?;

            is_a_type = value.ty.is_type();

            match &binding.kind {
                BindingKind::Let => {
                    if is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            self.infcx.normalize_ty_and_untyped(&value.ty).to_string(),
                            "a value",
                        ));
                    }
                }
                BindingKind::Type => {
                    if !is_a_type {
                        return Err(TypeError::expected(
                            value.span,
                            self.infcx.normalize_ty_and_untyped(&value.ty).to_string(),
                            "a type",
                        ));
                    }
                }
                _ => (),
            }

            self.infcx
                .unify_or_coerce_ty_expr(&expected_var.into(), value)?;
        }

        // * don't allow types to be bounded to mutable bindings
        if is_a_type {
            match &binding.pattern {
                Pattern::Single(SymbolPattern {
                    span, is_mutable, ..
                }) => {
                    if *is_mutable {
                        return Err(Diagnostic::error()
                            .with_message("variable of type `type` must be immutable")
                            .with_labels(vec![Label::primary(span.file_id, span.range().clone())])
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

        binding.ty = self.infcx.normalize_ty(&expected_var.into());

        self.check_binding_pattern(
            frame,
            &binding.pattern,
            binding.ty.clone(),
            None,
            binding.value.is_some(),
        )?;

        Ok(binding.ty.clone())
    }

    pub(crate) fn check_top_level_binding(
        &mut self,
        binding: &mut Binding,
        calling_module_idx: ModuleIdx,
        calling_symbol_span: Span,
    ) -> DiagnosticResult<Ty> {
        let idx = binding.pattern.into_single().binding_info_idx;

        let binding_info = self.workspace.get_binding_info(idx).unwrap().clone();

        if !binding_info.ty.is_unknown() {
            return Ok(binding_info.ty.clone());
        }

        self.is_item_accessible(&binding_info, calling_module_idx, calling_symbol_span)?;

        let mut frame = CheckFrame::new(0, binding_info.module_idx, None);

        self.check_binding(&mut frame, binding)?;
        binding.substitute(self.infcx.get_table_mut())?;

        self.update_binding_info_ty(idx, binding.ty.clone());

        println!("{}", binding.ty);

        Ok(binding.ty.clone())
    }

    pub(crate) fn check_import(&mut self, import: &mut Import) -> DiagnosticResult<Ty> {
        let mut ty = Ty::Module(import.module_idx);

        if !import.import_path.is_empty() {
            // go over the import_path, and get the relevant symbol
            let mut current_module_idx = import.module_idx;

            for (index, symbol) in import.import_path.iter().enumerate() {
                let binding_info = self.find_binding_info_in_module(
                    current_module_idx,
                    symbol.value.as_symbol(),
                    symbol.span,
                )?;

                ty = binding_info.ty.clone();

                match ty {
                    Ty::Module(idx) => current_module_idx = idx,
                    _ => {
                        if index < import.import_path.len() - 1 {
                            return Err(TypeError::type_mismatch(
                                symbol.span,
                                Ty::Module(Default::default()).to_string(),
                                ty.to_string(),
                            ));
                        }
                    }
                }
            }
        }

        self.update_binding_info_ty(import.binding_info_idx, ty.clone());

        Ok(ty)
    }

    pub fn find_binding_info_in_module(
        &self,
        module_idx: ModuleIdx,
        symbol: Ustr,
        symbol_span: Span,
    ) -> DiagnosticResult<&BindingInfo> {
        match self
            .workspace
            .binding_infos
            .iter()
            .find(|b| b.module_idx == module_idx && b.symbol == symbol)
        {
            Some(b) => Ok(b),
            None => {
                let module_info = self.workspace.get_module_info(module_idx).unwrap();
                Err(Diagnostic::error()
                    .with_message(format!(
                        "cannot find value `{}` in module `{}`",
                        symbol, module_info.name
                    ))
                    .with_labels(vec![Label::primary(
                        symbol_span.file_id,
                        symbol_span.range(),
                    )]))
            }
        }
    }

    fn is_item_accessible(
        &self,
        binding_info: &BindingInfo,
        calling_module_idx: ModuleIdx,
        calling_symbol_span: Span,
    ) -> DiagnosticResult<()> {
        if binding_info.visibility == Visibility::Private
            && binding_info.module_idx != calling_module_idx
        {
            Err(Diagnostic::error()
                .with_message(format!(
                    "associated symbol `{}` is private",
                    binding_info.symbol
                ))
                .with_labels(vec![
                    Label::primary(calling_symbol_span.file_id, calling_symbol_span.range())
                        .with_message("symbol is private"),
                    Label::secondary(binding_info.span.file_id, binding_info.span.range())
                        .with_message("symbol defined here"),
                ]))
        } else {
            Ok(())
        }
    }
}
