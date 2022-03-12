use crate::{CheckFrame, CheckSess};
use chili_ast::ty::*;
use chili_ast::workspace::{BindingInfo, ModuleIdx};
use chili_ast::{
    ast::{Binding, Import, Visibility},
    pattern::{Pattern, SymbolPattern},
};
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

impl<'c> CheckSess<'c> {
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

        for symbol in binding.pattern.symbols() {
            let var = self.infcx.fresh_type_var();
            self.update_binding_info_ty(symbol.binding_info_idx, var.into());
        }

        let (value, const_value) = if let Some(value) = &binding.value {
            let mut result = self.check_expr(frame, value, Some(expected_var.into()))?;

            self.infcx
                .unify_or_coerce_ty_expr(&Ty::from(expected_var), &mut result.expr)?;

            (Some(result.expr), result.value)
        } else {
            (None, None)
        };

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

        self.check_binding_pattern(&binding.pattern, ty.clone(), const_value.clone())?;

        Ok(Binding {
            kind: binding.kind,
            pattern: binding.pattern.clone(),
            ty_expr,
            ty,
            value,
            visibility: binding.visibility,
            const_value,
            should_codegen: true,
            lib_name: binding.lib_name,
        })
    }

    #[inline]
    pub(crate) fn check_top_level_binding(
        &mut self,
        binding: &mut Binding,
        calling_module_idx: ModuleIdx,
        calling_symbol_span: Span,
    ) -> DiagnosticResult<()> {
        let idx = binding.pattern.into_single().binding_info_idx;

        let binding_info = self.workspace.get_binding_info(idx).unwrap().clone();

        if !binding_info.ty.is_unknown() {
            return Ok(());
        }

        self.is_item_accessible(&binding_info, calling_module_idx, calling_symbol_span)?;

        let mut frame = CheckFrame::new(0, binding_info.module_idx, None);

        *binding = self.check_binding(&mut frame, binding)?;

        Ok(())
    }

    pub(crate) fn check_import(&mut self, import: &Import) -> DiagnosticResult<()> {
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

        Ok(())
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
