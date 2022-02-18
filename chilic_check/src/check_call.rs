use chilic_error::{DiagnosticResult, TypeError};
use chilic_ir::expr::{Call, CallArg, ExprKind};
use chilic_span::Span;
use chilic_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::{UstrMap, UstrSet};

use crate::{AnalysisContext, AnalysisFrame, CheckedExpr};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_call(
        &mut self,
        frame: &mut AnalysisFrame,
        call: &Call,
        span: &Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let callee = self.check_expr(frame, &call.callee, None)?;
        let ty = self.infcx.normalize_ty(&callee.ty);

        match ty {
            Ty::Fn(fn_type) => self.check_call_fn(frame, &fn_type, call, callee, span),
            _ => {
                let is_type = callee.value.as_ref().map_or(false, |v| v.is_type());
                if is_type && callee.value.clone().unwrap().into_type().is_struct() {
                    self.check_call_struct(frame, call, callee, span)
                } else {
                    Err(TypeError::expected(
                        &call.callee.span,
                        &ty,
                        "function or struct",
                    ))
                }
            }
        }
    }

    fn check_call_fn(
        &mut self,
        frame: &mut AnalysisFrame,
        fn_type: &FnTy,
        call: &Call,
        callee: CheckedExpr,
        span: &Span,
    ) -> DiagnosticResult<CheckedExpr> {
        if fn_type.variadic {
            if call.args.len() < fn_type.params.len() {
                return Err(TypeError::fn_call_arity_mismatch(
                    span,
                    fn_type.params.len(),
                    call.args.len(),
                ));
            }
        } else if call.args.len() != fn_type.params.len() {
            return Err(TypeError::fn_call_arity_mismatch(
                span,
                fn_type.params.len(),
                call.args.len(),
            ));
        }

        let mut new_args = vec![];
        let mut passed_args = UstrMap::default();

        for (index, arg) in call.args.iter().enumerate() {
            let new_arg = if let Some(symbol) = &arg.symbol {
                // * this is a named argument

                if let Some(passed_span) = passed_args.insert(symbol.value, symbol.span.clone()) {
                    return Err(Diagnostic::error()
                        .with_message(format!("duplicate argument `{}`", symbol.value))
                        .with_labels(vec![
                            Label::primary(symbol.span.file_id, symbol.span.range.clone())
                                .with_message("duplicate passed here"),
                            Label::secondary(passed_span.file_id, passed_span.range)
                                .with_message("has already been passed here"),
                        ]));
                }

                let found_param_index =
                    fn_type.params.iter().position(|p| p.symbol == symbol.value);
                if let Some(index) = found_param_index {
                    let param = &fn_type.params[index];

                    let mut arg = self.check_expr(frame, &arg.value, Some(param.ty.clone()))?;

                    let param_ty = self.infcx.normalize_ty(&param.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &param_ty,
                        &mut arg.expr,
                        &call.args[index].value.span,
                    )?;

                    arg
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!("unknown argument `{}`", symbol.value))
                        .with_labels(vec![Label::primary(
                            symbol.span.file_id,
                            symbol.span.range.clone(),
                        )]));
                }
            } else {
                // * this is a positional argument
                if let Some(param) = fn_type.params.get(index) {
                    passed_args.insert(param.symbol, arg.value.span.clone());

                    let mut arg = self.check_expr(frame, &arg.value, Some(param.ty.clone()))?;

                    let param_ty = self.infcx.normalize_ty(&param.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &param_ty,
                        &mut arg.expr,
                        &call.args[index].value.span,
                    )?;

                    arg
                } else {
                    // * this is a variadic argument, meaning that the argument's
                    // * index is greater than the function param length
                    self.check_expr(frame, &arg.value, None)?
                }
            };

            new_args.push(CallArg {
                symbol: arg.symbol.clone(),
                value: new_arg.expr,
            });
        }

        Ok(CheckedExpr::new(
            ExprKind::Call(Call {
                callee: Box::new(callee.expr),
                args: new_args,
            }),
            fn_type.ret.as_ref().clone(),
            None,
            span,
        ))
    }

    fn check_call_struct(
        &mut self,
        frame: &mut AnalysisFrame,
        call: &Call,
        callee: CheckedExpr,
        span: &Span,
    ) -> DiagnosticResult<CheckedExpr> {
        // * struct call-initialization syntax

        let ty = callee.value.unwrap().into_type();
        let struct_type = ty.into_struct();

        let mut new_args = vec![];
        let mut passed_args = UstrMap::default();
        let mut uninit_fields = UstrSet::from_iter(struct_type.fields.iter().map(|f| f.symbol));

        for (index, arg) in call.args.iter().enumerate() {
            let new_arg = if let Some(symbol) = &arg.symbol {
                // * this is a named argument

                if let Some(passed_span) = passed_args.insert(symbol.value, symbol.span.clone()) {
                    return Err(Diagnostic::error()
                        .with_message(format!("duplicate field `{}`", symbol.value))
                        .with_labels(vec![
                            Label::primary(symbol.span.file_id, symbol.span.range.clone())
                                .with_message("duplicate passed here"),
                            Label::secondary(passed_span.file_id, passed_span.range)
                                .with_message("has already been passed here"),
                        ]));
                }

                let found_field_index = struct_type
                    .fields
                    .iter()
                    .position(|p| p.symbol == symbol.value);

                if let Some(index) = found_field_index {
                    let field = &struct_type.fields[index];
                    uninit_fields.remove(&field.symbol);

                    let mut arg = self.check_expr(frame, &arg.value, Some(field.ty.clone()))?;

                    let field_type = self.infcx.normalize_ty(&field.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &field_type,
                        &mut arg.expr,
                        &call.args[index].value.span,
                    )?;

                    arg
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!("unknown field `{}`", symbol.value))
                        .with_labels(vec![Label::primary(
                            symbol.span.file_id,
                            symbol.span.range.clone(),
                        )]));
                }
            } else {
                // * this is a positional argument

                let field = &struct_type.fields[index];
                uninit_fields.remove(&field.symbol);

                passed_args.insert(field.symbol, arg.value.span.clone());

                let mut arg = self.check_expr(frame, &arg.value, Some(field.ty.clone()))?;

                let param_ty = self.infcx.normalize_ty(&field.ty);

                self.infcx.unify_or_coerce_ty_expr(
                    &param_ty,
                    &mut arg.expr,
                    &call.args[index].value.span,
                )?;

                arg
            };

            new_args.push(CallArg {
                symbol: arg.symbol.clone(),
                value: new_arg.expr,
            });
        }

        if struct_type.is_union() && new_args.len() != 1 {
            return Err(Diagnostic::error()
                .with_message("union should have exactly one field")
                .with_labels(vec![Label::primary(span.file_id, span.range.clone())]));
        }

        if !struct_type.is_union() && !uninit_fields.is_empty() {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "missing struct fields: {}",
                    uninit_fields
                        .iter()
                        .map(|f| f.as_str())
                        .collect::<Vec<&str>>()
                        .join(", ")
                ))
                .with_labels(vec![Label::primary(span.file_id, span.range.clone())]));
        }

        Ok(CheckedExpr::new(
            ExprKind::Call(Call {
                callee: Box::new(callee.expr),
                args: new_args,
            }),
            Ty::Struct(struct_type.clone()),
            None,
            span,
        ))
    }
}
