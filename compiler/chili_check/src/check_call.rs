use chili_ast::ast::{Call, CallArg, ExprKind};
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use chili_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::UstrMap;

use crate::{CheckContext, CheckFrame, CheckedExpr};

impl<'a> CheckContext<'a> {
    pub(crate) fn check_call(
        &mut self,
        frame: &mut CheckFrame,
        call: &Call,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let callee = self.check_expr(frame, &call.callee, None)?;
        let ty = self.infcx.normalize_ty(&callee.ty);

        match ty {
            Ty::Fn(fn_type) => {
                self.check_call_fn(frame, &fn_type, call, callee, span)
            }
            _ => Err(TypeError::expected(call.callee.span, &ty, "a function")),
        }
    }

    fn check_call_fn(
        &mut self,
        frame: &mut CheckFrame,
        fn_type: &FnTy,
        call: &Call,
        callee: CheckedExpr,
        span: Span,
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

                if let Some(passed_span) =
                    passed_args.insert(symbol.value, symbol.span)
                {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "duplicate argument `{}`",
                            symbol.value
                        ))
                        .with_labels(vec![
                            Label::primary(
                                symbol.span.file_id,
                                symbol.span.range().clone(),
                            )
                            .with_message("duplicate passed here"),
                            Label::secondary(
                                passed_span.file_id,
                                passed_span.range(),
                            )
                            .with_message("has already been passed here"),
                        ]));
                }

                let found_param_index = fn_type
                    .params
                    .iter()
                    .position(|p| p.symbol == symbol.value);
                if let Some(index) = found_param_index {
                    let param = &fn_type.params[index];

                    let mut arg = self.check_expr(
                        frame,
                        &arg.value,
                        Some(param.ty.clone()),
                    )?;

                    let param_ty = self.infcx.normalize_ty(&param.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &param_ty,
                        &mut arg.expr,
                        call.args[index].value.span,
                    )?;

                    arg
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "unknown argument `{}`",
                            symbol.value
                        ))
                        .with_labels(vec![Label::primary(
                            symbol.span.file_id,
                            symbol.span.range().clone(),
                        )]));
                }
            } else {
                // * this is a positional argument
                if let Some(param) = fn_type.params.get(index) {
                    passed_args.insert(param.symbol, arg.value.span);

                    let mut arg = self.check_expr(
                        frame,
                        &arg.value,
                        Some(param.ty.clone()),
                    )?;

                    let param_ty = self.infcx.normalize_ty(&param.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &param_ty,
                        &mut arg.expr,
                        call.args[index].value.span,
                    )?;

                    arg
                } else {
                    // * this is a variadic argument, meaning that the
                    //   argument's
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
}
