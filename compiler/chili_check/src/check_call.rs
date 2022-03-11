use chili_ast::ast::{Call, Expr};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::UstrMap;

use crate::{CheckFrame, CheckSess};

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_call(
        &mut self,
        frame: &mut CheckFrame,
        call: &mut Call,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        call.callee.ty = self.check_expr(frame, &mut call.callee, None)?;
        call.callee.ty = self.infcx.normalize_ty(&call.callee.ty);

        match &call.callee.ty {
            Ty::Fn(fn_type) => self.check_call_fn(frame, &fn_type, call, &call.callee, span),
            ty => Err(TypeError::expected(
                call.callee.span,
                ty.to_string(),
                "a function",
            )),
        }
    }

    fn check_call_fn(
        &mut self,
        frame: &mut CheckFrame,
        fn_type: &FnTy,
        call: &Call,
        callee: &Expr,
        span: Span,
    ) -> DiagnosticResult<Ty> {
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

        let mut passed_args = UstrMap::default();

        for (index, arg) in call.args.iter().enumerate() {
            let new_arg = if let Some(symbol) = &arg.symbol {
                // * this is a named argument

                if let Some(passed_span) = passed_args.insert(symbol.value, symbol.span) {
                    return Err(Diagnostic::error()
                        .with_message(format!("duplicate argument `{}`", symbol.value))
                        .with_labels(vec![
                            Label::primary(symbol.span.file_id, symbol.span.range().clone())
                                .with_message("duplicate passed here"),
                            Label::secondary(passed_span.file_id, passed_span.range())
                                .with_message("has already been passed here"),
                        ]));
                }

                let found_param_index =
                    fn_type.params.iter().position(|p| p.symbol == symbol.value);

                if let Some(index) = found_param_index {
                    let param = &fn_type.params[index];

                    arg.value.ty =
                        self.check_expr(frame, &mut arg.value, Some(param.ty.clone()))?;

                    let param_ty = self.infcx.normalize_ty(&param.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &param_ty,
                        &mut arg.value,
                        call.args[index].value.span,
                    )?;
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!("unknown argument `{}`", symbol.value))
                        .with_labels(vec![Label::primary(
                            symbol.span.file_id,
                            symbol.span.range().clone(),
                        )]));
                }
            } else {
                // * this is a positional argument
                if let Some(param) = fn_type.params.get(index) {
                    passed_args.insert(param.symbol, arg.value.span);

                    arg.value.ty =
                        self.check_expr(frame, &mut arg.value, Some(param.ty.clone()))?;

                    let param_ty = self.infcx.normalize_ty(&param.ty);

                    self.infcx.unify_or_coerce_ty_expr(
                        &param_ty,
                        &mut arg.value,
                        call.args[index].value.span,
                    )?;
                } else {
                    // * this is a variadic argument, meaning that the argument's
                    // * index is greater than the function param length
                    self.check_expr(frame, &mut arg.value, None)?;
                }
            };
        }

        Ok(fn_type.ret.as_ref().clone())
    }
}
