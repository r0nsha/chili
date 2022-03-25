use chili_ast::ty::*;
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::ustr;

use crate::{CheckFrame, CheckSess};
use chili_ast::{
    ast::{Expr, ExprKind, Fn, FnParam, FnSig},
    pattern::{Pattern, SymbolPattern},
};

impl<'c> CheckSess<'c> {
    pub(crate) fn check_fn(
        &mut self,
        frame: &mut CheckFrame,
        func: &Fn,
        span: Span,
        expected_ty: Option<Ty>,
    ) -> DiagnosticResult<Fn> {
        let sig = self.check_fn_sig(frame, &func.sig, expected_ty, span)?;

        let ty = sig.ty.as_fn();

        let mut fn_frame = CheckFrame::new(frame.depth, frame.module_id, Some(*ty.ret.clone()));

        if let Some(id) = func.sig.binding_info_id {
            self.update_binding_info_ty(id, sig.ty.clone());
        }

        for (index, param) in sig.params.iter().enumerate() {
            let param_ty = self.infcx.normalize_ty(&ty.params[index].ty);
            self.check_binding_pattern(&param.pattern, param_ty, None)?;
        }

        let (mut body, result_ty) =
            self.check_block(&mut fn_frame, &func.body, Some(sig.ty.clone()))?;

        let last_stmt_span = match func.body.exprs.last() {
            Some(stmt) => stmt.span,
            None => span,
        };

        let result_ty = self.infcx.normalize_ty(&result_ty);

        if !result_ty.is_never() {
            if body.exprs.is_empty() {
                self.infcx
                    .unify(ty.ret.as_ref().clone(), Ty::Unit, last_stmt_span)?;
            } else {
                if body.yields && !ty.ret.is_unit() {
                    let last_expr_mut = body.exprs.last_mut().unwrap();
                    self.infcx
                        .unify_or_coerce_ty_expr(ty.ret.as_ref(), last_expr_mut)?;
                } else {
                    self.infcx.unify(ty.ret.as_ref().clone(), Ty::Unit, span)?;
                }
            }
        }

        Ok(Fn {
            sig,
            body,
            is_entry_point: func.is_entry_point,
        })
    }

    pub(crate) fn check_fn_sig(
        &mut self,
        frame: &mut CheckFrame,
        sig: &FnSig,
        expected_ty: Option<Ty>,
        span: Span,
    ) -> DiagnosticResult<FnSig> {
        let expected_fn_ty = expected_ty
            .as_ref()
            .map(|t| self.infcx.normalize_ty(t))
            .and_then(|t| {
                if t.is_fn() {
                    Some(t.as_fn().clone())
                } else {
                    None
                }
            });

        let mut params = vec![];
        let mut param_tys = vec![];

        for (index, param) in sig.params.iter().enumerate() {
            let (type_expr, ty) = if let Some(ty) = &param.ty {
                let type_expr = self.check_type_expr(frame, ty)?;
                let ty = type_expr.value.unwrap().into_type();
                (Box::new(type_expr.expr), ty)
            } else {
                match expected_fn_ty {
                    Some(ref expected_fn_ty) => {
                        // infer this param's type from the expected param's
                        // type
                        let expected_param_ty = expected_fn_ty.params[index].ty.clone();
                        (
                            Box::new(Expr::typed(
                                ExprKind::Noop,
                                expected_param_ty.clone().create_type(),
                                param.pattern.span().clone(),
                            )),
                            expected_param_ty,
                        )
                    }
                    None => {
                        let span = param.pattern.span();
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "can't infer the parameter type for `{}`",
                                param.pattern
                            ))
                            .with_labels(vec![Label::primary(span.file_id, span.range())]));
                    }
                }
            };

            params.push(FnParam {
                pattern: param.pattern.clone(),
                ty: Some(type_expr),
            });

            param_tys.push(FnTyParam {
                symbol: if param.pattern.is_single() {
                    param.pattern.into_single().symbol
                } else {
                    ustr("")
                },
                ty,
            });
        }

        // if expected function is fn(int) -> int, and this function is fn() ->
        // int, insert the expected functions param as `it`
        // example:
        //
        // let map = fn(fn(int) -> int) ...
        //
        // ...
        //
        // # These will be the same
        // map fn(it) => it * 2
        // map fn => it * 2
        //
        if params.is_empty() {
            if let Some(expected_fn_ty) = &expected_fn_ty {
                if expected_fn_ty.params.len() == 1 {
                    let expected_param_ty = &expected_fn_ty.params[0].ty;
                    let symbol = ustr("it");

                    let pattern = Pattern::Single(SymbolPattern {
                        binding_info_id: Default::default(),
                        symbol,
                        alias: None,
                        span: span,
                        is_mutable: false,
                        ignore: false,
                    });

                    params.push(FnParam {
                        pattern: pattern.clone(),
                        ty: Some(Box::new(Expr::typed(
                            ExprKind::Noop,
                            expected_param_ty.clone().create_type(),
                            span,
                        ))),
                    });

                    param_tys.push(FnTyParam {
                        symbol,
                        ty: expected_param_ty.clone(),
                    });
                }
            }
        }

        let (ret_expr, ret_ty) = match &sig.ret {
            Some(ret) => {
                let type_expr = self.check_type_expr(frame, ret)?;
                let ty = type_expr.value.unwrap().into_type();
                (Some(Box::new(type_expr.expr)), ty)
            }
            None => match expected_fn_ty {
                Some(expected_fn_ty) => (None, expected_fn_ty.ret.as_ref().clone()),
                None => (None, Ty::Unit),
            },
        };

        let fn_ty = Ty::Fn(FnTy {
            params: param_tys,
            ret: Box::new(ret_ty),
            variadic: sig.variadic,
            lib_name: sig.lib_name,
        });

        Ok(FnSig {
            binding_info_id: sig.binding_info_id,
            name: sig.name,
            params,
            variadic: sig.variadic,
            ret: ret_expr,
            lib_name: sig.lib_name,
            ty: fn_ty.clone(),
        })
    }
}
