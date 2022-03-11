use chili_ast::ty::*;
use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::{ustr, Ustr, UstrMap};

use crate::{CheckFrame, CheckResult, CheckSess};
use chili_ast::{
    ast::{Expr, ExprKind, Fn, FnParam, Proto},
    pattern::{Pattern, SymbolPattern},
};

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_fn(
        &mut self,
        frame: &mut CheckFrame,
        func: &mut Fn,
        span: Span,
        expected_ty: Option<TyKind>,
    ) -> DiagnosticResult<CheckResult> {
        let proto_ty = self.check_proto(frame, &mut func.proto, expected_ty, span)?;

        let ty = proto_ty.into_fn();

        let mut fn_frame = CheckFrame::new(frame.depth, frame.module_idx, Some(*ty.ret.clone()));

        if let Some(idx) = func.proto.binding_info_idx {
            self.update_binding_info_ty(idx, proto_ty.clone());
        }

        for (index, param) in func.proto.params.iter().enumerate() {
            let param_ty = self.infcx.normalize_ty(&ty.params[index].ty);
            self.check_binding_pattern(&mut fn_frame, &param.pattern, param_ty, None, true)?;
        }

        self.init_scopes.push_scope();

        let result_ty = self.check_block(&mut fn_frame, &mut func.body, Some(proto_ty.clone()))?;

        self.init_scopes.pop_scope();

        let last_stmt_span = match func.body.exprs.last() {
            Some(stmt) => stmt.span,
            None => span,
        };

        let result_ty = self.infcx.normalize_ty(&result_ty);

        if !result_ty.is_never() {
            if func.body.exprs.is_empty() {
                self.infcx
                    .unify(ty.ret.as_ref().clone(), TyKind::Unit, last_stmt_span)?;
            } else {
                if func.body.yields && !ty.ret.is_unit() {
                    let last_expr_mut = func.body.exprs.last_mut().unwrap();
                    self.infcx
                        .unify_or_coerce_ty_expr(ty.ret.as_ref(), last_expr_mut)?;
                } else {
                    self.infcx
                        .unify(ty.ret.as_ref().clone(), TyKind::Unit, span)?;
                }
            }
        }

        let ty = self.infcx.normalize_ty(&proto_ty);
        let fn_ty = ty.into_fn().clone();

        if func.is_startup
            && (!(fn_ty.ret.is_unit() || fn_ty.ret.is_never())
                || !fn_ty.params.is_empty()
                || fn_ty.variadic)
        {
            return Err(Diagnostic::error()
                .with_message("entry point function `main` has wrong type, expected `fn() -> ()`")
                .with_labels(vec![Label::primary(span.file_id, span.range().clone())]));
        }

        Ok(ty)
    }

    pub(crate) fn check_proto(
        &mut self,
        frame: &mut CheckFrame,
        proto: &mut Proto,
        expected_ty: Option<TyKind>,
        span: Span,
    ) -> DiagnosticResult<CheckResult> {
        let mut expected_fn_ty = expected_ty
            .as_ref()
            .map(|t| self.infcx.normalize_ty(t))
            .and_then(|t| {
                if t.is_fn() {
                    Some(t.into_fn().clone())
                } else {
                    None
                }
            });

        let mut param_tys = vec![];
        let mut param_name_map = UstrMap::default();

        let mut check_symbol = |symbol: Ustr, span: Span| {
            if let Some(already_defined_span) = param_name_map.insert(symbol, span) {
                Err(SyntaxError::duplicate_symbol(
                    already_defined_span,
                    span,
                    symbol,
                ))
            } else {
                Ok(())
            }
        };

        for (index, param) in proto.params.iter_mut().enumerate() {
            match &param.pattern {
                Pattern::Single(SymbolPattern {
                    symbol,
                    span,
                    ignore,
                    ..
                }) => {
                    if !ignore {
                        if let Err(e) = check_symbol(*symbol, *span) {
                            return Err(e);
                        }
                    }
                }
                Pattern::StructDestructor(destructor) | Pattern::TupleDestructor(destructor) => {
                    for SymbolPattern {
                        symbol,
                        alias,
                        span,
                        ignore,
                        ..
                    } in destructor.symbols.iter()
                    {
                        if !ignore {
                            if let Err(e) = check_symbol(alias.unwrap_or(*symbol), *span) {
                                return Err(e);
                            }
                        }
                    }
                }
            }

            let ty = if let Some(ty_expr) = &mut param.ty {
                ty_expr.ty = self.check_type_expr(frame, ty_expr)?;
                ty_expr.ty.clone()
            } else {
                match &mut expected_fn_ty {
                    Some(expected_fn_ty) => {
                        // infer this param's type from the expected param's
                        // type
                        let expected_param_ty = expected_fn_ty.params[index].ty.clone();
                        param.ty = Some(Box::new(Expr::typed(
                            ExprKind::Noop,
                            expected_param_ty.clone().create_type(),
                            param.pattern.span().clone(),
                        )));
                        expected_param_ty
                    }
                    None => {
                        let span = param.pattern.span();
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "can't infer the parameter type for `{}`",
                                param.pattern
                            ))
                            .with_labels(vec![Label::primary(
                                span.file_id,
                                span.range().clone(),
                            )]));
                    }
                }
            };

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
        if proto.params.is_empty() {
            if let Some(expected_fn_ty) = &expected_fn_ty {
                if expected_fn_ty.params.len() == 1 {
                    let expected_param_ty = &expected_fn_ty.params[0].ty;
                    let symbol = ustr("it");

                    let pattern = Pattern::Single(SymbolPattern {
                        binding_info_idx: Default::default(),
                        symbol,
                        alias: None,
                        span: span,
                        is_mutable: false,
                        ignore: false,
                    });

                    proto.params.push(FnParam {
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

        let ret_ty = match &mut proto.ret {
            Some(ret) => {
                ret.ty = self.check_type_expr(frame, ret)?;
                ret.ty.clone()
            }
            None => match expected_fn_ty {
                Some(expected_fn_ty) => expected_fn_ty.ret.as_ref().clone(),
                None => TyKind::Unit,
            },
        };

        let fn_ty = TyKind::Fn(FnTy {
            params: param_tys,
            ret: Box::new(ret_ty),
            variadic: proto.variadic,
            lib_name: proto.lib_name,
        });

        if let Some(idx) = proto.binding_info_idx {
            self.update_binding_info_ty(idx, fn_ty.clone());
        }

        Ok(fn_ty)
    }
}
