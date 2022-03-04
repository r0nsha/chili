use chili_error::{DiagnosticResult, SyntaxError};
use chili_span::Span;
use chili_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::{ustr, Ustr, UstrMap};

use crate::{AnalysisContext, AnalysisFrame};
use chili_ast::{
    ast::{Expr, ExprKind, Fn, FnParam, Proto},
    pattern::{Pattern, SymbolPattern},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_fn(
        &mut self,
        frame: &mut AnalysisFrame,
        func: &Fn,
        span: Span,
        expected_ty: Option<Ty>,
    ) -> DiagnosticResult<Fn> {
        let proto = self.check_proto(frame, &func.proto, expected_ty, span)?;

        let ty = proto.ty.into_fn();

        let mut fn_frame = AnalysisFrame::new(
            frame.module_info,
            Some(*ty.ret.clone()),
            frame.env.clone(),
        );

        fn_frame.insert_binding(proto.name, proto.ty.clone(), span, true);

        for (index, param) in proto.params.iter().enumerate() {
            let param_ty = self.infcx.normalize_ty(&ty.params[index].ty);
            self.check_binding_pattern(
                &mut fn_frame,
                &param.pattern,
                param_ty,
                None,
                true,
            )?;
        }

        fn_frame.push_named_scope(proto.name);

        let (mut body, result_ty) = self.check_block(
            &mut fn_frame,
            &func.body,
            Some(proto.ty.clone()),
        )?;

        fn_frame.pop_scope();

        let last_stmt_span = match func.body.exprs.last() {
            Some(stmt) => stmt.span,
            None => span,
        };

        let result_ty = self.infcx.normalize_ty(&result_ty);

        if !result_ty.is_never() {
            if body.exprs.is_empty() {
                self.infcx.unify(
                    ty.ret.as_ref().clone(),
                    Ty::Unit,
                    last_stmt_span,
                )?;
            } else {
                if body.yields && !ty.ret.is_unit() {
                    let last_expr_mut = body.exprs.last_mut().unwrap();
                    self.infcx.unify_or_coerce_ty_expr(
                        ty.ret.as_ref(),
                        last_expr_mut,
                        last_stmt_span,
                    )?;
                } else {
                    self.infcx.unify(
                        ty.ret.as_ref().clone(),
                        Ty::Unit,
                        span,
                    )?;
                }
            }
        }

        let fn_ty = self.infcx.normalize_ty(&proto.ty).into_fn().clone();

        if func.is_startup
            && (!(fn_ty.ret.is_unit() || fn_ty.ret.is_never())
                || !fn_ty.params.is_empty()
                || fn_ty.variadic)
        {
            return Err(Diagnostic::error()
                .with_message("entry point function `main` has wrong type, expected `fn() -> ()`")
                .with_labels(vec![Label::primary(span.file_id, span.range().clone())]));
        }

        Ok(Fn {
            proto,
            body,
            is_startup: func.is_startup,
        })
    }

    pub(crate) fn check_proto(
        &mut self,
        frame: &mut AnalysisFrame,
        proto: &Proto,
        expected_ty: Option<Ty>,
        span: Span,
    ) -> DiagnosticResult<Proto> {
        let expected_fn_ty = expected_ty
            .as_ref()
            .map(|t| self.infcx.normalize_ty(t))
            .and_then(|t| {
                if t.is_fn() {
                    Some(t.into_fn().clone())
                } else {
                    None
                }
            });

        let mut params = vec![];
        let mut param_tys = vec![];
        let mut param_name_map = UstrMap::default();

        let mut check_symbol = |symbol: Ustr, span: Span| {
            if let Some(already_defined_span) =
                param_name_map.insert(symbol, span)
            {
                Err(SyntaxError::duplicate_symbol(
                    already_defined_span,
                    span,
                    symbol,
                ))
            } else {
                Ok(())
            }
        };

        for (index, param) in proto.params.iter().enumerate() {
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
                Pattern::StructDestructor(destructor)
                | Pattern::TupleDestructor(destructor) => {
                    for SymbolPattern {
                        symbol,
                        alias,
                        span,
                        ignore,
                        ..
                    } in destructor.symbols.iter()
                    {
                        if !ignore {
                            if let Err(e) =
                                check_symbol(alias.unwrap_or(*symbol), *span)
                            {
                                return Err(e);
                            }
                        }
                    }
                }
            }

            let (type_expr, ty) = if let Some(ty) = &param.ty {
                let type_expr = self.check_type_expr(frame, ty)?;
                let ty = type_expr.value.unwrap().into_type();
                (Box::new(type_expr.expr), ty)
            } else {
                match expected_fn_ty {
                    Some(ref expected_fn_ty) => {
                        // infer this param's type from the expected param's
                        // type
                        let expected_param_ty =
                            expected_fn_ty.params[index].ty.clone();
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
                            .with_labels(vec![Label::primary(
                                span.file_id,
                                span.range().clone(),
                            )]));
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

        let (ret_expr, ret_ty) = match &proto.ret {
            Some(ret) => {
                let type_expr = self.check_type_expr(frame, ret)?;
                let ty = type_expr.value.unwrap().into_type();
                (Some(Box::new(type_expr.expr)), ty)
            }
            None => match expected_fn_ty {
                Some(expected_fn_ty) => {
                    (None, expected_fn_ty.ret.as_ref().clone())
                }
                None => (None, Ty::Unit),
            },
        };

        let fn_ty = Ty::Fn(FnTy {
            params: param_tys,
            ret: Box::new(ret_ty),
            variadic: proto.variadic,
            lib_name: proto.lib_name,
        });

        Ok(Proto {
            name: proto.name,
            params,
            variadic: proto.variadic,
            ret: ret_expr,
            lib_name: proto.lib_name,
            ty: fn_ty.clone(),
        })
    }
}
