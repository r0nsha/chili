use crate::{CheckFrame, CheckSess, CheckedExpr};
use chili_ast::ty::*;
use chili_ast::{
    ast::{
        ArrayLiteralKind, Block, Builtin, Cast, Expr, ExprKind, ForIter, LiteralKind,
        StructLiteralField, StructType, StructTypeField,
    },
    value::Value,
};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_infer::{cast::ty_can_be_casted, infer::InferenceValue};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use ustr::{ustr, Ustr, UstrMap, UstrSet};

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_expr(
        &mut self,
        frame: &mut CheckFrame,
        expr: &Expr,
        expected_ty: Option<Ty>,
    ) -> DiagnosticResult<CheckedExpr> {
        let checked_expr = match &expr.kind {
            ExprKind::Import(imports) => {
                for import in imports.iter() {
                    self.check_import(import)?;
                }

                CheckedExpr::new(ExprKind::Import(imports.clone()), Ty::Unit, None, expr.span)
            }
            ExprKind::Foreign(bindings) => {
                let mut new_bindings = vec![];

                for binding in bindings.iter() {
                    new_bindings.push(self.check_binding(frame, binding)?);
                }

                CheckedExpr::new(ExprKind::Foreign(new_bindings), Ty::Unit, None, expr.span)
            }
            ExprKind::Binding(binding) => {
                let binding = self.check_binding(frame, binding)?;
                CheckedExpr::new(
                    ExprKind::Binding(Box::new(binding)),
                    Ty::Unit,
                    None,
                    expr.span,
                )
            }
            ExprKind::Defer(deferred) => {
                CheckedExpr::new(ExprKind::Defer(deferred.clone()), Ty::Unit, None, expr.span)
            }
            ExprKind::Assign { lvalue, rvalue } => {
                self.check_assign_expr(frame, lvalue, rvalue, expr.span)?
            }
            ExprKind::Cast(info) => {
                let info = self.check_cast(frame, info, expected_ty, expr.span)?;
                let source_ty = self.infcx.normalize_ty(&info.expr.ty);

                if ty_can_be_casted(&source_ty, &info.target_ty) {
                    let target_ty = info.target_ty.clone();
                    CheckedExpr::new(ExprKind::Cast(info), target_ty, None, expr.span)
                } else {
                    let source_ty = self.infcx.normalize_ty_and_untyped(&source_ty);
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "cannot cast from `{}` to `{}`",
                            source_ty, info.target_ty
                        ))
                        .with_labels(vec![Label::primary(
                            info.expr.span.file_id,
                            info.expr.span.range().clone(),
                        )
                        .with_message(format!("invalid cast to `{}`", info.target_ty))]));
                }
            }
            ExprKind::Fn(func) => {
                let func = self.check_fn(frame, func, expr.span, expected_ty)?;
                let ty = func.proto.ty.clone();
                CheckedExpr::new(ExprKind::Fn(func), ty, None, expr.span)
            }
            ExprKind::Builtin(builtin) => match builtin {
                Builtin::SizeOf(type_expr) | Builtin::AlignOf(type_expr) => {
                    let result = self.check_type_expr(frame, type_expr)?;
                    CheckedExpr::new(
                        ExprKind::Builtin(Builtin::SizeOf(Box::new(result.expr))),
                        Ty::UInt(UIntTy::Usize),
                        None,
                        expr.span,
                    )
                }
                Builtin::Panic(msg_expr) => {
                    let msg_expr = if let Some(e) = msg_expr {
                        let result = self.check_expr(frame, e, Some(Ty::str()))?;
                        Some(Box::new(result.expr))
                    } else {
                        None
                    };

                    CheckedExpr::new(
                        ExprKind::Builtin(Builtin::Panic(msg_expr)),
                        Ty::Unit,
                        None,
                        expr.span,
                    )
                }
            },
            ExprKind::While {
                cond,
                expr: looped_expr,
            } => {
                frame.loop_depth += 1;

                let mut cond = self.check_expr(frame, cond, None)?;
                let cond_span = cond.expr.span;
                self.infcx
                    .unify_or_coerce_ty_expr(&Ty::Bool, &mut cond.expr, cond_span)?;
                let looped_expr = self.check_expr(frame, looped_expr, None)?;

                frame.loop_depth -= 1;

                CheckedExpr::new(
                    ExprKind::While {
                        cond: Box::new(cond.expr),
                        expr: Box::new(looped_expr.expr),
                    },
                    Ty::Unit,
                    None,
                    expr.span,
                )
            }
            ExprKind::For {
                iter_name,
                iter_index_name,
                iterator,
                expr: looped_expr,
            } => {
                // TODO: for loop should iterate over anything that implements
                // an Iterator TODO: implements is a wide term,
                // it can be an interface like Go, or trait like Rust..

                frame.loop_depth += 1;

                let iterator = match iterator {
                    ForIter::Range(start, end) => {
                        let mut start = self.check_expr(frame, start, None)?;
                        let mut end = self.check_expr(frame, end, None)?;

                        // let end_span = end.expr.span;
                        // self.infcx.unify_or_coerce_expr_expr(
                        //     &mut start.expr,
                        //     &mut end.expr,
                        //     &end_span,
                        // )?;

                        if !self.infcx.is_any_integer(&start.ty)
                            || !self.infcx.is_any_integer(&end.ty)
                        {
                            let start_ty = self.infcx.normalize_ty(&start.ty);
                            let end_ty = self.infcx.normalize_ty(&end.ty);

                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "expected integers in `for range`, but found `{}` and `{}`",
                                    start_ty, end_ty
                                ))
                                .with_labels(vec![
                                    Label::primary(
                                        looped_expr.span.file_id,
                                        start.expr.span.range().clone(),
                                    ),
                                    Label::primary(
                                        looped_expr.span.file_id,
                                        end.expr.span.range().clone(),
                                    ),
                                ]));
                        }

                        if self.infcx.is_untyped_integer(&start.ty) {
                            let span = start.expr.span;
                            self.infcx.unify_or_coerce_ty_expr(
                                &Ty::Int(IntTy::Isize),
                                &mut start.expr,
                                span,
                            )?;
                        }
                        if self.infcx.is_untyped_integer(&end.ty) {
                            let span = end.expr.span;
                            self.infcx
                                .unify_or_coerce_ty_expr(&start.ty, &mut end.expr, span)?;
                        }

                        let start_ty = self.infcx.normalize_ty(&start.ty);

                        // TODO: get span from actual ids
                        todo!("iter type");
                        // frame.insert_binding(
                        //     *iter_name,
                        //     start_ty,
                        //     start.expr.span,
                        //     true,
                        // );
                        // TODO: remove index variable once i have proper iterators
                        todo!("iter index type");
                        // frame.insert_binding(
                        //     *iter_index_name,
                        //     Ty::UInt(UIntTy::Usize),
                        //     start.expr.span,
                        //     true,
                        // );

                        ForIter::Range(Box::new(start.expr), Box::new(end.expr))
                    }
                    ForIter::Value(value) => {
                        let value = self.check_expr(frame, value, None)?;
                        let value_ty = self.infcx.normalize_ty(&value.ty);

                        let by_ref = value_ty.is_pointer();

                        match &value_ty.maybe_deref_once() {
                            t @ Ty::Array(inner, ..) | t @ Ty::Slice(inner, ..) => {
                                // let is_mutable = if
                                let iter_ty = if by_ref {
                                    let is_mutable = match t {
                                        Ty::Array(..) => value.expr.is_mutable(),
                                        Ty::Slice(_, is_mutable) => *is_mutable,
                                        _ => unreachable!("got {}", t),
                                    };

                                    Ty::Pointer(inner.clone(), is_mutable)
                                } else {
                                    inner.as_ref().clone()
                                };

                                // TODO: get span from actual ids
                                todo!("iter type");
                                // frame.insert_binding(*iter_name, iter_ty, value.expr.span, true);
                                todo!("iter index type");
                                // frame.insert_binding(
                                //     *iter_index_name,
                                //     Ty::UInt(UIntTy::Usize),
                                //     value.expr.span,
                                //     true,
                                // );
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "can't iterate over `{}`",
                                        self.infcx.normalize_ty_and_untyped(&value_ty)
                                    ))
                                    .with_labels(vec![Label::primary(
                                        value.expr.span.file_id,
                                        value.expr.span.range().clone(),
                                    )]));
                            }
                        };

                        ForIter::Value(Box::new(value.expr))
                    }
                };

                let result = self.check_expr(frame, looped_expr, None)?;

                frame.loop_depth -= 1;

                CheckedExpr::new(
                    ExprKind::For {
                        iter_name: *iter_name,
                        iter_index_name: *iter_index_name,
                        iterator,
                        expr: Box::new(result.expr),
                    },
                    Ty::Unit,
                    None,
                    expr.span,
                )
            }
            ExprKind::Break { deferred } => {
                if frame.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(expr.span, "break"));
                }

                let deferred = self.check_expr_list(frame, deferred)?;

                CheckedExpr::new(ExprKind::Break { deferred }, Ty::Never, None, expr.span)
            }
            ExprKind::Continue { deferred } => {
                if frame.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(expr.span, "continue"));
                }

                let deferred = self.check_expr_list(frame, deferred)?;

                CheckedExpr::new(ExprKind::Continue { deferred }, Ty::Never, None, expr.span)
            }
            ExprKind::Return {
                expr: returned_expr,
                deferred,
            } => {
                if frame.depth <= 1 {
                    return Err(SyntaxError::outside_of_function(expr.span, "return"));
                }

                match &frame.expected_return_ty.clone() {
                    Some(return_ty) => {
                        let returned_expr = if let Some(returned_expr) = returned_expr {
                            let mut returned_result = self.check_expr(
                                frame,
                                returned_expr,
                                frame.expected_return_ty.clone(),
                            )?;

                            self.infcx.unify_or_coerce_ty_expr(
                                &return_ty,
                                &mut returned_result.expr,
                                expr.span,
                            )?;

                            Some(Box::new(returned_result.expr))
                        } else {
                            self.infcx.unify(return_ty.clone(), Ty::Unit, expr.span)?;

                            None
                        };

                        let deferred = self.check_expr_list(frame, deferred)?;

                        CheckedExpr::new(
                            ExprKind::Return {
                                expr: returned_expr,
                                deferred,
                            },
                            Ty::Never,
                            None,
                            expr.span,
                        )
                    }
                    None => return Err(SyntaxError::outside_of_function(expr.span, "return")),
                }
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond = self.check_expr(frame, cond, None)?;
                let ty = self.infcx.normalize_ty(&cond.ty);

                self.infcx.unify(Ty::Bool, ty, cond.expr.span)?;

                let mut then_result = self.check_expr(frame, then_expr, expected_ty.clone())?;

                let (else_result, result_ty) = if let Some(else_expr) = else_expr {
                    let mut else_result = self.check_expr(frame, else_expr, expected_ty)?;

                    let span = else_result.expr.span;

                    let result_ty = self.infcx.unify_or_coerce_expr_expr(
                        &mut then_result.expr,
                        &mut else_result.expr,
                        span,
                    )?;

                    then_result.ty = result_ty.clone();
                    else_result.ty = result_ty.clone();

                    (Some(else_result), result_ty)
                } else {
                    (None, Ty::Unit)
                };

                if cond.value.is_some() {
                    if cond.value.unwrap().into_bool() {
                        then_result
                    } else {
                        else_result.unwrap_or(CheckedExpr::new(
                            ExprKind::Noop,
                            Ty::Unit,
                            None,
                            expr.span,
                        ))
                    }
                } else {
                    CheckedExpr::new(
                        ExprKind::If {
                            cond: Box::new(cond.expr),
                            then_expr: Box::new(then_result.expr),
                            else_expr: else_result.map(|r| Box::new(r.expr)),
                        },
                        result_ty,
                        None,
                        expr.span,
                    )
                }
            }
            ExprKind::Block(block) => {
                let (block, result_ty) = self.check_block(frame, block, expected_ty)?;

                CheckedExpr::new(ExprKind::Block(block), result_ty, None, expr.span)
            }
            ExprKind::Binary { lhs, op, rhs } => {
                self.check_binary_expr(frame, lhs, *op, rhs, expected_ty, expr.span)?
            }
            ExprKind::Unary { op, lhs } => {
                self.check_unary_expr(frame, *op, lhs, expected_ty, expr.span)?
            }
            ExprKind::Subscript {
                expr: accessed_expr,
                index,
            } => {
                let mut index = self.check_expr(frame, index, None)?;

                let index_span = index.expr.span;
                self.infcx.unify_or_coerce_ty_expr(
                    &Ty::UInt(UIntTy::Usize),
                    &mut index.expr,
                    index_span,
                )?;

                let accessed_expr_result = self.check_expr(frame, accessed_expr, None)?;

                let ty = self.infcx.normalize_ty(&accessed_expr_result.ty);
                let ty_deref = ty.maybe_deref_once();

                if index.value.as_ref().map_or(false, |v| v.is_int()) {
                    let index_value = index.value.unwrap().into_int();
                    match ty_deref {
                        Ty::Array(_, size) => {
                            if index_value < 0 || index_value >= size as _ {
                                return Err(
                                    Diagnostic::error()
                                    .with_message(format!("index out of array bounds - expected 0 to {}, but found {}", size-1,index_value))
                                    .with_labels(vec![
                                        Label::primary(index.expr.span.file_id, index.expr.span.range().clone()).with_message("index out of bounds")
                                    ]
                                ));
                            }
                        }
                        _ => (),
                    }
                }

                match ty_deref {
                    Ty::Array(inner, ..) | Ty::Slice(inner, ..) | Ty::MultiPointer(inner, ..) => {
                        CheckedExpr::new(
                            ExprKind::Subscript {
                                expr: Box::new(accessed_expr_result.expr),
                                index: Box::new(index.expr),
                            },
                            inner.as_ref().clone(),
                            None,
                            expr.span,
                        )
                    }
                    _ => {
                        return Err(TypeError::invalid_expr_in_subscript(
                            accessed_expr_result.expr.span,
                            ty.to_string(),
                        ))
                    }
                }
            }
            ExprKind::Slice {
                expr: sliced_expr,
                low,
                high,
            } => {
                let sliced_expr = self.check_expr(frame, sliced_expr, None)?;
                let sliced_expr_ty = self.infcx.normalize_ty(&sliced_expr.ty);

                let low = if let Some(low) = low {
                    let mut low = self.check_expr(frame, low, None)?;

                    let span = low.expr.span;
                    self.infcx.unify_or_coerce_ty_expr(
                        &Ty::UInt(UIntTy::Usize),
                        &mut low.expr,
                        span,
                    )?;

                    Some(Box::new(low.expr))
                } else {
                    None
                };

                let high = if let Some(high) = high {
                    let mut high = self.check_expr(frame, high, None)?;

                    let span = high.expr.span;
                    self.infcx.unify_or_coerce_ty_expr(
                        &Ty::UInt(UIntTy::Usize),
                        &mut high.expr,
                        span,
                    )?;

                    Some(Box::new(high.expr))
                } else {
                    if sliced_expr_ty.is_multi_pointer() {
                        return Err(Diagnostic::error()
                            .with_message(
                                "multi pointer has unknown length, so you must specify the ending index",
                            )
                            .with_labels(vec![Label::primary(
                                expr.span.file_id,
                                expr.span.range().clone(),
                            )]));
                    }

                    None
                };

                let (result_ty, is_mutable) = match sliced_expr_ty {
                    Ty::Array(inner, ..) => (inner, sliced_expr.expr.is_mutable()),

                    Ty::Slice(inner, is_mutable) | Ty::MultiPointer(inner, is_mutable) => {
                        (inner, is_mutable)
                    }

                    _ => {
                        return Err(TypeError::invalid_expr_in_slice(
                            sliced_expr.expr.span,
                            sliced_expr_ty.to_string(),
                        ))
                    }
                };

                CheckedExpr::new(
                    ExprKind::Slice {
                        expr: Box::new(sliced_expr.expr),
                        low,
                        high,
                    },
                    Ty::Slice(result_ty, is_mutable),
                    None,
                    expr.span,
                )
            }
            ExprKind::Call(call) => self.check_call(frame, call, expr.span)?,
            ExprKind::MemberAccess {
                expr: accessed_expr,
                member: field,
            } => {
                let accessed_expr = self.check_expr(frame, accessed_expr, None)?;
                let accessed_expr_ty = self.infcx.normalize_ty(&accessed_expr.ty);

                let ty = match &accessed_expr_ty.maybe_deref_once() {
                    ty @ Ty::Tuple(tys) => match field.as_str().parse::<i32>() {
                        Ok(index) => match tys.get(index as usize) {
                            Some(field_ty) => field_ty.clone(),
                            None => {
                                return Err(TypeError::tuple_field_out_of_bounds(
                                    expr.span,
                                    &field,
                                    ty.to_string(),
                                    tys.len() - 1,
                                ))
                            }
                        },
                        Err(_) => {
                            return Err(TypeError::non_numeric_tuple_field(
                                expr.span,
                                &field,
                                ty.to_string(),
                            ));
                        }
                    },
                    Ty::Struct(ty) => match ty.fields.iter().find(|f| f.symbol == *field) {
                        Some(field) => field.ty.clone(),
                        None => {
                            return Err(TypeError::invalid_struct_field(
                                expr.span,
                                *field,
                                ty.clone().to_string(),
                            ))
                        }
                    },
                    Ty::Array(..) | Ty::Slice(..) if field.as_str() == BUILTIN_FIELD_LEN => {
                        Ty::UInt(UIntTy::Usize)
                    }
                    Ty::Slice(inner, is_mutable) if field.as_str() == BUILTIN_FIELD_DATA => {
                        Ty::MultiPointer(inner.clone(), *is_mutable)
                    }
                    Ty::Module(module_idx) => {
                        let binding_info =
                            self.find_binding_info_in_module(*module_idx, *field, expr.span)?;
                        binding_info.ty.clone()
                    }
                    ty => {
                        return Err(TypeError::field_access_on_invalid_type(
                            accessed_expr.expr.span,
                            ty.to_string(),
                        ));
                    }
                };

                CheckedExpr::new(
                    ExprKind::MemberAccess {
                        expr: Box::new(accessed_expr.expr),
                        member: *field,
                    },
                    ty,
                    None,
                    expr.span,
                )
            }
            ExprKind::Id {
                symbol,
                binding_info_idx,
                ..
            } => {
                let binding_info = self.workspace.get_binding_info(*binding_info_idx).unwrap();

                if self
                    .init_scopes
                    .get(*binding_info_idx)
                    .unwrap()
                    .is_not_init()
                {
                    let msg = format!("`{}` is possibly uninitialized", symbol);
                    return Err(Diagnostic::error()
                        .with_message(msg.clone())
                        .with_labels(vec![
                            Label::primary(expr.span.file_id, expr.span.range().clone())
                                .with_message(msg),
                            Label::secondary(
                                binding_info.span.file_id,
                                binding_info.span.range().clone(),
                            )
                            .with_message("defined here"),
                        ]));
                }

                CheckedExpr::new(
                    ExprKind::Id {
                        symbol: *symbol,
                        is_mutable: binding_info.is_mutable,
                        binding_span: binding_info.span,
                        binding_info_idx: Default::default(),
                    },
                    binding_info.ty.clone(),
                    None,
                    expr.span,
                )
            }
            ExprKind::ArrayLiteral(kind) => match kind {
                ArrayLiteralKind::List(elements) => {
                    let element_ty: Ty = self.infcx.fresh_type_var().into();

                    let mut new_elements = vec![];

                    for el in elements {
                        let mut el = self.check_expr(frame, el, Some(element_ty.clone()))?;

                        let el_span = el.expr.span;
                        self.infcx
                            .unify_or_coerce_ty_expr(&element_ty, &mut el.expr, el_span)?;

                        new_elements.push(el.expr);
                    }

                    CheckedExpr::new(
                        ExprKind::ArrayLiteral(ArrayLiteralKind::List(new_elements)),
                        Ty::Array(Box::new(element_ty), elements.len()),
                        None,
                        expr.span,
                    )
                }
                ArrayLiteralKind::Fill { expr, len } => {
                    let len = self.check_expr(frame, len, None)?;
                    let len_value = self.expect_value_is_int(len.value, &len.ty, len.expr.span)?;
                    let len_value = len_value as isize;

                    if len_value < 0 {
                        return Err(TypeError::negative_array_len(len.expr.span, len_value));
                    }

                    let expr = self.check_expr(frame, expr, None)?;
                    let span = expr.expr.span;

                    CheckedExpr::new(
                        ExprKind::ArrayLiteral(ArrayLiteralKind::Fill {
                            len: Box::new(len.expr),
                            expr: Box::new(expr.expr),
                        }),
                        Ty::Array(Box::new(expr.ty), len_value as _),
                        None,
                        span,
                    )
                }
            },
            ExprKind::TupleLiteral(elements) => {
                let mut new_elements = vec![];

                for el in elements {
                    new_elements.push(self.check_expr(frame, el, None)?);
                }

                let mut is_type_expression = true;

                for element in &new_elements {
                    let is_type = element.value.as_ref().map_or(false, |v| v.is_type());

                    if !is_type {
                        is_type_expression = false;
                        break;
                    }
                }

                let ty = Ty::Tuple(new_elements.iter().map(|el| el.ty.clone()).collect());
                let new_elements = new_elements.iter().map(|e| e.expr.clone()).collect();
                let span = expr.span;

                if is_type_expression {
                    CheckedExpr::new(
                        ExprKind::TupleLiteral(new_elements),
                        ty.clone().create_type(),
                        Some(Value::Type(ty)),
                        span,
                    )
                } else {
                    CheckedExpr::new(ExprKind::TupleLiteral(new_elements), ty, None, span)
                }
            }
            ExprKind::StructLiteral { type_expr, fields } => match type_expr {
                Some(type_expr) => {
                    let checked_type_expr = self.check_type_expr(frame, type_expr)?;

                    let ty = checked_type_expr.value.unwrap().into_type();

                    match ty {
                        Ty::Struct(struct_ty) => self.check_named_struct_literal(
                            frame,
                            Some(Box::new(checked_type_expr.expr)),
                            fields,
                            struct_ty,
                            expr.span,
                        )?,
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "type `{}` does not support struct initialization syntax",
                                    ty
                                ))
                                .with_labels(vec![Label::primary(
                                    type_expr.span.file_id,
                                    type_expr.span.range().clone(),
                                )]))
                        }
                    }
                }
                None => match expected_ty {
                    Some(ty) => {
                        let ty = self.infcx.normalize_ty(&ty);

                        match ty.maybe_deref_once() {
                            Ty::Struct(struct_ty) => self.check_named_struct_literal(
                                frame, None, fields, struct_ty, expr.span,
                            )?,
                            Ty::Var(_) => {
                                self.check_anonymous_struct_literal(frame, fields, expr.span)?
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "type `{}` does not support struct initialization syntax",
                                        ty
                                    ))
                                    .with_labels(vec![Label::primary(
                                        expr.span.file_id,
                                        expr.span.range().clone(),
                                    )]))
                            }
                        }
                    }
                    None => self.check_anonymous_struct_literal(frame, fields, expr.span)?,
                },
            },
            ExprKind::Literal(kind) => {
                let (ty, value): (Ty, Option<Value>) = match kind {
                    LiteralKind::Int(i) => (
                        self.infcx.new_key(InferenceValue::UntypedInt).into(),
                        Some(Value::Int(*i)),
                    ),
                    LiteralKind::Float(f) => (
                        self.infcx.new_key(InferenceValue::UntypedFloat).into(),
                        Some(Value::Float(*f)),
                    ),
                    LiteralKind::Nil => {
                        (self.infcx.new_key(InferenceValue::UntypedNil).into(), None)
                    }
                    LiteralKind::Unit => match expected_ty {
                        Some(expected_ty) if expected_ty.is_type() => {
                            (Ty::Unit.create_type(), Some(Value::Type(Ty::Unit)))
                        }
                        _ => (Ty::Unit, None),
                    },
                    LiteralKind::Bool(b) => (Ty::Bool, Some(Value::Bool(*b))),
                    LiteralKind::Str(_) => (Ty::str(), None),
                    LiteralKind::Char(_) => (Ty::char(), None),
                };

                CheckedExpr::new(ExprKind::Literal(kind.clone()), ty, value, expr.span)
            }
            ExprKind::PointerType(expr, is_mutable) => {
                let type_expr = self.check_type_expr(frame, expr)?;
                let ty = type_expr.value.unwrap().into_type();

                let new_ty = Ty::Pointer(Box::new(ty.clone()), *is_mutable);

                CheckedExpr::new(
                    ExprKind::PointerType(Box::new(type_expr.expr), *is_mutable),
                    new_ty.clone().create_type(),
                    Some(Value::Type(new_ty)),
                    expr.span,
                )
            }
            ExprKind::MultiPointerType(expr, is_mutable) => {
                let type_expr = self.check_type_expr(frame, expr)?;
                let ty = type_expr.value.unwrap().into_type();

                let new_ty = Ty::MultiPointer(Box::new(ty.clone()), *is_mutable);

                CheckedExpr::new(
                    ExprKind::MultiPointerType(Box::new(type_expr.expr), *is_mutable),
                    new_ty.clone().create_type(),
                    Some(Value::Type(new_ty)),
                    expr.span,
                )
            }
            ExprKind::ArrayType(expr, size) => {
                let type_expr = self.check_type_expr(frame, expr)?;
                let ty = type_expr.value.unwrap().into_type();

                let size = self.check_expr(frame, size, None)?;

                let size_value = self.expect_value_is_int(size.value, &size.ty, size.expr.span)?;
                let size_value = size_value as isize;

                if size_value < 0 {
                    return Err(TypeError::negative_array_len(size.expr.span, size_value));
                }

                let new_ty = Ty::Array(Box::new(ty.clone()), size_value as usize);

                CheckedExpr::new(
                    ExprKind::ArrayType(Box::new(type_expr.expr), Box::new(size.expr)),
                    new_ty.clone().create_type(),
                    Some(Value::Type(new_ty)),
                    expr.span,
                )
            }
            ExprKind::SliceType(expr, is_mutable) => {
                let type_expr = self.check_type_expr(frame, expr)?;
                let ty = type_expr.value.unwrap().into_type();

                let new_ty = Ty::Slice(Box::new(ty.clone()), *is_mutable);

                CheckedExpr::new(
                    ExprKind::SliceType(Box::new(type_expr.expr), *is_mutable),
                    new_ty.clone().create_type(),
                    Some(Value::Type(new_ty)),
                    expr.span,
                )
            }
            ExprKind::StructType(struct_type) => {
                let name = if struct_type.name == "" {
                    self.get_anonymous_struct_name(expr.span)
                } else {
                    struct_type.name
                };

                let binding_info = struct_type
                    .binding_info_idx
                    .map(|idx| self.workspace.get_binding_info_mut(idx).unwrap());

                let qualified_name = binding_info.as_ref().map_or(name, |b| b.qualified_name());

                let opaque_struct = Ty::Struct(StructTy::opaque(
                    struct_type.name,
                    qualified_name,
                    struct_type.kind,
                ));

                let opaque_struct_type = opaque_struct.clone().create_type();

                if let Some(binding_info) = binding_info {
                    binding_info.ty = opaque_struct_type.clone();
                }

                frame.self_types.push(opaque_struct_type);

                let mut field_span_map = UstrMap::<Span>::default();

                let mut new_fields = vec![];
                let mut struct_ty_fields = vec![];

                for field in &struct_type.fields {
                    let type_expr = self.check_type_expr(frame, &field.ty)?;
                    let ty = type_expr.value.unwrap().into_type();

                    if let Some(defined_span) = field_span_map.insert(field.name, field.span) {
                        return Err(SyntaxError::duplicate_struct_field(
                            defined_span,
                            field.span,
                            field.name.to_string(),
                        ));
                    }

                    new_fields.push(StructTypeField {
                        name: field.name,
                        ty: type_expr.expr,
                        span: field.span,
                    });

                    struct_ty_fields.push(StructTyField {
                        symbol: field.name,
                        ty,
                        span: field.span,
                    });
                }

                let mut struct_ty = StructTy {
                    name,
                    qualified_name,
                    kind: struct_type.kind,
                    fields: struct_ty_fields,
                };

                for field in struct_ty.fields.iter() {
                    if self.occurs_check(&field.ty, qualified_name) {
                        return Err(TypeError::circular_type(expr.span, &struct_ty.name));
                    }
                }

                // TODO: i probably don't need emplace_struct_ty
                let struct_ty_copy = struct_ty.clone();
                for field in struct_ty.fields.iter_mut() {
                    self.emplace_struct_ty(&mut field.ty, &struct_ty_copy);
                }

                frame.self_types.pop();

                CheckedExpr::new(
                    ExprKind::StructType(StructType {
                        name,
                        kind: struct_type.kind,
                        fields: new_fields,
                        binding_info_idx: struct_type.binding_info_idx,
                    }),
                    Ty::Struct(struct_ty.clone()).create_type(),
                    Some(Value::Type(Ty::Struct(struct_ty))),
                    expr.span,
                )
            }
            ExprKind::FnType(proto) => {
                let proto = self.check_proto(frame, proto, expected_ty, expr.span)?;

                if proto.lib_name.is_some() {
                    let ty = proto.ty.clone();
                    CheckedExpr::new(ExprKind::FnType(proto), ty, None, expr.span)
                } else {
                    let ty = proto.ty.clone().create_type();
                    CheckedExpr::new(
                        ExprKind::FnType(proto),
                        ty.clone(),
                        Some(Value::Type(ty)),
                        expr.span,
                    )
                }
            }
            ExprKind::SelfType => match frame.self_types.last() {
                Some(self_type) => CheckedExpr::new(
                    ExprKind::SelfType,
                    self_type.clone(),
                    Some(Value::Type(self_type.clone())),
                    expr.span,
                ),
                None => {
                    return Err(Diagnostic::error()
                        .with_message("`Self` is only available within struct definitions")
                        .with_labels(vec![Label::primary(
                            expr.span.file_id,
                            expr.span.range().clone(),
                        )]))
                }
            },
            ExprKind::NeverType => CheckedExpr::new(
                ExprKind::NeverType,
                Ty::Never.create_type(),
                Some(Value::Type(Ty::Never)),
                expr.span,
            ),
            ExprKind::UnitType => CheckedExpr::new(
                ExprKind::UnitType,
                Ty::Unit.create_type(),
                Some(Value::Type(Ty::Unit)),
                expr.span,
            ),
            ExprKind::PlaceholderType => {
                let tyvar = self.infcx.fresh_type_var();
                CheckedExpr::new(
                    ExprKind::PlaceholderType,
                    Ty::from(tyvar).create_type(),
                    Some(Value::Type(tyvar.into())),
                    expr.span,
                )
            }
            ExprKind::Noop => CheckedExpr::new(
                ExprKind::Noop,
                self.infcx.fresh_type_var().into(),
                None,
                expr.span,
            ),
        };

        Ok(checked_expr)
    }

    pub(crate) fn check_type_expr(
        &mut self,
        frame: &mut CheckFrame,
        expr: &Expr,
    ) -> DiagnosticResult<CheckedExpr> {
        let mut result = self.check_expr(frame, expr, Some(Ty::anytype()))?;

        let is_type = result.value.as_ref().map_or(false, |v| v.is_type());

        if !is_type {
            return Err(TypeError::expected(
                expr.span,
                result.ty.to_string(),
                "a type",
            ));
        }

        let ty = result.value.unwrap().into_type();
        let ty = self.infcx.normalize_ty_and_expand_types(&ty);

        result.value = Some(Value::Type(ty));

        Ok(result)
    }

    pub(crate) fn check_expr_list(
        &mut self,
        frame: &mut CheckFrame,
        exprs: &Vec<Expr>,
    ) -> DiagnosticResult<Vec<Expr>> {
        let mut new_exprs = vec![];

        for expr in exprs {
            new_exprs.push(self.check_expr(frame, expr, None)?.expr);
        }

        Ok(new_exprs)
    }

    #[inline]
    fn check_named_struct_literal(
        &mut self,
        frame: &mut CheckFrame,
        type_expr: Option<Box<Expr>>,
        fields: &Vec<StructLiteralField>,
        struct_ty: StructTy,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let mut field_set = UstrSet::default();

        let mut new_fields = vec![];
        let mut uninit_fields = UstrSet::from_iter(struct_ty.fields.iter().map(|f| f.symbol));

        for field in fields {
            if !field_set.insert(field.symbol) {
                return Err(SyntaxError::struct_field_specified_more_than_once(
                    field.span,
                    field.symbol.to_string(),
                ));
            }

            match struct_ty.fields.iter().find(|f| f.symbol == field.symbol) {
                Some(f) => {
                    uninit_fields.remove(&field.symbol);

                    let mut field_value =
                        self.check_expr(frame, &field.value, Some(f.ty.clone()))?;

                    let field_span = field.value.span;
                    self.infcx
                        .unify_or_coerce_ty_expr(&f.ty, &mut field_value.expr, field_span)?;

                    new_fields.push(StructLiteralField {
                        symbol: field.symbol,
                        value: field_value.expr,
                        span: field.span,
                    })
                }
                None => {
                    return Err(TypeError::invalid_struct_field(
                        field.span,
                        field.symbol,
                        Ty::Struct(struct_ty).to_string(),
                    ))
                }
            }
        }

        if struct_ty.is_union() && new_fields.len() != 1 {
            return Err(Diagnostic::error()
                .with_message("union literal should have exactly one field")
                .with_labels(vec![Label::primary(span.file_id, span.range().clone())]));
        }

        if !struct_ty.is_union() && !uninit_fields.is_empty() {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "missing struct fields: {}",
                    uninit_fields
                        .iter()
                        .map(|f| f.as_str())
                        .collect::<Vec<&str>>()
                        .join(", ")
                ))
                .with_labels(vec![Label::primary(span.file_id, span.range().clone())]));
        }

        Ok(CheckedExpr::new(
            ExprKind::StructLiteral {
                type_expr: type_expr.clone(),
                fields: new_fields,
            },
            Ty::Struct(struct_ty),
            None,
            span,
        ))
    }

    #[inline]
    fn check_anonymous_struct_literal(
        &mut self,
        frame: &mut CheckFrame,
        fields: &Vec<StructLiteralField>,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let mut field_set = UstrSet::default();

        let mut new_fields = vec![];
        let mut struct_ty_fields = vec![];

        for field in fields {
            if !field_set.insert(field.symbol) {
                return Err(SyntaxError::struct_field_specified_more_than_once(
                    field.span,
                    field.symbol.to_string(),
                ));
            }

            let field_value = self.check_expr(frame, &field.value, None)?;

            struct_ty_fields.push(StructTyField {
                symbol: field.symbol,
                ty: field_value.ty.clone(),
                span: field.span,
            });

            new_fields.push(StructLiteralField {
                symbol: field.symbol,
                value: field_value.expr,
                span: field.span,
            })
        }

        let type_name = self.get_anonymous_struct_name(span);

        let struct_ty = StructTy {
            name: type_name,
            qualified_name: type_name,
            kind: StructTyKind::Struct,
            fields: struct_ty_fields,
        };

        Ok(CheckedExpr::new(
            ExprKind::StructLiteral {
                type_expr: None,
                fields: new_fields,
            },
            Ty::Struct(struct_ty),
            None,
            span,
        ))
    }

    fn occurs_check(&self, ty: &Ty, struct_name: Ustr) -> bool {
        match ty {
            Ty::Fn(func) => {
                func.params
                    .iter()
                    .any(|p| self.occurs_check(&p.ty, struct_name))
                    || self.occurs_check(&func.ret, struct_name)
            }
            Ty::Array(ty, _) => self.occurs_check(ty, struct_name),
            Ty::Tuple(tys) => tys.iter().any(|ty| self.occurs_check(ty, struct_name)),
            Ty::Struct(ty) => {
                if ty.qualified_name == struct_name {
                    true
                } else {
                    ty.fields
                        .iter()
                        .any(|field| self.occurs_check(&field.ty, struct_name))
                }
            }
            // Ty::Type(ty) => self.occurs_check(ty, struct_name),
            _ => false,
        }
    }

    // TODO: this function is probably redundant?
    fn emplace_struct_ty(&self, ty: &mut Ty, struct_ty: &StructTy) {
        match ty {
            Ty::Fn(func) => {
                for p in func.params.iter_mut() {
                    self.emplace_struct_ty(&mut p.ty, struct_ty)
                }

                self.emplace_struct_ty(&mut func.ret, struct_ty)
            }

            Ty::Tuple(tys) => {
                for ty in tys.iter_mut() {
                    self.emplace_struct_ty(ty, struct_ty)
                }
            }

            Ty::Struct(ty) => {
                if ty.qualified_name == struct_ty.qualified_name {
                    *ty = struct_ty.clone();
                } else {
                    for field in ty.fields.iter_mut() {
                        self.emplace_struct_ty(&mut field.ty, struct_ty);
                    }
                }
            }

            Ty::Type(ty)
            | Ty::Array(ty, ..)
            | Ty::Pointer(ty, ..)
            | Ty::MultiPointer(ty, ..)
            | Ty::Slice(ty, ..) => self.emplace_struct_ty(ty, struct_ty),

            _ => (),
        }
    }

    fn get_anonymous_struct_name(&self, span: Span) -> Ustr {
        ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
    }

    pub(crate) fn check_block(
        &mut self,
        frame: &mut CheckFrame,
        block: &Block,
        expected_ty: Option<Ty>,
    ) -> DiagnosticResult<(Block, Ty)> {
        let mut new_block = Block {
            exprs: vec![],
            deferred: vec![],
            yields: block.yields,
        };

        self.init_scopes.push_scope();

        let mut result_ty = Ty::Unit;

        if !block.exprs.is_empty() {
            let last_index = block.exprs.len() - 1;

            for (index, expr) in block.exprs.iter().enumerate() {
                let is_last = index == last_index;

                let result = self.check_expr(
                    frame,
                    expr,
                    if is_last { expected_ty.clone() } else { None },
                )?;

                new_block.exprs.push(result.expr);

                if is_last {
                    result_ty = result.ty.into();
                }
            }
        }

        new_block.deferred = self.check_expr_list(frame, &block.deferred)?;

        self.init_scopes.pop_scope();

        Ok((new_block, result_ty))
    }

    fn check_cast(
        &mut self,
        frame: &mut CheckFrame,
        info: &Cast,
        expected_ty: Option<Ty>,
        expr_span: Span,
    ) -> DiagnosticResult<Cast> {
        let casted_expr = self.check_expr(frame, &info.expr, None)?;

        let (type_expr, target_ty) = if let Some(type_expr) = &info.type_expr {
            let type_expr = self.check_type_expr(frame, type_expr)?;
            let target_ty = type_expr.value.unwrap().into_type();
            (Some(Box::new(type_expr.expr)), target_ty)
        } else {
            match expected_ty {
                Some(expected_ty) => {
                    let expected_ty = self.infcx.normalize_ty(&expected_ty);
                    (None, expected_ty)
                }
                None => {
                    return Err(Diagnostic::error()
                        .with_message("can't infer the type cast's target type")
                        .with_labels(vec![Label::primary(expr_span.file_id, expr_span.range())]))
                }
            }
        };

        Ok(Cast {
            expr: Box::new(casted_expr.expr),
            type_expr,
            target_ty,
        })
    }

    pub(super) fn check_expr_can_be_mutably_referenced(
        &mut self,
        expr: &Expr,
    ) -> DiagnosticResult<()> {
        use MutabilityRefCheckErr::*;

        self.check_expr_can_be_mutably_referenced_internal(expr, true)
            .map_err(|err| match err {
                ImmutableReference { symbol, ty_str } => Diagnostic::error()
                    .with_message(format!(
                        "cannot reference `{}`, because it is behind an immutable `{}`",
                        symbol, ty_str
                    ))
                    .with_labels(vec![Label::primary(
                        expr.span.file_id,
                        expr.span.range().clone(),
                    )
                    .with_message("cannot reference")]),
                ImmutableFieldAccess {
                    root_symbol,
                    binding_span,
                    full_path,
                } => Diagnostic::error()
                    .with_message(format!(
                        "cannot reference `{}`, as `{}` is not declared as mutable",
                        full_path, root_symbol
                    ))
                    .with_labels(vec![
                        Label::primary(expr.span.file_id, expr.span.range().clone())
                            .with_message("cannot reference"),
                        Label::secondary(binding_span.file_id, binding_span.range()).with_message(
                            format!(
                                "consider changing this to be mutable: `mut {}`",
                                root_symbol
                            ),
                        ),
                    ]),
                Immutablebinding {
                    symbol,
                    binding_span,
                } => Diagnostic::error()
                    .with_message(format!(
                        "cannot reference `{}` as mutable, as it is not declared as mutable",
                        symbol
                    ))
                    .with_labels(vec![
                        Label::primary(expr.span.file_id, expr.span.range().clone())
                            .with_message("cannot reference immutable variable"),
                        Label::secondary(binding_span.file_id, binding_span.range()).with_message(
                            format!("consider changing this to be mutable: `mut {}`", symbol),
                        ),
                    ]),
            })
    }

    fn check_expr_can_be_mutably_referenced_internal(
        &mut self,
        expr: &Expr,
        is_direct_ref: bool,
    ) -> Result<(), MutabilityRefCheckErr> {
        use MutabilityRefCheckErr::*;

        let ty = self.infcx.normalize_ty_and_untyped(&expr.ty);

        match &expr.kind {
            ExprKind::MemberAccess { expr, member } => {
                match self.check_expr_can_be_mutably_referenced_internal(expr, true) {
                    Ok(_) => match ty {
                        Ty::Tuple(tys) => {
                            let index = member.parse::<usize>().unwrap();
                            let ty = &tys[index];

                            match ty {
                                Ty::Slice(_, is_mutable)
                                | Ty::MultiPointer(_, is_mutable)
                                | Ty::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        symbol: *member,
                                        ty_str: ty.to_string(),
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        Ty::Struct(struct_ty) => {
                            let field_ty = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.symbol == *member)
                                .map(|f| &f.ty)
                                .unwrap();

                            match field_ty {
                                Ty::Slice(_, is_mutable)
                                | Ty::MultiPointer(_, is_mutable)
                                | Ty::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        symbol: *member,
                                        ty_str: field_ty.to_string(),
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        Ty::Module(idx) => {
                            todo!()
                            // let module =
                            //     self.new_ir.modules.get(&module_name).
                            // unwrap();
                            // match module.find_binding(*member) {
                            //     Some(binding) => match &binding.ty {
                            //         Ty::Slice(_, is_mutable)
                            //         | Ty::MultiPointer(_, is_mutable)
                            //         | Ty::Pointer(_, is_mutable)
                            //             if !is_mutable =>
                            //         {
                            //             Err(ImmutableReference {
                            //                 symbol: *member,
                            //                 ty_str: binding.ty.to_string(),
                            //             })
                            //         }
                            //         _ => {
                            //             let SymbolPattern {
                            //                 span,
                            //                 is_mutable,
                            //                 ..
                            //             } = binding.pattern.into_single();

                            //             if is_mutable {
                            //                 Ok(())
                            //             } else {
                            //                 Err(Immutablebinding {
                            //                     symbol: ustr(&format!(
                            //                         "{}.{}",
                            //                         module_name, member
                            //                     )),
                            //                     binding_span: span,
                            //                 })
                            //             }
                            //         }
                            //     },
                            //     None => {
                            //         let import =
                            //             module.find_import(*member).unwrap();
                            //         Err(Immutablebinding {
                            //             symbol: ustr(&format!(
                            //                 "{}.{}",
                            //                 module_name, member
                            //             )),
                            //             binding_span: import.span,
                            //         })
                            //     }
                            // }
                        }
                        _ => Ok(()),
                    },
                    Err(err) => Err(match err {
                        ImmutableFieldAccess {
                            root_symbol,
                            binding_span,
                            full_path,
                        } => ImmutableFieldAccess {
                            root_symbol,
                            binding_span,
                            full_path: format!("{}.{}", full_path, member),
                        },
                        ImmutableReference { symbol, ty_str } => ImmutableReference {
                            symbol: ustr(&format!("{}.{}", symbol, member)),
                            ty_str,
                        },
                        Immutablebinding {
                            symbol,
                            binding_span,
                        } => ImmutableFieldAccess {
                            root_symbol: symbol,
                            binding_span,
                            full_path: format!("{}.{}", symbol, member),
                        },
                    }),
                }
            }
            ExprKind::Id {
                symbol,
                is_mutable,
                binding_span,
                binding_info_idx: _,
            } => {
                match ty {
                    Ty::Slice(_, is_mutable)
                    | Ty::MultiPointer(_, is_mutable)
                    | Ty::Pointer(_, is_mutable) => {
                        if is_mutable && is_direct_ref {
                            return Ok(());
                        } else {
                            return Err(ImmutableReference {
                                symbol: *symbol,
                                ty_str: ty.to_string(),
                            });
                        }
                    }
                    _ => (),
                }

                if *is_mutable {
                    Ok(())
                } else {
                    Err(Immutablebinding {
                        symbol: *symbol,
                        binding_span: *binding_span,
                    })
                }
            }
            _ => Ok(()),
        }
    }
}

enum MutabilityRefCheckErr {
    ImmutableReference {
        symbol: Ustr,
        ty_str: String,
    },
    ImmutableFieldAccess {
        root_symbol: Ustr,
        binding_span: Span,
        full_path: String,
    },
    Immutablebinding {
        symbol: Ustr,
        binding_span: Span,
    },
}
