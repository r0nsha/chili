use crate::{CheckFrame, CheckResult, CheckSess};
use chili_ast::ast::ExprKind;
use chili_ast::ty::*;
use chili_ast::{ast, value::Value};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_infer::{cast::Cast, sess::InferValue};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use ustr::{ustr, Ustr, UstrMap, UstrSet};

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_expr(
        &mut self,
        frame: &mut CheckFrame,
        expr: &mut ast::Expr,
        expected_ty: Option<TyKind>,
    ) -> DiagnosticResult<CheckResult> {
        let ty = match &mut expr.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports.iter_mut() {
                    self.check_import(import)?;
                }
                Ok(CheckResult::new(TyKind::Unit, None))
            }
            ast::ExprKind::Foreign(bindings) => {
                for binding in bindings.iter_mut() {
                    self.check_binding(frame, binding)?;
                }
                Ok(CheckResult::new(TyKind::Unit, None))
            }
            ast::ExprKind::Binding(binding) => self.check_binding(frame, binding),
            ast::ExprKind::Defer(deferred) => {
                self.check_expr(frame, deferred.as_mut(), expected_ty)?;
                Ok(CheckResult::new(TyKind::Unit, None))
            }
            ast::ExprKind::Assign { lvalue, rvalue } => {
                self.check_assign_expr(frame, lvalue, rvalue)
            }
            ast::ExprKind::Cast(info) => {
                let target_ty = self.check_cast(frame, info, expected_ty, expr.span)?;
                let source_ty = self.infcx.normalize_ty(&info.expr.ty);

                if source_ty.can_cast(&target_ty.ty) {
                    Ok(target_ty)
                } else {
                    let source_ty = self.infcx.normalize_ty_and_untyped(&source_ty);
                    Err(Diagnostic::error()
                        .with_message(format!(
                            "cannot cast from `{}` to `{}`",
                            source_ty, info.target_ty
                        ))
                        .with_labels(vec![Label::primary(
                            info.expr.span.file_id,
                            info.expr.span.range().clone(),
                        )
                        .with_message(format!("invalid cast to `{}`", info.target_ty))]))
                }
            }
            ast::ExprKind::Fn(func) => self.check_fn(frame, func, expr.span, expected_ty),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(type_expr) | ast::Builtin::AlignOf(type_expr) => {
                    self.check_type_expr(frame, type_expr)?;
                    Ok(CheckResult::new(TyKind::UInt(UIntTy::Usize), None))
                }
                ast::Builtin::Panic(msg_expr) => {
                    if let Some(expr) = msg_expr {
                        self.check_expr(frame, expr, Some(TyKind::str()))?;
                    }
                    Ok(CheckResult::new(TyKind::Unit, None))
                }
            },
            ast::ExprKind::While { cond, expr: block } => {
                cond.ty = self.check_expr(frame, cond, Some(TyKind::Bool))?.ty;
                self.infcx.unify_or_coerce_ty_expr(&TyKind::Bool, cond)?;
                self.check_expr(frame, block, None)?;
                Ok(CheckResult::new(TyKind::Unit, None))
            }
            ast::ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        self.check_expr(frame, start, None)?;
                        self.check_expr(frame, end, None)?;

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
                                    Label::primary(start.span.file_id, start.span.range())
                                        .with_message(format!("type is `{}`", start_ty)),
                                    Label::primary(end.span.file_id, end.span.range())
                                        .with_message(format!("type is `{}`", end_ty)),
                                ]));
                        }

                        if self.infcx.is_untyped_integer(&start.ty) {
                            self.infcx
                                .unify_or_coerce_ty_expr(&TyKind::Int(IntTy::Isize), start)?;
                        }
                        if self.infcx.is_untyped_integer(&end.ty) {
                            self.infcx.unify_or_coerce_ty_expr(&start.ty, end)?;
                        }

                        let start_ty = self.infcx.normalize_ty(&start.ty);

                        self.update_binding_info_ty(for_.iter_idx, start_ty);
                        self.update_binding_info_ty(
                            for_.iter_index_idx,
                            TyKind::UInt(UIntTy::Usize),
                        );
                    }
                    ast::ForIter::Value(value) => {
                        self.check_expr(frame, value, None)?;
                        let value_ty = self.infcx.normalize_ty(&value.ty);

                        let by_ref = value_ty.is_pointer();

                        match &value_ty.maybe_deref_once() {
                            t @ TyKind::Array(inner, ..) | t @ TyKind::Slice(inner, ..) => {
                                let iter_ty = if by_ref {
                                    let is_mutable = match t {
                                        TyKind::Array(..) => value.is_mutable(),
                                        TyKind::Slice(_, is_mutable) => *is_mutable,
                                        _ => unreachable!("got {}", t),
                                    };

                                    TyKind::Pointer(inner.clone(), is_mutable)
                                } else {
                                    inner.as_ref().clone()
                                };

                                self.update_binding_info_ty(for_.iter_idx, iter_ty);
                                self.update_binding_info_ty(
                                    for_.iter_index_idx,
                                    TyKind::UInt(UIntTy::Usize),
                                );
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "can't iterate over `{}`",
                                        self.infcx.normalize_ty_and_untyped(&value_ty)
                                    ))
                                    .with_labels(vec![Label::primary(
                                        value.span.file_id,
                                        value.span.range().clone(),
                                    )]));
                            }
                        };
                    }
                };

                self.check_expr(frame, &mut for_.expr, None)?;

                Ok(CheckResult::new(TyKind::Unit, None))
            }
            ast::ExprKind::Break { deferred } => {
                self.check_expr_list(frame, deferred)?;
                Ok(CheckResult::new(TyKind::Never, None))
            }
            ast::ExprKind::Continue { deferred } => {
                self.check_expr_list(frame, deferred)?;
                Ok(CheckResult::new(TyKind::Never, None))
            }
            ast::ExprKind::Return {
                expr: returned_expr,
                deferred,
            } => {
                if frame.depth <= 1 {
                    return Err(SyntaxError::outside_of_function(expr.span, "return"));
                }

                match &frame.expected_return_ty.clone() {
                    Some(return_ty) => {
                        if let Some(returned_expr) = returned_expr {
                            self.check_expr(
                                frame,
                                returned_expr,
                                frame.expected_return_ty.clone(),
                            )?;

                            self.infcx
                                .unify_or_coerce_ty_expr(&return_ty, returned_expr)?;
                        } else {
                            self.infcx
                                .unify(return_ty.clone(), TyKind::Unit, expr.span)?;
                        }

                        self.check_expr_list(frame, deferred)?;

                        Ok(CheckResult::new(TyKind::Never, None))
                    }
                    None => Err(SyntaxError::outside_of_function(expr.span, "return")),
                }
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_result = self.check_expr(frame, cond, None)?;
                let cond_ty = self.infcx.normalize_ty(&cond.ty);

                self.infcx.unify(TyKind::Bool, cond_ty, cond.span)?;

                self.check_expr(frame, then_expr, expected_ty.clone())?;

                let result_ty = if let Some(else_expr) = else_expr {
                    self.check_expr(frame, else_expr, expected_ty)?;

                    let result_ty = self.infcx.unify_or_coerce_expr_expr(
                        then_expr,
                        else_expr,
                        else_expr.span,
                    )?;

                    else_expr.ty = result_ty.clone();

                    result_ty
                } else {
                    TyKind::Unit
                };

                then_expr.ty = result_ty.clone();

                if cond_result.value.is_some() {
                    if cond_result.value.unwrap().into_bool() {
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

                Ok(result_ty)
            }
            ast::ExprKind::Block(block) => self.check_block(frame, block, expected_ty),
            ast::ExprKind::Binary { lhs, op, rhs } => {
                let result =
                    self.check_binary_expr(frame, lhs, *op, rhs, expected_ty, expr.span)?;

                if let Some(value) = result.value {
                    expr.kind = ExprKind::Noop;
                }

                todo!();
                result
            }
            ast::ExprKind::Unary { op, lhs } => {
                self.check_unary_expr(frame, *op, lhs, expected_ty, expr.span)
            }
            ast::ExprKind::Subscript {
                expr: accessed_expr,
                index,
            } => {
                self.check_expr(frame, index, None)?;

                self.infcx
                    .unify_or_coerce_ty_expr(&TyKind::UInt(UIntTy::Usize), index)?;

                self.check_expr(frame, accessed_expr, None)?;

                let ty = self.infcx.normalize_ty(&accessed_expr.ty);
                let ty_deref = ty.maybe_deref_once();

                match ty_deref {
                    TyKind::Array(inner, ..)
                    | TyKind::Slice(inner, ..)
                    | TyKind::MultiPointer(inner, ..) => Ok(inner.as_ref().clone()),
                    _ => {
                        return Err(TypeError::invalid_expr_in_subscript(
                            accessed_expr.span,
                            ty.to_string(),
                        ))
                    }
                }
            }
            ast::ExprKind::Slice {
                expr: sliced_expr,
                low,
                high,
            } => {
                self.check_expr(frame, sliced_expr, None)?;
                let sliced_expr_ty = self.infcx.normalize_ty(&sliced_expr.ty);

                if let Some(low) = low {
                    self.check_expr(frame, low, None)?;
                    self.infcx
                        .unify_or_coerce_ty_expr(&TyKind::UInt(UIntTy::Usize), low)?;
                }

                if let Some(high) = high {
                    self.check_expr(frame, high, None)?;
                    self.infcx
                        .unify_or_coerce_ty_expr(&TyKind::UInt(UIntTy::Usize), high)?;
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
                }

                let (result_ty, is_mutable) = match sliced_expr_ty {
                    TyKind::Array(inner, ..) => (inner, sliced_expr.is_mutable()),

                    TyKind::Slice(inner, is_mutable) | TyKind::MultiPointer(inner, is_mutable) => {
                        (inner, is_mutable)
                    }

                    _ => {
                        return Err(TypeError::invalid_expr_in_slice(
                            sliced_expr.span,
                            sliced_expr_ty.to_string(),
                        ))
                    }
                };

                Ok(TyKind::Slice(result_ty, is_mutable))
            }
            ast::ExprKind::Call(call) => self.check_call(frame, call, expr.span),
            ast::ExprKind::MemberAccess {
                expr: accessed_expr,
                member: field,
            } => {
                self.check_expr(frame, accessed_expr, None)?;
                let accessed_expr_ty = self.infcx.normalize_ty(&accessed_expr.ty);

                let ty = match &accessed_expr_ty.maybe_deref_once() {
                    ty @ TyKind::Tuple(tys) => match field.as_str().parse::<i32>() {
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
                    TyKind::Struct(ty) => match ty.fields.iter().find(|f| f.symbol == *field) {
                        Some(field) => field.ty.clone(),
                        None => {
                            return Err(TypeError::invalid_struct_field(
                                expr.span,
                                *field,
                                ty.clone().to_string(),
                            ))
                        }
                    },
                    TyKind::Array(..) | TyKind::Slice(..)
                        if field.as_str() == BUILTIN_FIELD_LEN =>
                    {
                        TyKind::UInt(UIntTy::Usize)
                    }
                    TyKind::Slice(inner, is_mutable) if field.as_str() == BUILTIN_FIELD_DATA => {
                        TyKind::MultiPointer(inner.clone(), *is_mutable)
                    }
                    TyKind::Module(module_idx) => {
                        let binding_info =
                            self.find_binding_info_in_module(*module_idx, *field, expr.span)?;
                        binding_info.ty.clone()
                    }
                    ty => {
                        return Err(TypeError::field_access_on_invalid_type(
                            accessed_expr.span,
                            ty.to_string(),
                        ));
                    }
                };

                Ok(ty)
            }
            ast::ExprKind::Id {
                symbol,
                binding_info_idx,
                ..
            } => {
                let binding_info = self.workspace.get_binding_info(*binding_info_idx).unwrap();

                if let Some(state) = self.init_scopes.get(*binding_info_idx) {
                    if state.is_not_init() {
                        let msg = format!("use of possibly uninitialized value `{}`", symbol);
                        return Err(Diagnostic::error().with_message(msg.clone()).with_labels(
                            vec![
                                Label::primary(expr.span.file_id, expr.span.range().clone())
                                    .with_message(msg),
                                Label::secondary(
                                    binding_info.span.file_id,
                                    binding_info.span.range().clone(),
                                )
                                .with_message("defined here"),
                            ],
                        ));
                    }
                }

                Ok(binding_info.ty.clone())
            }
            ast::ExprKind::ArrayLiteral(kind) => match kind {
                ast::ArrayLiteralKind::List(elements) => {
                    let element_ty: TyKind = self.infcx.fresh_type_var().into();

                    for el in elements.iter_mut() {
                        self.check_expr(frame, el, Some(element_ty.clone()))?;
                        self.infcx.unify_or_coerce_ty_expr(&element_ty, el)?;
                    }

                    Ok(TyKind::Array(Box::new(element_ty), elements.len()))
                }
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    self.check_expr(frame, len, None)?;
                    let inner = self.check_expr(frame, expr, None)?;
                    Ok(TyKind::Array(Box::new(inner), 0))
                }
            },
            ast::ExprKind::TupleLiteral(elements) => {
                for el in elements.iter_mut() {
                    self.check_expr(frame, el, None)?;
                }

                let is_type_expression = elements.iter().all(|el| el.ty.is_type());
                let ty = TyKind::Tuple(elements.iter().map(|el| el.ty.clone()).collect());

                if is_type_expression {
                    Ok(ty.clone().create_type())
                } else {
                    Ok(ty)
                }
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => match type_expr {
                Some(type_expr) => {
                    let ty = self.check_type_expr(frame, type_expr)?;
                    match ty {
                        TyKind::Struct(struct_ty) => {
                            self.check_named_struct_literal(frame, fields, struct_ty, expr.span)
                        }
                        _ => Err(Diagnostic::error()
                            .with_message(format!(
                                "type `{}` does not support struct initialization syntax",
                                ty
                            ))
                            .with_labels(vec![Label::primary(
                                type_expr.span.file_id,
                                type_expr.span.range().clone(),
                            )])),
                    }
                }
                None => match expected_ty {
                    Some(ty) => {
                        let ty = self.infcx.normalize_ty(&ty);

                        match ty.maybe_deref_once() {
                            TyKind::Struct(struct_ty) => {
                                self.check_named_struct_literal(frame, fields, struct_ty, expr.span)
                            }
                            TyKind::Var(_) => {
                                self.check_anonymous_struct_literal(frame, fields, expr.span)
                            }
                            _ => Err(Diagnostic::error()
                                .with_message(format!(
                                    "type `{}` does not support struct initialization syntax",
                                    ty
                                ))
                                .with_labels(vec![Label::primary(
                                    expr.span.file_id,
                                    expr.span.range().clone(),
                                )])),
                        }
                    }
                    None => self.check_anonymous_struct_literal(frame, fields, expr.span),
                },
            },
            ast::ExprKind::Literal(kind) => Ok(match kind {
                ast::LiteralKind::Int(i) => self.infcx.new_key(InferValue::UntypedInt).into(),
                ast::LiteralKind::Float(f) => self.infcx.new_key(InferValue::UntypedFloat).into(),
                ast::LiteralKind::Nil => self.infcx.new_key(InferValue::UntypedNil).into(),
                ast::LiteralKind::Unit => match expected_ty {
                    Some(expected_ty) if expected_ty.is_type() => TyKind::Unit.create_type(),
                    _ => TyKind::Unit,
                },
                ast::LiteralKind::Bool(b) => TyKind::Bool,
                ast::LiteralKind::Str(_) => TyKind::str(),
                ast::LiteralKind::Char(_) => TyKind::char(),
            }),
            ast::ExprKind::PointerType(expr, is_mutable) => {
                let ty = self.check_type_expr(frame, expr)?;
                let new_ty = TyKind::Pointer(Box::new(ty.clone()), *is_mutable);
                Ok(new_ty.clone().create_type())
            }
            ast::ExprKind::MultiPointerType(expr, is_mutable) => {
                let ty = self.check_type_expr(frame, expr)?;
                let new_ty = TyKind::MultiPointer(Box::new(ty.clone()), *is_mutable);
                Ok(new_ty.clone().create_type())
            }
            ast::ExprKind::ArrayType(expr, size) => {
                let ty = self.check_type_expr(frame, expr)?;
                let size = self.check_expr(frame, size, None)?;
                let new_ty = TyKind::Array(Box::new(ty.clone()), 0);
                Ok(new_ty.clone().create_type())
            }
            ast::ExprKind::SliceType(expr, is_mutable) => {
                let ty = self.check_type_expr(frame, expr)?;
                let new_ty = TyKind::Slice(Box::new(ty.clone()), *is_mutable);
                Ok(new_ty.clone().create_type())
            }
            ast::ExprKind::StructType(struct_type) => {
                let name = if struct_type.name.is_empty() {
                    self.get_anonymous_struct_name(expr.span)
                } else {
                    struct_type.name
                };

                let binding_info = struct_type
                    .binding_info_idx
                    .map(|idx| self.workspace.get_binding_info_mut(idx).unwrap());

                let qualified_name = binding_info.as_ref().map_or(name, |b| b.qualified_name());

                let opaque_struct = TyKind::Struct(StructTy::opaque(
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

                let mut struct_ty_fields = vec![];

                for field in struct_type.fields.iter_mut() {
                    let ty = self.check_type_expr(frame, &mut field.ty)?;

                    if let Some(defined_span) = field_span_map.insert(field.name, field.span) {
                        return Err(SyntaxError::duplicate_struct_field(
                            defined_span,
                            field.span,
                            field.name.to_string(),
                        ));
                    }

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

                Ok(TyKind::Struct(struct_ty.clone()).create_type())
            }
            ast::ExprKind::FnType(proto) => {
                let proto_ty = self.check_proto(frame, proto, expected_ty, expr.span)?;

                Ok(if proto.lib_name.is_some() {
                    // this is a foreign function
                    proto_ty
                } else {
                    // this is just a function type
                    proto_ty.create_type()
                })
            }
            ast::ExprKind::SelfType => match frame.self_types.last() {
                Some(self_type) => Ok(self_type.clone()),
                None => Err(Diagnostic::error()
                    .with_message("`Self` is only available within struct definitions")
                    .with_labels(vec![Label::primary(
                        expr.span.file_id,
                        expr.span.range().clone(),
                    )])),
            },
            ast::ExprKind::NeverType => Ok(TyKind::Never.create_type()),
            ast::ExprKind::UnitType => Ok(TyKind::Unit.create_type()),
            ast::ExprKind::PlaceholderType => {
                Ok(TyKind::from(self.infcx.fresh_type_var()).create_type())
            }
            ast::ExprKind::Noop => Ok(self.infcx.fresh_type_var().into()),
        }?;

        expr.ty = ty.clone();

        Ok(ty)
    }

    pub(crate) fn check_type_expr(
        &mut self,
        frame: &mut CheckFrame,
        expr: &mut ast::Expr,
    ) -> DiagnosticResult<CheckResult> {
        self.check_expr(frame, expr, Some(TyKind::anytype()))?;

        if !expr.ty.is_type() {
            return Err(TypeError::expected(
                expr.span,
                self.infcx.normalize_ty_and_untyped(&expr.ty).to_string(),
                "a type",
            ));
        }

        Ok(self.infcx.normalize_ty_and_expand_types(&expr.ty))
    }

    pub(crate) fn check_expr_list(
        &mut self,
        frame: &mut CheckFrame,
        exprs: &mut Vec<ast::Expr>,
    ) -> DiagnosticResult<()> {
        for expr in exprs {
            self.check_expr(frame, expr, None)?;
        }
        Ok(())
    }

    #[inline]
    fn check_named_struct_literal(
        &mut self,
        frame: &mut CheckFrame,
        fields: &mut Vec<ast::StructLiteralField>,
        struct_ty: StructTy,
        span: Span,
    ) -> DiagnosticResult<CheckResult> {
        let mut field_set = UstrSet::default();

        let mut uninit_fields = UstrSet::from_iter(struct_ty.fields.iter().map(|f| f.symbol));

        for field in fields.iter_mut() {
            if !field_set.insert(field.symbol) {
                return Err(SyntaxError::struct_field_specified_more_than_once(
                    field.span,
                    field.symbol.to_string(),
                ));
            }

            match struct_ty.fields.iter().find(|f| f.symbol == field.symbol) {
                Some(f) => {
                    uninit_fields.remove(&field.symbol);

                    self.check_expr(frame, &mut field.value, Some(f.ty.clone()))?;

                    self.infcx
                        .unify_or_coerce_ty_expr(&f.ty, &mut field.value)?;
                }
                None => {
                    return Err(TypeError::invalid_struct_field(
                        field.span,
                        field.symbol,
                        TyKind::Struct(struct_ty).to_string(),
                    ))
                }
            }
        }

        if struct_ty.is_union() && fields.len() != 1 {
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

        Ok(TyKind::Struct(struct_ty))
    }

    #[inline]
    fn check_anonymous_struct_literal(
        &mut self,
        frame: &mut CheckFrame,
        fields: &mut Vec<ast::StructLiteralField>,
        span: Span,
    ) -> DiagnosticResult<CheckResult> {
        let mut field_set = UstrSet::default();

        let mut struct_ty_fields = vec![];

        for field in fields {
            if !field_set.insert(field.symbol) {
                return Err(SyntaxError::struct_field_specified_more_than_once(
                    field.span,
                    field.symbol.to_string(),
                ));
            }

            let value_ty = self.check_expr(frame, &mut field.value, None)?;

            struct_ty_fields.push(StructTyField {
                symbol: field.symbol,
                ty: value_ty,
                span: field.span,
            });
        }

        let type_name = self.get_anonymous_struct_name(span);

        let struct_ty = StructTy {
            name: type_name,
            qualified_name: type_name,
            kind: StructTyKind::Struct,
            fields: struct_ty_fields,
        };

        Ok(TyKind::Struct(struct_ty))
    }

    fn occurs_check(&self, ty: &TyKind, struct_name: Ustr) -> bool {
        match ty {
            TyKind::Fn(func) => {
                func.params
                    .iter()
                    .any(|p| self.occurs_check(&p.ty, struct_name))
                    || self.occurs_check(&func.ret, struct_name)
            }
            TyKind::Array(ty, _) => self.occurs_check(ty, struct_name),
            TyKind::Tuple(tys) => tys.iter().any(|ty| self.occurs_check(ty, struct_name)),
            TyKind::Struct(ty) => {
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
    fn emplace_struct_ty(&self, ty: &mut TyKind, struct_ty: &StructTy) {
        match ty {
            TyKind::Fn(func) => {
                for p in func.params.iter_mut() {
                    self.emplace_struct_ty(&mut p.ty, struct_ty)
                }

                self.emplace_struct_ty(&mut func.ret, struct_ty)
            }

            TyKind::Tuple(tys) => {
                for ty in tys.iter_mut() {
                    self.emplace_struct_ty(ty, struct_ty)
                }
            }

            TyKind::Struct(ty) => {
                if ty.qualified_name == struct_ty.qualified_name {
                    *ty = struct_ty.clone();
                } else {
                    for field in ty.fields.iter_mut() {
                        self.emplace_struct_ty(&mut field.ty, struct_ty);
                    }
                }
            }

            TyKind::Type(ty)
            | TyKind::Array(ty, ..)
            | TyKind::Pointer(ty, ..)
            | TyKind::MultiPointer(ty, ..)
            | TyKind::Slice(ty, ..) => self.emplace_struct_ty(ty, struct_ty),

            _ => (),
        }
    }

    fn get_anonymous_struct_name(&self, span: Span) -> Ustr {
        ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
    }

    pub(crate) fn check_block(
        &mut self,
        frame: &mut CheckFrame,
        block: &mut ast::Block,
        expected_ty: Option<TyKind>,
    ) -> DiagnosticResult<CheckResult> {
        self.init_scopes.push_scope();

        let mut result_ty = TyKind::Unit;

        if !block.exprs.is_empty() {
            let last_index = block.exprs.len() - 1;

            for (index, expr) in block.exprs.iter_mut().enumerate() {
                let is_last = index == last_index;

                let ty = self.check_expr(
                    frame,
                    expr,
                    if is_last { expected_ty.clone() } else { None },
                )?;

                if is_last {
                    result_ty = ty.into();
                }
            }
        }

        self.check_expr_list(frame, &mut block.deferred)?;

        self.init_scopes.pop_scope();

        Ok(result_ty)
    }

    fn check_cast(
        &mut self,
        frame: &mut CheckFrame,
        info: &mut ast::Cast,
        expected_ty: Option<TyKind>,
        expr_span: Span,
    ) -> DiagnosticResult<CheckResult> {
        let casted_expr = self.check_expr(frame, &mut info.expr, None)?;

        if let Some(type_expr) = &mut info.type_expr {
            self.check_type_expr(frame, type_expr)
        } else {
            match expected_ty {
                Some(expected_ty) => Ok(self.infcx.normalize_ty(&expected_ty)),
                None => Err(Diagnostic::error()
                    .with_message("can't infer the type cast's target type")
                    .with_labels(vec![Label::primary(expr_span.file_id, expr_span.range())])),
            }
        }
    }

    pub(super) fn check_expr_can_be_mutably_referenced(
        &mut self,
        expr: &ast::Expr,
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
        expr: &ast::Expr,
        is_direct_ref: bool,
    ) -> Result<(), MutabilityRefCheckErr> {
        use MutabilityRefCheckErr::*;

        let ty = self.infcx.normalize_ty_and_untyped(&expr.ty);

        match &expr.kind {
            ast::ExprKind::MemberAccess { expr, member } => {
                match self.check_expr_can_be_mutably_referenced_internal(expr, true) {
                    Ok(_) => match ty {
                        TyKind::Tuple(tys) => {
                            let index = member.parse::<usize>().unwrap();
                            let ty = &tys[index];

                            match ty {
                                TyKind::Slice(_, is_mutable)
                                | TyKind::MultiPointer(_, is_mutable)
                                | TyKind::Pointer(_, is_mutable)
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
                        TyKind::Struct(struct_ty) => {
                            let field_ty = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.symbol == *member)
                                .map(|f| &f.ty)
                                .unwrap();

                            match field_ty {
                                TyKind::Slice(_, is_mutable)
                                | TyKind::MultiPointer(_, is_mutable)
                                | TyKind::Pointer(_, is_mutable)
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
                        TyKind::Module(idx) => {
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
            ast::ExprKind::Id {
                symbol,
                is_mutable,
                binding_span,
                binding_info_idx: _,
            } => {
                match ty {
                    TyKind::Slice(_, is_mutable)
                    | TyKind::MultiPointer(_, is_mutable)
                    | TyKind::Pointer(_, is_mutable) => {
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
