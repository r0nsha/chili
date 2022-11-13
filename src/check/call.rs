use crate::{
    ast,
    check::intrinsics::{can_dispatch_intrinsic_at_comptime, dispatch_intrinsic},
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, TypeError,
    },
    hir::{
        self,
        const_value::{ConstElement, ConstValue},
    },
    infer::{
        coerce::{coerce_array_to_slice, OrCoerceIntoTy},
        display::{DisplayType, OrReportErr},
        normalize::Normalize,
        unify::UnifyType,
    },
    span::Span,
    sym,
    types::*,
};
use indexmap::IndexMap;
use ustr::{Ustr, UstrMap, UstrSet};

use super::{env::Env, Check, CheckResult, CheckSess};

impl Check for ast::Call {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        let callee = self.callee.check(sess, env, None)?;

        match callee.ty().normalize(&sess.tcx) {
            Type::Function(function_type) => check_function_call(sess, env, self, callee, &function_type),
            Type::Type(inner) if inner.is_struct() => check_struct_call(sess, env, self, &inner.into_struct()),
            ty => {
                Err(Diagnostic::error()
                    .with_message(format!(
                        "expected a function or a struct, found `{}`",
                        ty.display(&sess.tcx)
                    ))
                    .with_label(Label::primary(callee.span(), "expression is not callable")))
                // // Try to infer this expression as a function
                // let args = self
                //     .args
                //     .iter()
                //     .map(|arg| arg.value.check(sess, env, None))
                //     .collect::<DiagnosticResult<Vec<_>>>()?;

                // let return_type = sess.tcx.var(self.span);

                // let inferred_function_type = Type::Function(FunctionType {
                //     params: args
                //         .iter()
                //         .map(|arg| FunctionTypeParam {
                //             name: ustr(""),
                //             ty: arg.ty().into(),
                //             default_value: None,
                //         })
                //         .collect(),
                //     return_type: Box::new(return_type.into()),
                //     varargs: None,
                //     kind: FunctionTypeKind::Orphan,
                // });

                // ty.unify(&inferred_function_type, &mut sess.tcx).or_report_err(
                //     &sess.tcx,
                //     &inferred_function_type,
                //     None,
                //     &ty,
                //     self.callee.span(),
                // )?;
                //
                // validate_call_args(sess, &args)?;

                // Ok(hir::Node::Call(hir::Call {
                //     callee: Box::new(callee),
                //     args,
                //     ty: return_type,
                //     span: self.span,
                // }))
            }
        }
    }
}

fn check_function_call(
    sess: &mut CheckSess,
    env: &mut Env,
    call: &ast::Call,
    callee: hir::Node,
    function_type: &FunctionType,
) -> CheckResult {
    let args = build_function_call_args(sess, env, call, function_type)?;
    let ty = sess.tcx.bound(function_type.return_type.as_ref().clone(), call.span);

    if let Some(intrinsic) = can_dispatch_intrinsic_at_comptime(sess, &callee) {
        dispatch_intrinsic(sess, env, &intrinsic, &args, ty, call.span)
    } else {
        Ok(hir::Node::Call(hir::Call {
            callee: Box::new(callee),
            args,
            ty,
            span: call.span,
        }))
    }
}

fn build_function_call_args(
    sess: &mut CheckSess,
    env: &mut Env,
    call: &ast::Call,
    function_type: &FunctionType,
) -> DiagnosticResult<Vec<hir::Node>> {
    fn arg_mismatch(sess: &CheckSess, function_type: &FunctionType, arg_count: usize, span: Span) -> Diagnostic {
        let expected = function_type.params.len();
        let actual = arg_count;

        Diagnostic::error()
            .with_message(format!(
                "function expects {} argument{}, but {} {} supplied",
                expected,
                if expected == 0 || expected > 1 { "s" } else { "" },
                actual,
                if actual == 0 || actual > 1 { "were" } else { "was" },
            ))
            .with_label(Label::primary(
                span,
                format!(
                    "expected {} argument{}, got {}",
                    expected,
                    if expected == 0 || expected > 1 { "s" } else { "" },
                    actual
                ),
            ))
            .with_note(format!("function is of type `{}`", function_type.display(&sess.tcx)))
    }

    enum Varargs {
        Empty,
        Individual(Vec<hir::Node>),
        Spread(hir::Node),
    }

    let mut used_args = UstrMap::<Span>::default();

    let mut args = Vec::<hir::Node>::new();
    let mut vararg_args = Varargs::Empty;

    // If the function was annotated by track_caller, its first argument
    // should be the inserted location parameter: track_caller@location
    let param_offset = match function_type.params.first() {
        Some(param) if param.name == sym::TRACK_CALLER_LOCATION_PARAM => {
            let ty = sess.location_type()?;

            let arg = match sess.get_track_caller_location_param_id(env, call.span) {
                Ok(id) => hir::Node::Id(hir::Id {
                    id,
                    ty,
                    span: call.span,
                }),
                Err(_) => {
                    let value = sess.build_location_value(env, call.span)?;

                    hir::Node::Const(hir::Const {
                        value,
                        ty,
                        span: call.span,
                    })
                }
            };

            args.push(arg);

            1
        }
        _ => 0,
    };

    // Check the arguments passed against the function's parameter types
    for (index, arg) in call.args.iter().enumerate() {
        if let Some(param) = function_type.params.get(index + param_offset) {
            if !param.name.is_empty() {
                used_args.insert(param.name, arg.value.span());
            }

            let param_type = sess.tcx.bound(param.ty.clone(), arg.value.span());
            let mut node = arg.value.check(sess, env, Some(param_type))?;

            node.ty()
                .unify(&param_type, &mut sess.tcx)
                .or_coerce_into_ty(&mut node, &param_type, &mut sess.tcx, sess.target_metrics.word_size)
                .or_report_err(&sess.tcx, &param_type, None, &node.ty(), arg.value.span())?;

            args.push(node);
        } else if let Some(varargs) = &function_type.varargs {
            // this is a variadic argument, meaning that the argument's
            // index is greater than the function's param length
            let mut node = arg.value.check(sess, env, None)?;

            if let Some(vararg_type) = &varargs.ty {
                let is_last = index == call.args.len() - 1;
                match (arg.spread, is_last) {
                    (true, true) => {
                        // This is a spreaded variadic argument
                        match &vararg_args {
                            Varargs::Individual(varargs) => {
                                return Err(Diagnostic::error()
                                    .with_message("variadic arguments cannot be passed and spreaded at the same time")
                                    .with_label(Label::primary(arg.value.span(), "cannot spread this argument"))
                                    .with_label(Label::secondary(
                                        varargs[0].span(),
                                        "first variadic argument passed here",
                                    )))
                            }
                            Varargs::Spread(node) => {
                                return Err(Diagnostic::error()
                                    .with_message("already spreaded variadic arguments")
                                    .with_label(Label::primary(arg.value.span(), "variadic arguments spreaded twice"))
                                    .with_label(Label::secondary(node.span(), "first spread here")))
                            }
                            _ => {
                                let ty = node.ty().normalize(&sess.tcx);

                                match node.ty().normalize(&sess.tcx) {
                                    Type::Pointer(inner, _) => match inner.as_ref() {
                                        Type::Slice(elem_type) => {
                                            elem_type.unify(vararg_type, &mut sess.tcx).or_report_err(
                                                &sess.tcx,
                                                vararg_type,
                                                None,
                                                elem_type.as_ref(),
                                                node.span(),
                                            )?;

                                            vararg_args = Varargs::Spread(node);
                                        }
                                        _ => {
                                            return Err(Diagnostic::error()
                                                .with_message(format!(
                                                    "cannot spread argument of type `{}`",
                                                    ty.display(&sess.tcx)
                                                ))
                                                .with_label(Label::primary(arg.value.span(), "invalid argument type")))
                                        }
                                    },
                                    Type::Array(elem_type, _) => {
                                        elem_type.unify(vararg_type, &mut sess.tcx).or_report_err(
                                            &sess.tcx,
                                            vararg_type,
                                            None,
                                            elem_type.as_ref(),
                                            node.span(),
                                        )?;

                                        let (bound_node, rvalue_node) =
                                            sess.build_rvalue_ref(env, node, false, call.span)?;

                                        let slice_type = Type::slice_pointer(vararg_type.clone(), false);

                                        let varargs_slice =
                                            coerce_array_to_slice(&mut sess.tcx, &rvalue_node, slice_type.clone());

                                        let varargs_seq = hir::Node::Sequence(hir::Sequence {
                                            statements: vec![bound_node, varargs_slice],
                                            ty: sess.tcx.bound(slice_type, call.span),
                                            span: call.span,
                                            is_scope: false,
                                        });

                                        vararg_args = Varargs::Spread(varargs_seq);
                                    }
                                    _ => {
                                        return Err(Diagnostic::error()
                                            .with_message(format!(
                                                "cannot spread argument of type `{}`",
                                                ty.display(&sess.tcx)
                                            ))
                                            .with_label(Label::primary(arg.value.span(), "invalid argument type")))
                                    }
                                }
                            }
                        }
                    }
                    (true, false) => {
                        return Err(Diagnostic::error()
                            .with_message("variadic argument spread must come last")
                            .with_label(Label::primary(arg.value.span(), "invalid argument spread")))
                    }
                    _ => {
                        // This is a regular variadic argument

                        node.ty()
                            .unify(vararg_type, &mut sess.tcx)
                            .or_coerce_into_ty(&mut node, vararg_type, &mut sess.tcx, sess.target_metrics.word_size)
                            .or_report_err(&sess.tcx, vararg_type, None, &node.ty(), arg.value.span())?;

                        match &mut vararg_args {
                            Varargs::Individual(varargs) => {
                                varargs.push(node);
                            }
                            Varargs::Spread(_) => unreachable!(),
                            _ => {
                                vararg_args = Varargs::Individual(vec![node]);
                            }
                        }
                    }
                }
            } else if arg.spread {
                return Err(Diagnostic::error()
                    .with_message("cannot spread untyped variadic arguments")
                    .with_label(Label::primary(arg.value.span(), "cannot spread this argument")));
            } else {
                // This is C varargs, meaning it's untyped
                args.push(node);
            }
        } else {
            // Passed too many arguments
            return Err(arg_mismatch(sess, function_type, call.args.len(), call.span));
        }
    }

    // Build varargs if needed
    if let Some(varargs) = &function_type.varargs {
        if let Some(vararg_type) = &varargs.ty {
            match vararg_args {
                Varargs::Empty => (),
                Varargs::Individual(vararg_args) => {
                    // Build a slice out of the passed variadic arguments
                    let varargs_array_literal =
                        sess.array_literal_or_const(vararg_args, vararg_type.clone(), call.span);

                    let (bound_node, rvalue_node) =
                        sess.build_rvalue_ref(env, varargs_array_literal, false, call.span)?;

                    let slice_type = Type::slice_pointer(vararg_type.clone(), false);

                    let varargs_slice = coerce_array_to_slice(&mut sess.tcx, &rvalue_node, slice_type.clone());

                    let varargs_seq = hir::Node::Sequence(hir::Sequence {
                        statements: vec![bound_node, varargs_slice],
                        ty: sess.tcx.bound(slice_type, call.span),
                        span: call.span,
                        is_scope: false,
                    });

                    args.push(varargs_seq);
                }
                Varargs::Spread(node) => args.push(node),
            }
        }
    }

    let mut named_args = Vec::<(usize, hir::Node)>::with_capacity(call.named_args.len());

    // Check the arguments passed against the function's parameter types
    for arg in call.named_args.iter() {
        if let Some(used_span) = used_args.get(&arg.name.name) {
            return Err(Diagnostic::error()
                .with_message(format!("parameter `{}` is passed twice", arg.name.name,))
                .with_label(Label::primary(arg.name.span, "parameter passed twice"))
                .with_label(Label::secondary(*used_span, "already passed here")));
        }

        if let Some((param_index, param)) = function_type
            .params
            .iter()
            .enumerate()
            .find(|(_, p)| p.name == arg.name.name)
        {
            if !param.name.is_empty() {
                used_args.insert(param.name, arg.value.span());
            }

            let param_type = sess.tcx.bound(param.ty.clone(), arg.value.span());
            let mut node = arg.value.check(sess, env, Some(param_type))?;

            node.ty()
                .unify(&param_type, &mut sess.tcx)
                .or_coerce_into_ty(&mut node, &param_type, &mut sess.tcx, sess.target_metrics.word_size)
                .or_report_err(&sess.tcx, &param_type, None, &node.ty(), arg.value.span())?;

            used_args.insert(param.name, arg.name.span);
            named_args.push((param_index, node));
        } else {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "parameter `{}` doesn't exist in function of type `{}`",
                    arg.name.name,
                    function_type.display(&sess.tcx)
                ))
                .with_label(Label::primary(arg.name.span, "no parameter with this name")));
        }
    }

    // Sort named arguments by their parameters' index
    // PERF: We could try to inserted the named argument in their appropriates indices without
    // sorting them every time...
    named_args.sort_by_key(|(index, _)| *index);
    args.reserve(named_args.len());
    named_args.into_iter().for_each(|(_, named_arg)| args.push(named_arg));

    // Check for arity & apply default arguments if needed
    if args.len() < function_type.params.len() {
        for param in function_type.params.iter().skip(args.len()) {
            if let Some(default_value) = &param.default_value {
                args.push(hir::Node::Const(hir::Const {
                    value: default_value.clone(),
                    ty: sess.tcx.bound(param.ty.clone(), call.span),
                    span: call.span,
                }))
            } else {
                return Err(arg_mismatch(sess, function_type, args.len(), call.span));
            }
        }
    }

    match &function_type.varargs {
        Some(_) if args.len() < function_type.params.len() => {
            return Err(arg_mismatch(sess, function_type, args.len(), call.span))
        }
        None if args.len() != function_type.params.len() => {
            return Err(arg_mismatch(sess, function_type, args.len(), call.span))
        }
        _ => (),
    }

    // Validate that call arguments are of valid types
    for arg in args.iter() {
        match arg.ty().normalize(&sess.tcx) {
            Type::Type(_) | Type::AnyType => {
                return Err(Diagnostic::error()
                    .with_message("types cannot be passed as function arguments")
                    .with_label(Label::primary(arg.span(), "cannot pass type")))
            }
            Type::Module(_) => {
                return Err(Diagnostic::error()
                    .with_message("modules cannot be passed as function arguments")
                    .with_label(Label::primary(arg.span(), "cannot pass module")))
            }
            _ => (),
        }
    }

    Ok(args)
}

fn check_struct_call(sess: &mut CheckSess, env: &mut Env, call: &ast::Call, struct_type: &StructType) -> CheckResult {
    let mut used_fields = UstrMap::<Span>::default();
    let mut uninit_fields = UstrSet::from_iter(struct_type.fields.iter().map(|f| f.name));

    let mut field_nodes: Vec<hir::StructLiteralField> = vec![];

    if struct_type.is_union() && !call.args.is_empty() {
        return Err(Diagnostic::error()
            .with_message("positional argument are not allowed in union literals, only 1 named argument is allowed")
            .with_label(Label::primary(
                call.span,
                format!("type is `{}`", struct_type.display(&sess.tcx)),
            )));
    }

    for (index, arg) in call.args.iter().enumerate() {
        if arg.spread {
            return Err(Diagnostic::error()
                .with_message("cannot spread arguments into struct fields")
                .with_label(Label::primary(call.span, "spreading isn't allowed")));
        }

        match struct_type.fields.get(index) {
            Some(field) => {
                used_fields.insert(field.name, arg.value.span());
                uninit_fields.remove(&field.name);

                let expected_type = sess.tcx.bound(field.ty.clone(), field.span);
                let mut node = arg.value.check(sess, env, Some(expected_type))?;

                node.ty()
                    .unify(&expected_type, &mut sess.tcx)
                    .or_coerce_into_ty(&mut node, &expected_type, &mut sess.tcx, sess.target_metrics.word_size)
                    .or_report_err(
                        &sess.tcx,
                        &expected_type,
                        Some(field.span),
                        &node.ty(),
                        arg.value.span(),
                    )?;

                field_nodes.push(hir::StructLiteralField {
                    ty: node.ty(),
                    span: arg.value.span(),
                    name: field.name,
                    value: Box::new(node),
                });
            }
            None => {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "struct `{}` expects {} fields, but {} were passed",
                        struct_type.name,
                        struct_type.fields.len(),
                        call.args.len()
                    ))
                    .with_label(Label::primary(call.span, "passed too many fields")))
            }
        }
    }

    for arg in call.named_args.iter() {
        if arg.spread {
            return Err(Diagnostic::error()
                .with_message("cannot spread arguments into struct fields")
                .with_label(Label::primary(call.span, "spreading isn't allowed")));
        }

        match struct_type.field(arg.name.name) {
            Some(field) => {
                if let Some(used_span) = used_fields.get(&arg.name.name) {
                    return Err(Diagnostic::error()
                        .with_message(format!("field `{}` is passed twice", arg.name.name,))
                        .with_label(Label::primary(arg.name.span, "field passed twice"))
                        .with_label(Label::secondary(*used_span, "already passed here")));
                }

                used_fields.insert(field.name, arg.value.span());
                uninit_fields.remove(&field.name);

                let expected_type = sess.tcx.bound(field.ty.clone(), field.span);
                let mut node = arg.value.check(sess, env, Some(expected_type))?;

                node.ty()
                    .unify(&expected_type, &mut sess.tcx)
                    .or_coerce_into_ty(&mut node, &expected_type, &mut sess.tcx, sess.target_metrics.word_size)
                    .or_report_err(
                        &sess.tcx,
                        &expected_type,
                        Some(field.span),
                        &node.ty(),
                        arg.value.span(),
                    )?;

                field_nodes.push(hir::StructLiteralField {
                    ty: node.ty(),
                    span: arg.value.span(),
                    name: field.name,
                    value: Box::new(node),
                });
            }
            None => {
                return Err(TypeError::invalid_struct_field(
                    arg.name.span,
                    arg.name.name,
                    struct_type.display(&sess.tcx),
                ));
            }
        }
    }

    if struct_type.is_union() && field_nodes.len() != 1 {
        return Err(Diagnostic::error()
            .with_message("union literal should have exactly one field")
            .with_label(Label::primary(
                call.span,
                format!("type is `{}`", struct_type.display(&sess.tcx)),
            )));
    }

    if !struct_type.is_union() && !uninit_fields.is_empty() {
        let mut uninit_fields_str = uninit_fields.iter().map(|f| f.as_str()).collect::<Vec<&str>>();

        uninit_fields_str.reverse();
        return Err(Diagnostic::error()
            .with_message(format!("missing struct fields: {}", uninit_fields_str.join(", ")))
            .with_label(Label::primary(call.span, "missing fields")));
    }

    Ok(make_struct_literal_node(sess, field_nodes, struct_type, call.span))
}

fn make_struct_literal_node(
    sess: &mut CheckSess,
    field_nodes: Vec<hir::StructLiteralField>,
    struct_ty: &StructType,
    span: Span,
) -> hir::Node {
    let is_const_struct = field_nodes.iter().all(|f| f.value.is_const());

    if is_const_struct {
        let mut const_value_fields = IndexMap::<Ustr, ConstElement>::new();

        for field in field_nodes.iter() {
            const_value_fields.insert(
                field.name,
                ConstElement {
                    value: field.value.as_const_value().unwrap().clone(),
                    ty: field.ty,
                },
            );
        }

        hir::Node::Const(hir::Const {
            value: ConstValue::Struct(const_value_fields),
            ty: sess.tcx.bound(Type::Struct(struct_ty.clone()), span),
            span,
        })
    } else {
        hir::Node::Literal(hir::Literal::Struct(hir::StructLiteral {
            ty: sess.tcx.bound(Type::Struct(struct_ty.clone()), span),
            span,
            fields: field_nodes,
        }))
    }
}
