mod builtin;
mod cast;
pub mod display;
mod environment;
pub mod normalize;
mod substitute;
mod top_level;
pub mod ty_ctx;
pub mod unify;

use crate::{
    cast::CanCast,
    display::{map_unify_err, DisplayTy},
    normalize::NormalizeTy,
    top_level::CheckTopLevel,
    ty_ctx::TyCtx,
    unify::UnifyTy,
};
use chili_ast::{
    ast,
    pattern::{Pattern, SymbolPattern},
    ty::*,
    value::Value,
    workspace::{BindingInfoId, ModuleId, ModuleInfo, ScopeLevel, Workspace},
};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use environment::Environment;
use std::collections::HashMap;
use ustr::{ustr, UstrMap};

pub fn check(
    workspace: &mut Workspace,
    ast: ast::ResolvedAst,
) -> DiagnosticResult<(ast::ResolvedAst, TyCtx)> {
    let mut sess = CheckSess::new(workspace, &ast);
    sess.start()?;
    Ok((sess.new_ast, sess.tycx))
}

pub(crate) struct CheckSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) tycx: TyCtx,

    // The ast being processed
    pub(crate) old_ast: &'s ast::ResolvedAst,

    // The new ast being generated
    pub(crate) new_ast: ast::ResolvedAst,

    // A map from binding ids to their const value, if the have one
    pub(crate) const_bindings: HashMap<BindingInfoId, Value>,

    // Contains information about scope, builtin symbols, modules, etc.
    pub(crate) env: Environment,

    // Stack of function frames, each ast::Fn creates its own frame
    pub(crate) function_frames: Vec<FunctionFrame>,

    // Stack of `Self` types
    pub(crate) self_types: Vec<Ty>,

    // The current loop (while/for) depth
    // 0 meaning we are not in a loop, > 1 means we are in a loop
    pub(crate) loop_depth: usize,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionFrame {
    return_ty: Ty,
    scope_level: ScopeLevel,
}

impl<'s> CheckSess<'s> {
    pub(crate) fn new(workspace: &'s mut Workspace, old_ast: &'s ast::ResolvedAst) -> Self {
        let mut tycx = TyCtx::new();
        let env = Environment::new(workspace, &mut tycx);

        Self {
            workspace,
            tycx,
            old_ast,
            new_ast: ast::ResolvedAst::new(),
            const_bindings: HashMap::new(),
            env,
            function_frames: vec![],
            self_types: vec![],
            loop_depth: 0,
        }
    }

    pub(crate) fn start(&mut self) -> DiagnosticResult<()> {
        for binding in self.old_ast.bindings.iter() {
            match &binding.pattern {
                Pattern::Single(pat) => {
                    let binding_info = self
                        .workspace
                        .get_binding_info(pat.binding_info_id)
                        .unwrap();

                    if binding_info.ty != Ty::unknown() {
                        continue;
                    }

                    binding.clone().check_top_level(self)?;
                }
                Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => {
                    let span = binding.pattern.span();
                    return Err(Diagnostic::error()
                        .with_message("this pattern is not supported yet for global bindings")
                        .with_labels(vec![Label::primary(span.file_id, span.range())
                            .with_message("not supported yet")]));
                }
            };
        }

        Ok(())
    }

    pub(crate) fn with_function_frame<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        frame: FunctionFrame,
        mut f: F,
    ) -> T {
        self.function_frames.push(frame);
        let result = f(self);
        self.function_frames.pop();
        result
    }

    pub(crate) fn function_frame(&self) -> Option<FunctionFrame> {
        self.function_frames.last().map(|&f| f)
    }

    pub(crate) fn extract_const_type(
        &self,
        const_value: Option<Value>,
        ty: Ty,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        match const_value {
            Some(v) => {
                if let Value::Type(t) = v {
                    return Ok(t);
                }
            }
            None => (),
        }

        Err(TypeError::expected(span, ty.display(&self.tycx), "a type"))
    }
}

pub(crate) type CheckResult = DiagnosticResult<Res>;

pub(crate) struct Res {
    ty: Ty,
    const_value: Option<Value>,
}

impl Res {
    pub(crate) fn new(ty: Ty) -> Self {
        Self {
            ty,
            const_value: None,
        }
    }

    pub(crate) fn new_maybe_const(ty: Ty, const_value: Option<Value>) -> Self {
        Self { ty, const_value }
    }

    pub(crate) fn new_const(ty: Ty, const_value: Value) -> Self {
        Self {
            ty,
            const_value: Some(const_value),
        }
    }
}

pub(crate) trait Check
where
    Self: Sized,
{
    fn check(&mut self, sess: &mut CheckSess, expected_ty: Option<Ty>) -> CheckResult;
}

impl Check for ast::Import {
    fn check(&mut self, sess: &mut CheckSess, _expected_ty: Option<Ty>) -> CheckResult {
        self.module_id = sess.workspace.find_module_info(self.module_info).unwrap();

        self.binding_info_id = sess.env.add_binding(
            sess.workspace,
            self.alias,
            self.visibility,
            false,
            ast::BindingKind::Import,
            self.span,
        );

        let mut ty = sess.tycx.bound(TyKind::Module(self.module_id));
        let mut const_value = None;

        let mut module_id = self.module_id;
        let mut target_binding_info = None;

        for (index, node) in self.import_path.iter().enumerate() {
            let id =
                sess.find_binding_info_id_in_module(module_id, node.value.as_symbol(), node.span)?;

            target_binding_info = Some(id);

            let res = sess.check_binding_by_id(id)?;
            ty = res.ty;
            const_value = res.const_value;

            match ty.normalize(&sess.tycx) {
                TyKind::Module(id) => module_id = id,
                _ => {
                    if index < self.import_path.len() - 1 {
                        return Err(TypeError::expected(node.span, ty.to_string(), "a module"));
                    }
                }
            }
        }

        sess.workspace
            .get_binding_info_mut(self.binding_info_id)
            .unwrap()
            .ty = ty;

        self.target_binding_info = target_binding_info;

        Ok(Res::new_maybe_const(ty, const_value))
    }
}

impl Check for ast::Binding {
    fn check(&mut self, sess: &mut CheckSess, _expected_ty: Option<Ty>) -> CheckResult {
        // TODO: support other patterns
        let pat = self.pattern.as_single_ref();
        let binding_ty = sess.tycx.var();

        sess.workspace
            .get_binding_info_mut(pat.binding_info_id)
            .unwrap()
            .ty = binding_ty;

        if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, None)?;
            let typ = sess.extract_const_type(res.const_value, res.ty, ty_expr.span)?;
            typ.unify(&binding_ty, sess)
                .map_err(|e| map_unify_err(e, typ, binding_ty, ty_expr.span, &sess.tycx))?;
        }

        let const_value = if let Some(expr) = &mut self.expr {
            let res = expr.check(sess, Some(binding_ty))?;
            res.ty
                .unify(&binding_ty, sess)
                .map_err(|e| map_unify_err(e, binding_ty, res.ty, expr.span, &sess.tycx))?;
            res.const_value
        } else {
            None
        };

        // don't allow const values with mutable bindings or patterns that are not `Single`
        let const_value = match &self.pattern {
            Pattern::Single(SymbolPattern { is_mutable, .. }) => {
                if *is_mutable {
                    None
                } else {
                    const_value
                }
            }
            Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => None,
        };

        if let Some(const_value) = const_value {
            sess.const_bindings.insert(pat.binding_info_id, const_value);
        }

        Ok(Res {
            ty: sess.tycx.common_types.unit,
            const_value: None,
        })
    }
}

impl Check for ast::Fn {
    fn check(&mut self, sess: &mut CheckSess, expected_ty: Option<Ty>) -> CheckResult {
        let res = self.sig.check(sess, expected_ty)?;

        let fn_ty = sess.tycx.ty_kind(res.ty);
        let fn_ty = fn_ty.as_fn();

        let return_ty = sess.tycx.bound(fn_ty.ret.as_ref().clone());

        let body_res = sess.with_function_frame(
            FunctionFrame {
                return_ty,
                scope_level: sess.env.scope_level(),
            },
            |sess| self.body.check(sess, None),
        )?;

        body_res
            .ty
            .unify(&return_ty, sess)
            .map_err(|e| map_unify_err(e, return_ty, body_res.ty, self.body.span, &sess.tycx))?;

        Ok(Res::new(res.ty))
    }
}

impl Check for ast::FnSig {
    fn check(&mut self, sess: &mut CheckSess, expected_ty: Option<Ty>) -> CheckResult {
        let mut ty_params = vec![];

        for param in self.params.iter_mut() {
            // TODO: support other patterns
            let pat = param.pattern.as_single_ref();

            let ty = if let Some(expr) = &mut param.ty {
                let res = expr.check(sess, None)?;
                sess.extract_const_type(res.const_value, res.ty, expr.span)?
            } else {
                sess.tycx.var()
            };

            sess.workspace
                .get_binding_info_mut(pat.binding_info_id)
                .unwrap()
                .ty = ty;

            ty_params.push(FnTyParam {
                symbol: pat.symbol,
                ty: ty.into(),
            });
        }

        let ret = if let Some(expr) = &mut self.ret {
            let res = expr.check(sess, None)?;
            sess.extract_const_type(res.const_value, res.ty, expr.span)?
        } else {
            sess.tycx.var()
        };

        self.ty = sess.tycx.bound(TyKind::Fn(FnTy {
            params: ty_params,
            ret: Box::new(ret.into()),
            variadic: self.variadic,
            lib_name: self.lib_name,
        }));

        Ok(Res {
            ty: self.ty,
            const_value: None,
        })
    }
}

impl Check for ast::Expr {
    fn check(&mut self, sess: &mut CheckSess, expected_ty: Option<Ty>) -> CheckResult {
        let res = match &mut self.kind {
            ast::ExprKind::Import(_) => todo!(),
            ast::ExprKind::Foreign(_) => todo!(),
            ast::ExprKind::Binding(binding) => binding.check(sess, None),
            ast::ExprKind::Defer(_) => todo!(),
            ast::ExprKind::Assign { lvalue, rvalue } => todo!(),
            ast::ExprKind::Cast(cast) => cast.check(sess, expected_ty),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    let res = expr.check(sess, None)?;
                    sess.extract_const_type(res.const_value, res.ty, expr.span)?;
                    Ok(Res::new(sess.tycx.common_types.uint))
                }
                ast::Builtin::Panic(expr) => {
                    if let Some(expr) = expr {
                        expr.check(sess, None)?;
                    }
                    Ok(Res::new(sess.tycx.common_types.unit))
                }
            },
            ast::ExprKind::Fn(f) => f.check(sess, expected_ty),
            ast::ExprKind::While { cond, block } => {
                let cond_ty = sess.tycx.common_types.bool;
                let cond_res = cond.check(sess, Some(cond_ty))?;

                cond_res
                    .ty
                    .unify(&cond_ty, sess)
                    .map_err(|e| map_unify_err(e, cond_ty, cond_res.ty, cond.span, &sess.tycx))?;

                sess.loop_depth += 1;
                block.check(sess, None)?;
                sess.loop_depth -= 1;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => todo!(),
                    ast::ForIter::Value(value) => todo!(),
                }

                sess.loop_depth += 1;
                for_.block.check(sess, None)?;
                sess.loop_depth -= 1;
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Break { deferred } => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "break"));
                }

                for expr in deferred.iter_mut() {
                    expr.check(sess, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Continue { deferred } => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "continue"));
                }

                for expr in deferred.iter_mut() {
                    expr.check(sess, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Return { expr, deferred } => {
                let function_frame = sess
                    .function_frame()
                    .ok_or(SyntaxError::outside_of_function(self.span, "return"))?;

                if let Some(expr) = expr {
                    let expected = function_frame.return_ty;
                    let res = expr.check(sess, Some(expected))?;
                    res.ty
                        .unify(&expected, sess)
                        .map_err(|e| map_unify_err(e, expected, res.ty, expr.span, &sess.tycx))?;
                } else {
                    let expected = sess.tycx.common_types.unit;
                    function_frame
                        .return_ty
                        .unify(&expected, sess)
                        .map_err(|e| {
                            map_unify_err(
                                e,
                                expected,
                                function_frame.return_ty,
                                self.span,
                                &sess.tycx,
                            )
                        })?;
                }

                for expr in deferred.iter_mut() {
                    expr.check(sess, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Binary { lhs, op, rhs } => todo!(),
            ast::ExprKind::Unary { op, lhs } => todo!(),
            ast::ExprKind::Subscript { expr, index } => todo!(),
            ast::ExprKind::Slice { expr, low, high } => todo!(),
            ast::ExprKind::FnCall(call) => call.check(sess, expected_ty),
            ast::ExprKind::MemberAccess { expr, member } => {
                let res = expr.check(sess, None)?;
                let kind = res.ty.normalize(&sess.tycx);

                match &kind.maybe_deref_once() {
                    ty @ TyKind::Tuple(tys) => match member.as_str().parse::<i32>() {
                        Ok(index) => match tys.get(index as usize) {
                            Some(field_ty) => Ok(Res::new(sess.tycx.bound(field_ty.clone()))),
                            None => Err(TypeError::tuple_field_out_of_bounds(
                                expr.span,
                                &member,
                                ty.to_string(),
                                tys.len() - 1,
                            )),
                        },
                        Err(_) => Err(TypeError::non_numeric_tuple_field(
                            expr.span,
                            &member,
                            ty.to_string(),
                        )),
                    },
                    TyKind::Struct(ty) => match ty.fields.iter().find(|f| f.symbol == *member) {
                        Some(field) => Ok(Res::new(sess.tycx.bound(field.ty.clone()))),
                        None => Err(TypeError::invalid_struct_field(
                            expr.span,
                            *member,
                            ty.to_string(),
                        )),
                    },
                    TyKind::Array(_, size) if member.as_str() == BUILTIN_FIELD_LEN => Ok(
                        Res::new_const(sess.tycx.common_types.uint, Value::Int(*size as _)),
                    ),
                    TyKind::Slice(..) if member.as_str() == BUILTIN_FIELD_LEN => {
                        Ok(Res::new(sess.tycx.common_types.uint))
                    }
                    TyKind::Slice(inner, is_mutable) if member.as_str() == BUILTIN_FIELD_DATA => {
                        Ok(Res::new(
                            sess.tycx
                                .bound(TyKind::MultiPointer(inner.clone(), *is_mutable)),
                        ))
                    }
                    TyKind::Module(module_id) => {
                        let binding_info_id =
                            sess.find_binding_info_id_in_module(*module_id, *member, self.span)?;
                        sess.check_binding_by_id(binding_info_id)
                    }
                    ty => Err(TypeError::member_access_on_invalid_type(
                        expr.span,
                        ty.to_string(),
                    )),
                }
            }
            ast::ExprKind::Ident {
                binding_info_id, ..
            } => sess.check_binding_by_id(*binding_info_id),
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral { type_expr, fields } => todo!(),
            ast::ExprKind::Literal(lit) => lit.check(sess, expected_ty),
            ast::ExprKind::PointerType(inner, is_mutable) => {
                let res = inner.check(sess, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Pointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::MultiPointerType(inner, is_mutable) => {
                let res = inner.check(sess, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::MultiPointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::ArrayType(_, _) => todo!(),
            ast::ExprKind::SliceType(inner, is_mutable) => {
                let res = inner.check(sess, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Slice(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::StructType(_) => todo!(),
            ast::ExprKind::FnType(sig) => sig.check(sess, expected_ty),
            ast::ExprKind::SelfType => match sess.self_types.last() {
                Some(&ty) => Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                )),
                None => Err(Diagnostic::error()
                    .with_message("`Self` is only available within struct definitions")
                    .with_labels(vec![Label::primary(
                        self.span.file_id,
                        self.span.range().clone(),
                    )])),
            },
            ast::ExprKind::NeverType => {
                let ty = sess.tycx.common_types.never;
                Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                ))
            }
            ast::ExprKind::UnitType => {
                let ty = sess.tycx.common_types.unit;
                Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                ))
            }
            ast::ExprKind::PlaceholderType => {
                let ty = sess.tycx.var();
                Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                ))
            }
            ast::ExprKind::Noop => Ok(Res::new(sess.tycx.var())),
        }?;

        self.ty = res.ty;

        Ok(res)
    }
}

impl Check for ast::FnCall {
    fn check(&mut self, sess: &mut CheckSess, _expected_ty: Option<Ty>) -> CheckResult {
        let callee_res = self.callee.check(sess, None)?;

        match callee_res.ty.normalize(&sess.tycx) {
            TyKind::Fn(fn_ty) => {
                if fn_ty.variadic {
                    if self.args.len() < fn_ty.params.len() {
                        return Err(TypeError::fn_call_arity_mismatch(
                            self.span,
                            fn_ty.params.len(),
                            self.args.len(),
                        ));
                    }
                } else if self.args.len() != fn_ty.params.len() {
                    return Err(TypeError::fn_call_arity_mismatch(
                        self.span,
                        fn_ty.params.len(),
                        self.args.len(),
                    ));
                }

                let mut passed_args = UstrMap::default();

                for (index, arg) in self.args.iter_mut().enumerate() {
                    if let Some(symbol) = &arg.symbol {
                        // this is a named argument

                        if let Some(passed_span) = passed_args.insert(symbol.value, symbol.span) {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "duplicate argument passed `{}`",
                                    symbol.value
                                ))
                                .with_labels(vec![
                                    Label::primary(symbol.span.file_id, symbol.span.range())
                                        .with_message("duplicate passed here"),
                                    Label::secondary(passed_span.file_id, passed_span.range())
                                        .with_message("has already been passed here"),
                                ]));
                        }

                        let found_param_index =
                            fn_ty.params.iter().position(|p| p.symbol == symbol.value);

                        if let Some(index) = found_param_index {
                            let param_ty = sess.tycx.bound(fn_ty.params[index].ty.clone());
                            let res = arg.expr.check(sess, Some(param_ty))?;

                            res.ty.unify(&param_ty, sess).map_err(|e| {
                                map_unify_err(e, param_ty, res.ty, arg.expr.span, &sess.tycx)
                            })?;
                            // TODO: coerce
                            // TODO: self.infcx
                            // TODO:     .unify_or_coerce_ty_expr(&param_ty, &mut arg.expr)?;
                        } else {
                            return Err(Diagnostic::error()
                                .with_message(format!("unknown argument `{}`", symbol.value))
                                .with_labels(vec![Label::primary(
                                    symbol.span.file_id,
                                    symbol.span.range(),
                                )]));
                        }
                    } else {
                        // this is a positional argument
                        if let Some(param) = fn_ty.params.get(index) {
                            passed_args.insert(param.symbol, arg.expr.span);

                            let param_ty = sess.tycx.bound(fn_ty.params[index].ty.clone());
                            let res = arg.expr.check(sess, Some(param_ty))?;

                            res.ty.unify(&param_ty, sess).map_err(|e| {
                                map_unify_err(e, param_ty, res.ty, arg.expr.span, &sess.tycx)
                            })?;

                            // TODO: coerce
                            // TODO: self.infcx
                            // TODO:     .unify_or_coerce_ty_expr(&param_ty, &mut arg.expr)?;
                        } else {
                            // this is a variadic argument, meaning that the argument's
                            // index is greater than the function's param length
                            arg.expr.check(sess, None)?;
                        }
                    };
                }

                Ok(Res::new(sess.tycx.bound(fn_ty.ret.as_ref().clone())))
            }
            ty => {
                for arg in self.args.iter_mut() {
                    arg.expr.check(sess, None)?;
                }

                let return_ty = sess.tycx.var();

                let inferred_fn_ty = TyKind::Fn(FnTy {
                    params: self
                        .args
                        .iter()
                        .map(|arg| FnTyParam {
                            symbol: arg.symbol.as_ref().map_or(ustr(""), |s| s.value),
                            ty: arg.expr.ty.into(),
                        })
                        .collect(),
                    ret: Box::new(return_ty.into()),
                    variadic: false,
                    lib_name: None,
                });

                ty.unify(&inferred_fn_ty, sess).map_err(|e| {
                    map_unify_err(e, inferred_fn_ty, ty, self.callee.span, &sess.tycx)
                })?;

                Ok(Res::new(return_ty))
            }
        }
    }
}

impl Check for ast::Cast {
    fn check(&mut self, sess: &mut CheckSess, expected_ty: Option<Ty>) -> CheckResult {
        let res = self.expr.check(sess, None)?;

        self.target_ty = if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, None)?;
            sess.extract_const_type(res.const_value, res.ty, ty_expr.span)?
        } else {
            match expected_ty {
                Some(t) => t,
                None => {
                    return Err(Diagnostic::error()
                        .with_message("can't infer the type cast's target type")
                        .with_labels(vec![Label::primary(
                            self.expr.span.file_id,
                            self.expr.span.range(),
                        )]))
                }
            }
        };

        let source_ty = res.ty.normalize(&sess.tycx);
        let target_ty = self.target_ty.normalize(&sess.tycx);

        if source_ty.can_cast(&target_ty) {
            Ok(Res::new(self.target_ty))
        } else {
            Err(Diagnostic::error()
                .with_message(format!(
                    "cannot cast from `{}` to `{}`",
                    source_ty, target_ty
                ))
                .with_labels(vec![Label::primary(
                    self.expr.span.file_id,
                    self.expr.span.range(),
                )
                .with_message(format!("invalid cast to `{}`", target_ty))]))
        }
    }
}

impl Check for ast::Block {
    fn check(&mut self, sess: &mut CheckSess, expected_ty: Option<Ty>) -> CheckResult {
        let mut res = Res::new(sess.tycx.common_types.unit);

        if !self.exprs.is_empty() {
            let last_index = self.exprs.len() - 1;

            for (index, expr) in self.exprs.iter_mut().enumerate() {
                res = expr.check(
                    sess,
                    if index == last_index {
                        expected_ty
                    } else {
                        None
                    },
                )?;
            }
        }
        for expr in self.deferred.iter_mut() {
            expr.check(sess, None)?;
        }

        Ok(res)
    }
}

impl Check for ast::Literal {
    fn check(&mut self, sess: &mut CheckSess, _expected_ty: Option<Ty>) -> CheckResult {
        let res = match self {
            ast::Literal::Unit => Res::new(sess.tycx.common_types.unit),
            ast::Literal::Nil => Res::new(sess.tycx.var()),
            ast::Literal::Bool(b) => Res::new_const(sess.tycx.common_types.bool, Value::Bool(*b)),
            ast::Literal::Int(i) => Res::new_const(sess.tycx.anyint(), Value::Int(*i)),
            ast::Literal::Float(f) => Res::new_const(sess.tycx.anyfloat(), Value::Float(*f)),
            ast::Literal::Str(_) => Res::new(sess.tycx.common_types.str),
            ast::Literal::Char(_) => Res::new(sess.tycx.common_types.u8),
        };
        Ok(res)
    }
}
