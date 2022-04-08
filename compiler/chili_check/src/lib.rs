mod bind;
mod builtin;
mod cast;
mod coerce;
mod const_fold;
pub mod display;
mod env;
pub mod normalize;
mod substitute;
mod top_level;
pub mod ty_ctx;
pub mod unify;

use crate::{
    cast::CanCast, display::DisplayTy, normalize::NormalizeTy, top_level::CheckTopLevel,
    ty_ctx::TyCtx, unify::UnifyTy,
};
use chili_ast::{
    ast::{self, ForeignLibrary},
    pattern::Pattern,
    ty::{FnTy, FnTyParam, StructTy, StructTyField, StructTyKind, Ty, TyKind},
    value::Value,
    workspace::{BindingInfoFlags, BindingInfoId, ModuleId, ScopeLevel, Workspace},
};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use coerce::{OrCoerceExprIntoTy, OrCoerceExprs};
use common::{
    builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    target::TargetMetrics,
};
use const_fold::binary::const_fold_binary;
use display::OrReportErr;
use env::{Env, Scope};
use std::collections::HashMap;
use top_level::CallerInfo;
use ustr::{ustr, Ustr, UstrMap, UstrSet};

pub fn check(
    workspace: &mut Workspace,
    ast: Vec<ast::Ast>,
) -> DiagnosticResult<(ast::TypedAst, TyCtx)> {
    let mut sess = CheckSess::new(workspace, &ast);
    sess.start()?;
    Ok((sess.new_ast, sess.tycx))
}

pub(crate) struct CheckSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) target_metrics: TargetMetrics,

    pub(crate) tycx: TyCtx,

    // The ast's being processed
    pub(crate) old_asts: &'s Vec<ast::Ast>,

    // The new typed ast being generated
    pub(crate) new_ast: ast::TypedAst,

    // Information that's relevant for the global context
    pub(crate) global_scopes: HashMap<ModuleId, Scope>,
    pub(crate) builtin_types: UstrMap<BindingInfoId>,

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
    pub(crate) fn new(workspace: &'s mut Workspace, old_ast: &'s Vec<ast::Ast>) -> Self {
        let target_metrics = workspace.build_options.target_platform.metrics();
        Self {
            workspace,
            target_metrics,
            tycx: TyCtx::new(),
            old_asts: old_ast,
            new_ast: ast::TypedAst::new(),
            global_scopes: HashMap::default(),
            builtin_types: UstrMap::default(),
            function_frames: vec![],
            self_types: vec![],
            loop_depth: 0,
        }
    }

    pub(crate) fn start(&mut self) -> DiagnosticResult<()> {
        self.add_builtin_types();

        for ast in self.old_asts.iter() {
            let module_id = ast.module_id;
            for binding in ast.bindings.iter() {
                match &binding.pattern {
                    Pattern::Single(pat) => {
                        if let None = self.get_global_symbol(module_id, pat.symbol) {
                            binding.clone().check_top_level(self, module_id)?;
                        }
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

            for import in ast.imports.iter() {
                if let None = self.get_global_symbol(module_id, import.alias) {
                    import.clone().check_top_level(self, module_id)?;
                }
            }
        }

        // Check that an entry point function exists
        // Note (Ron): This won't be relevant for targets like WASM or libraries
        if self.workspace.entry_point_function_id.is_none() {
            return Err(Diagnostic::error()
                .with_message("entry point function `main` is not defined")
                .with_notes(vec![
                    "define function `let main = fn() {}` in your entry file".to_string(),
                ]));
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

    pub(crate) fn with_env<T, F: FnMut(&mut Self, Env) -> T>(
        &mut self,
        module_id: ModuleId,
        mut f: F,
    ) -> T {
        let module_info = *self.workspace.get_module_info(module_id).unwrap();
        f(self, Env::new(module_id, module_info))
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

    pub(crate) fn extract_const_int(
        &mut self,
        value: Option<Value>,
        ty: Ty,
        span: Span,
    ) -> DiagnosticResult<i64> {
        match &value {
            Some(value) => {
                if !value.is_int() {
                    Err(TypeError::expected(
                        span,
                        ty.display(&self.tycx),
                        "compile-time known integer",
                    ))
                } else {
                    Ok(value.clone().into_int())
                }
            }
            None => Err(TypeError::expected(
                span,
                ty.display(&self.tycx),
                "compile-time known integer",
            )),
        }
    }

    fn add_builtin_types(&mut self) {
        let mk = |sess: &mut CheckSess, symbol: &str, ty: Ty| {
            let symbol = ustr(symbol);

            let id = sess.workspace.add_binding_info(
                Default::default(),
                symbol,
                ast::Visibility::Public,
                sess.tycx.bound(ty.kind().create_type()),
                Some(Value::Type(ty)),
                false,
                ast::BindingKind::Type,
                ScopeLevel::Global,
                ustr(""),
                Span::unknown(),
            );

            let info = sess.workspace.get_binding_info_mut(id).unwrap();
            info.flags.insert(BindingInfoFlags::BUILTIN_TYPE);

            sess.builtin_types.insert(symbol, id);
        };

        mk(self, builtin::SYM_UNIT, self.tycx.common_types.unit);
        mk(self, builtin::SYM_BOOL, self.tycx.common_types.bool);

        mk(self, builtin::SYM_I8, self.tycx.common_types.i8);
        mk(self, builtin::SYM_I16, self.tycx.common_types.i16);
        mk(self, builtin::SYM_I32, self.tycx.common_types.i32);
        mk(self, builtin::SYM_I64, self.tycx.common_types.i64);
        mk(self, builtin::SYM_INT, self.tycx.common_types.int);

        mk(self, builtin::SYM_U8, self.tycx.common_types.u8);
        mk(self, builtin::SYM_U16, self.tycx.common_types.u16);
        mk(self, builtin::SYM_U32, self.tycx.common_types.u32);
        mk(self, builtin::SYM_U64, self.tycx.common_types.u64);
        mk(self, builtin::SYM_UINT, self.tycx.common_types.uint);

        mk(self, builtin::SYM_F16, self.tycx.common_types.f16);
        mk(self, builtin::SYM_F32, self.tycx.common_types.f32);
        mk(self, builtin::SYM_F64, self.tycx.common_types.f64);
        mk(self, builtin::SYM_FLOAT, self.tycx.common_types.float);

        mk(self, builtin::SYM_STR, self.tycx.common_types.str);

        mk(self, builtin::SYM_NEVER, self.tycx.common_types.never);
    }
}

pub(crate) type CheckResult = DiagnosticResult<Res>;

#[derive(Debug)]
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
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult;
}

impl Check for ast::Import {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_ty: Option<Ty>,
    ) -> CheckResult {
        self.target_module_id = sess
            .workspace
            .find_module_info(self.target_module_info)
            .unwrap();

        let mut previous_module_id = env.module_id();
        let mut module_id = self.target_module_id;

        let mut ty = sess.tycx.bound(TyKind::Module(self.target_module_id));
        let mut const_value = None;
        let mut target_binding_info = None;

        for (index, node) in self.import_path.iter().enumerate() {
            let (res, id) = sess.check_top_level_symbol(
                CallerInfo {
                    module_id: previous_module_id,
                    span: node.span,
                },
                module_id,
                node.value.as_symbol(),
            )?;

            previous_module_id = module_id;

            target_binding_info = Some(id);
            ty = res.ty;
            const_value = res.const_value;

            match ty.normalize(&sess.tycx) {
                TyKind::Module(id) => module_id = id,
                _ => {
                    if index < self.import_path.len() - 1 {
                        return Err(TypeError::expected(
                            node.span,
                            ty.display(&sess.tycx),
                            "a module",
                        ));
                    }
                }
            }
        }

        self.target_binding_info = target_binding_info;

        self.binding_info_id = sess.bind_symbol(
            env,
            self.alias,
            self.visibility,
            ty,
            const_value,
            false,
            ast::BindingKind::Import,
            self.span,
        )?;

        Ok(Res::new_maybe_const(ty, const_value))
    }
}

impl Check for ast::Binding {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_ty: Option<Ty>,
    ) -> CheckResult {
        self.module_id = env.module_id();

        // Collect foreign libraries to be linked later
        if let Some(lib) = self.lib_name {
            sess.workspace
                .foreign_libraries
                .insert(ForeignLibrary::from_str(
                    &lib,
                    env.module_info().file_path,
                    self.pattern.span(),
                )?);
        }

        self.ty = if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, env, None)?;
            sess.extract_const_type(res.const_value, res.ty, ty_expr.span)?
        } else {
            sess.tycx.var()
        };

        let const_value = if let Some(expr) = &mut self.expr {
            let res = expr.check(sess, env, Some(self.ty))?;

            res.ty
                .unify(&self.ty, sess)
                .or_coerce_expr_into_ty(
                    expr,
                    self.ty,
                    &mut sess.tycx,
                    sess.target_metrics.word_size,
                )
                .or_report_err(&sess.tycx, self.ty, res.ty, expr.span)?;

            res.const_value
        } else {
            None
        };

        let const_value = match &self.pattern {
            Pattern::Single(pat) => {
                // don't allow const values with mutable bindings or patterns
                // that are not `Single` and are not mutable
                if !pat.is_mutable {
                    const_value
                } else {
                    None
                }
            }
            _ => None,
        };

        sess.bind_pattern(
            env,
            &mut self.pattern,
            self.visibility,
            self.ty,
            const_value,
            self.kind,
        )?;

        if let Some(expr) = &mut self.expr {
            // If this expressions matches the entry point function's requirements
            // Tag it as the entry function
            match &self.pattern {
                Pattern::Single(pat) => {
                    if sess.workspace.entry_point_function_id.is_none()
                        && pat.symbol == "main"
                        && expr.is_fn()
                    {
                        sess.workspace.entry_point_function_id = Some(pat.binding_info_id);
                        expr.as_fn_mut().is_entry_point = true;
                    }
                }
                _ => (),
            }
        }

        Ok(Res::new_maybe_const(self.ty, const_value))
    }
}

impl Check for ast::Fn {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let sig_res = self.sig.check(sess, env, expected_ty)?;

        let fn_ty = sess.tycx.ty_kind(sig_res.ty);
        let fn_ty = fn_ty.as_fn();

        let return_ty = sess.tycx.bound(fn_ty.ret.as_ref().clone());

        env.push_named_scope(self.sig.name);

        sess.bind_symbol(
            env,
            self.sig.name,
            ast::Visibility::Private,
            sig_res.ty,
            None,
            false,
            ast::BindingKind::Value,
            self.body.span,
        )?;

        for (param, param_ty) in self.sig.params.iter_mut().zip(fn_ty.params.iter()) {
            let ty = sess.tycx.bound(param_ty.ty.clone());
            sess.bind_pattern(
                env,
                &mut param.pattern,
                ast::Visibility::Private,
                ty,
                None,
                ast::BindingKind::Value,
            )?;
        }

        let body_res = sess.with_function_frame(
            FunctionFrame {
                return_ty,
                scope_level: env.scope_level(),
            },
            |sess| self.body.check(sess, env, None),
        )?;

        let mut unify_res = body_res.ty.unify(&return_ty, sess);
        if let Some(last_expr) = self.body.exprs.last_mut() {
            unify_res = unify_res.or_coerce_expr_into_ty(
                last_expr,
                return_ty,
                &mut sess.tycx,
                sess.target_metrics.word_size,
            );
        }
        unify_res.or_report_err(&sess.tycx, return_ty, body_res.ty, self.body.span)?;

        env.pop_scope();

        Ok(Res::new(sig_res.ty))
    }
}

impl Check for ast::FnSig {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_ty: Option<Ty>,
    ) -> CheckResult {
        let mut ty_params = vec![];
        let mut param_map = UstrMap::default();

        for param in self.params.iter_mut() {
            param.ty = if let Some(expr) = &mut param.ty_expr {
                let res = expr.check(sess, env, None)?;
                sess.extract_const_type(res.const_value, res.ty, expr.span)?
            } else {
                sess.tycx.var()
            };

            ty_params.push(FnTyParam {
                symbol: if param.pattern.is_single() {
                    param.pattern.as_single_ref().symbol
                } else {
                    ustr("")
                },
                ty: param.ty.into(),
            });

            for pat in param.pattern.symbols() {
                if let Some(already_defined_span) = param_map.insert(pat.symbol, pat.span) {
                    return Err(SyntaxError::duplicate_symbol(
                        already_defined_span,
                        pat.span,
                        pat.symbol,
                    ));
                }
            }
        }

        let ret = if let Some(expr) = &mut self.ret {
            let res = expr.check(sess, env, None)?;
            sess.extract_const_type(res.const_value, res.ty, expr.span)?
        } else if self.lib_name.is_some() {
            sess.tycx.common_types.unit
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
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let res = match &mut self.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports.iter_mut() {
                    import.check(sess, env, None)?;
                }
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Foreign(bindings) => {
                for binding in bindings.iter_mut() {
                    binding.check(sess, env, None)?;
                }
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Binding(binding) => {
                binding.check(sess, env, None)?;
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Defer(expr) => {
                expr.check(sess, env, None)?;
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Assign(assign) => {
                let lres = assign.lvalue.check(sess, env, None)?;
                let rres = assign.rvalue.check(sess, env, Some(lres.ty))?;

                rres.ty
                    .unify(&lres.ty, sess)
                    .or_coerce_expr_into_ty(
                        &mut assign.rvalue,
                        lres.ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, lres.ty, rres.ty, assign.rvalue.span)?;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Cast(cast) => cast.check(sess, env, expected_ty),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    let res = expr.check(sess, env, None)?;
                    sess.extract_const_type(res.const_value, res.ty, expr.span)?;
                    Ok(Res::new(sess.tycx.common_types.uint))
                }
                ast::Builtin::Panic(expr) => {
                    if let Some(expr) = expr {
                        expr.check(sess, env, None)?;
                    }
                    Ok(Res::new(sess.tycx.common_types.unit))
                }
            },
            ast::ExprKind::Fn(f) => f.check(sess, env, expected_ty),
            ast::ExprKind::While(while_) => {
                let cond_ty = sess.tycx.common_types.bool;
                let cond_res = while_.cond.check(sess, env, Some(cond_ty))?;

                cond_res
                    .ty
                    .unify(&cond_ty, sess)
                    .or_coerce_expr_into_ty(
                        &mut while_.cond,
                        cond_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, cond_ty, cond_res.ty, while_.cond.span)?;

                sess.loop_depth += 1;
                while_.block.check(sess, env, None)?;
                sess.loop_depth -= 1;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::For(for_) => {
                let iter_ty = match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        let start_res = start.check(sess, env, None)?;
                        let end_res = end.check(sess, env, None)?;

                        let anyint = sess.tycx.anyint();

                        start_res.ty.unify(&anyint, sess).or_report_err(
                            &sess.tycx,
                            anyint,
                            start_res.ty,
                            start.span,
                        )?;

                        start_res
                            .ty
                            .unify(&end_res.ty, sess)
                            .or_coerce_exprs(
                                start,
                                end,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(&sess.tycx, start_res.ty, end_res.ty, end.span)?;

                        start_res.ty
                    }
                    ast::ForIter::Value(value) => {
                        let res = value.check(sess, env, None)?;
                        let ty = res.ty.normalize(&sess.tycx);

                        match ty.maybe_deref_once() {
                            TyKind::Array(inner, ..) | TyKind::Slice(inner, ..) => {
                                sess.tycx.bound(*inner)
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!("can't iterate over `{}`", ty))
                                    .with_labels(vec![Label::primary(
                                        value.span.file_id,
                                        value.span.range(),
                                    )]));
                            }
                        }
                    }
                };

                sess.bind_symbol(
                    env,
                    for_.iter_name,
                    ast::Visibility::Private,
                    iter_ty,
                    None,
                    false,
                    ast::BindingKind::Value,
                    self.span, // TODO: use iter's actual span
                )?;

                sess.bind_symbol(
                    env,
                    for_.iter_index_name,
                    ast::Visibility::Private,
                    sess.tycx.common_types.uint,
                    None,
                    false,
                    ast::BindingKind::Value,
                    self.span, // TODO: use iter_index's actual span
                )?;

                sess.loop_depth += 1;
                for_.block.check(sess, env, None)?;
                sess.loop_depth -= 1;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Break(e) => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "break"));
                }

                for expr in e.deferred.iter_mut() {
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Continue(e) => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "continue"));
                }

                for expr in e.deferred.iter_mut() {
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Return(ret) => {
                let function_frame = sess
                    .function_frame()
                    .ok_or(SyntaxError::outside_of_function(self.span, "return"))?;

                if let Some(expr) = &mut ret.expr {
                    let expected = function_frame.return_ty;
                    let res = expr.check(sess, env, Some(expected))?;
                    res.ty
                        .unify(&expected, sess)
                        .or_coerce_expr_into_ty(
                            expr,
                            expected,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tycx, expected, res.ty, expr.span)?;
                } else {
                    let expected = sess.tycx.common_types.unit;
                    function_frame
                        .return_ty
                        .unify(&expected, sess)
                        .or_report_err(&sess.tycx, expected, function_frame.return_ty, self.span)?;
                }

                for expr in ret.deferred.iter_mut() {
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::If(if_) => {
                let unit = sess.tycx.common_types.unit;
                let cond_ty = sess.tycx.common_types.bool;
                let cond_res = if_.cond.check(sess, env, Some(cond_ty))?;

                cond_res
                    .ty
                    .unify(&cond_ty, sess)
                    .or_coerce_expr_into_ty(
                        &mut if_.cond,
                        cond_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, cond_ty, cond_res.ty, if_.cond.span)?;

                // if the condition is compile-time known, only check the resulting branch
                if let Some(cond_value) = cond_res.const_value {
                    let res = if cond_value.into_bool() {
                        let res = if_.then.check(sess, env, expected_ty)?;
                        *self = if_.then.as_ref().clone();
                        Ok(res)
                    } else {
                        if let Some(otherwise) = &mut if_.otherwise {
                            let res = otherwise.check(sess, env, expected_ty)?;
                            *self = otherwise.as_ref().clone();
                            Ok(res)
                        } else {
                            *self = ast::Expr::typed(
                                ast::ExprKind::Literal(ast::Literal::Unit),
                                unit,
                                self.span,
                            );
                            Ok(Res::new(unit))
                        }
                    };

                    return res;
                }

                let then_res = if_.then.check(sess, env, expected_ty)?;

                if let Some(otherwise) = &mut if_.otherwise {
                    let otherwise_res = otherwise.check(sess, env, Some(then_res.ty))?;

                    otherwise_res
                        .ty
                        .unify(&then_res.ty, sess)
                        .or_coerce_exprs(
                            &mut if_.then,
                            otherwise,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tycx, then_res.ty, otherwise_res.ty, otherwise.span)?;

                    Ok(Res::new(then_res.ty))
                } else {
                    Ok(Res::new(unit))
                }

                // let then_res = if_.then.check(sess, env, expected_ty)?;

                // if let Some(otherwise) = &mut if_.otherwise {
                //     let otherwise_res = otherwise.check(sess, env, Some(then_res.ty))?;

                //     otherwise_res
                //         .ty
                //         .unify(&then_res.ty, sess)
                //         .or_coerce_exprs(
                //             &mut if_.then,
                //             otherwise,
                //             &mut sess.tycx,
                //             sess.target_metrics.word_size,
                //         )
                //         .or_report_err(&sess.tycx, then_res.ty, otherwise_res.ty, otherwise.span)?;

                //     // if the condition is a constant, we can choose the branch at compile time
                //     if let Some(cond_value) = cond_res.const_value {
                //         if cond_value.into_bool() {
                //             *self = if_.then.as_ref().clone();
                //             Ok(Res::new_maybe_const(then_res.ty, then_res.const_value))
                //         } else {
                //             *self = otherwise.as_ref().clone();
                //             Ok(Res::new_maybe_const(
                //                 otherwise_res.ty,
                //                 otherwise_res.const_value,
                //             ))
                //         }
                //     } else {
                //         Ok(Res::new(then_res.ty))
                //     }
                // } else {
                //     let unit = sess.tycx.common_types.unit;
                //     // if the condition is a constant, we can either choose the then
                //     // branch at compile time, or transform this into a unit
                //     if let Some(cond_value) = cond_res.const_value {
                //         if cond_value.into_bool() {
                //             *self = if_.then.as_ref().clone();
                //             Ok(Res::new_maybe_const(then_res.ty, then_res.const_value))
                //         } else {
                //             *self = ast::Expr::typed(
                //                 ast::ExprKind::Literal(ast::Literal::Unit),
                //                 unit,
                //                 self.span,
                //             );
                //             Ok(Res::new(unit))
                //         }
                //     } else {
                //         Ok(Res::new(unit))
                //     }
                // }
            }
            ast::ExprKind::Block(block) => block.check(sess, env, expected_ty),
            ast::ExprKind::Binary(binary) => {
                let lhs_res = binary.lhs.check(sess, env, expected_ty)?;
                let rhs_res = binary.rhs.check(sess, env, expected_ty)?;

                let expected_ty = match &binary.op {
                    ast::BinaryOp::Add
                    | ast::BinaryOp::Sub
                    | ast::BinaryOp::Mul
                    | ast::BinaryOp::Div
                    | ast::BinaryOp::Rem
                    | ast::BinaryOp::Lt
                    | ast::BinaryOp::LtEq
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::GtEq
                    | ast::BinaryOp::Shl
                    | ast::BinaryOp::Shr
                    | ast::BinaryOp::BitwiseOr
                    | ast::BinaryOp::BitwiseXor
                    | ast::BinaryOp::BitwiseAnd => sess.tycx.anyint(),
                    ast::BinaryOp::Eq | ast::BinaryOp::NEq => sess.tycx.var(),
                    ast::BinaryOp::And | ast::BinaryOp::Or => sess.tycx.common_types.bool,
                };

                lhs_res.ty.unify(&expected_ty, sess).or_report_err(
                    &sess.tycx,
                    expected_ty,
                    lhs_res.ty,
                    binary.lhs.span,
                )?;

                rhs_res
                    .ty
                    .unify(&lhs_res.ty, sess)
                    .or_coerce_exprs(
                        &mut binary.lhs,
                        &mut binary.rhs,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, lhs_res.ty, rhs_res.ty, binary.rhs.span)?;

                let result_ty = match &binary.op {
                    ast::BinaryOp::Add
                    | ast::BinaryOp::Sub
                    | ast::BinaryOp::Mul
                    | ast::BinaryOp::Div
                    | ast::BinaryOp::Rem
                    | ast::BinaryOp::Shl
                    | ast::BinaryOp::Shr
                    | ast::BinaryOp::BitwiseOr
                    | ast::BinaryOp::BitwiseXor
                    | ast::BinaryOp::BitwiseAnd => binary.lhs.ty,

                    ast::BinaryOp::Eq
                    | ast::BinaryOp::NEq
                    | ast::BinaryOp::Lt
                    | ast::BinaryOp::LtEq
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::GtEq
                    | ast::BinaryOp::And
                    | ast::BinaryOp::Or => sess.tycx.common_types.bool,
                };

                match (lhs_res.const_value, rhs_res.const_value) {
                    (Some(lhs), Some(rhs)) => {
                        let value = const_fold_binary(lhs, rhs, binary.op, self.span)?;
                        *self = value.into_literal().into_expr(result_ty, self.span);
                        Ok(Res::new_const(result_ty, value))
                    }
                    _ => Ok(Res::new(result_ty)),
                }
            }
            ast::ExprKind::Unary(unary) => {
                let res = unary.lhs.check(sess, env, None)?;

                match unary.op {
                    ast::UnaryOp::Ref(is_mutable) => {
                        let ty = sess
                            .tycx
                            .bound(TyKind::Pointer(Box::new(res.ty.into()), is_mutable));
                        Ok(Res::new(ty))
                    }
                    ast::UnaryOp::Deref => {
                        let kind = res.ty.normalize(&sess.tycx);
                        match kind {
                            // TODO: instead of checking type directly, apply `Deref` constraint
                            TyKind::Pointer(inner, _) => {
                                let ty = sess.tycx.bound(*inner);
                                Ok(Res::new(ty))
                            }
                            ty => Err(TypeError::deref_non_pointer_ty(
                                self.span,
                                ty.display(&sess.tycx),
                            )),
                        }
                    }
                    ast::UnaryOp::Not => {
                        let bool = sess.tycx.common_types.bool;

                        res.ty.unify(&bool, sess).or_report_err(
                            &sess.tycx,
                            bool,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        if let Some(value) = res.const_value {
                            let value = Value::Bool(!value.into_bool());
                            *self = value.into_literal().into_expr(res.ty, self.span);
                            Ok(Res::new_const(res.ty, value))
                        } else {
                            Ok(Res::new(res.ty))
                        }
                    }
                    ast::UnaryOp::Neg => {
                        let anyint = sess.tycx.anyint();

                        res.ty.unify(&anyint, sess).or_report_err(
                            &sess.tycx,
                            anyint,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        if let Some(value) = res.const_value {
                            let value = match value {
                                Value::Int(i) => Value::Int(-i),
                                Value::Float(f) => Value::Float(-f),
                                _ => unreachable!("got {}", value),
                            };

                            *self = value.into_literal().into_expr(res.ty, self.span);

                            Ok(Res::new_const(res.ty, value))
                        } else {
                            Ok(Res::new(res.ty))
                        }
                    }
                    ast::UnaryOp::Plus => {
                        let anyint = sess.tycx.anyint();

                        res.ty.unify(&anyint, sess).or_report_err(
                            &sess.tycx,
                            anyint,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        Ok(Res::new_maybe_const(res.ty, res.const_value))
                    }
                    ast::UnaryOp::BitwiseNot => {
                        let anyint = sess.tycx.anyint();

                        res.ty.unify(&anyint, sess).or_report_err(
                            &sess.tycx,
                            anyint,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        if let Some(value) = res.const_value {
                            let value = Value::Int(!value.into_int());
                            *self = value.into_literal().into_expr(res.ty, self.span);
                            Ok(Res::new_const(res.ty, value))
                        } else {
                            Ok(Res::new(res.ty))
                        }
                    }
                }
            }
            ast::ExprKind::Subscript(sub) => {
                let index_res = sub.index.check(sess, env, None)?;
                let uint = sess.tycx.common_types.uint;

                index_res
                    .ty
                    .unify(&uint, sess)
                    .or_coerce_expr_into_ty(
                        &mut sub.index,
                        uint,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, uint, index_res.ty, sub.index.span)?;

                let res = sub.expr.check(sess, env, None)?;
                let kind = res.ty.normalize(&sess.tycx);
                let kind_deref = kind.maybe_deref_once();

                if let Some(value) = index_res.const_value {
                    let value = value.into_int();
                    if let TyKind::Array(_, size) = kind_deref {
                        if value < 0 || value >= size as _ {
                            let msg = format!(
                                "index out of array bounds - expected 0 to {}, but found {}",
                                size - 1,
                                value
                            );
                            return Err(Diagnostic::error().with_message(msg).with_labels(vec![
                                Label::primary(sub.index.span.file_id, sub.index.span.range())
                                    .with_message("index out of bounds"),
                            ]));
                        }
                    }
                }

                match kind_deref {
                    TyKind::Array(inner, ..)
                    | TyKind::Slice(inner, ..)
                    | TyKind::MultiPointer(inner, ..) => {
                        let ty = sess.tycx.bound(*inner);
                        Ok(Res::new(ty))
                    }
                    _ => Err(TypeError::invalid_expr_in_subscript(
                        sub.expr.span,
                        kind.display(&sess.tycx),
                    )),
                }
            }
            ast::ExprKind::Slice(slice) => {
                let expr_res = slice.expr.check(sess, env, None)?;
                let expr_ty = expr_res.ty.normalize(&sess.tycx);
                let uint = sess.tycx.common_types.uint;

                if let Some(low) = &mut slice.low {
                    let res = low.check(sess, env, None)?;

                    res.ty
                        .unify(&uint, sess)
                        .or_coerce_expr_into_ty(
                            low,
                            uint,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tycx, uint, res.ty, low.span)?;
                }

                if let Some(high) = &mut slice.high {
                    let res = high.check(sess, env, None)?;

                    res.ty
                        .unify(&uint, sess)
                        .or_coerce_expr_into_ty(
                            high,
                            uint,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tycx, uint, res.ty, high.span)?;
                }

                let (result_ty, is_mutable) = match expr_ty {
                    // TODO: this is immutable even if the array is mutable
                    TyKind::Array(inner, ..) => (inner, false),
                    TyKind::Slice(inner, is_mutable) | TyKind::MultiPointer(inner, is_mutable) => {
                        (inner, is_mutable)
                    }
                    _ => {
                        return Err(TypeError::invalid_expr_in_slice(
                            slice.expr.span,
                            expr_ty.display(&sess.tycx),
                        ))
                    }
                };

                let ty = sess.tycx.bound(TyKind::Slice(result_ty, is_mutable));

                Ok(Res::new(ty))
            }
            ast::ExprKind::FnCall(call) => call.check(sess, env, expected_ty),
            ast::ExprKind::MemberAccess(access) => {
                let res = access.expr.check(sess, env, None)?;
                let kind = res.ty.normalize(&sess.tycx);

                match &kind.maybe_deref_once() {
                    ty @ TyKind::Tuple(tys) => match access.member.as_str().parse::<i32>() {
                        Ok(index) => match tys.get(index as usize) {
                            Some(field_ty) => Ok(Res::new(sess.tycx.bound(field_ty.clone()))),
                            None => Err(TypeError::tuple_field_out_of_bounds(
                                access.expr.span,
                                &access.member,
                                ty.display(&sess.tycx),
                                tys.len() - 1,
                            )),
                        },
                        Err(_) => Err(TypeError::non_numeric_tuple_field(
                            access.expr.span,
                            &access.member,
                            ty.display(&sess.tycx),
                        )),
                    },
                    ty @ TyKind::Struct(st) => {
                        match st.fields.iter().find(|f| f.symbol == access.member) {
                            Some(field) => Ok(Res::new(sess.tycx.bound(field.ty.clone()))),
                            None => Err(TypeError::invalid_struct_field(
                                access.expr.span,
                                access.member,
                                ty.display(&sess.tycx),
                            )),
                        }
                    }
                    TyKind::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => Ok(
                        Res::new_const(sess.tycx.common_types.uint, Value::Int(*size as _)),
                    ),
                    TyKind::Slice(..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        Ok(Res::new(sess.tycx.common_types.uint))
                    }
                    TyKind::Slice(inner, is_mutable)
                        if access.member.as_str() == BUILTIN_FIELD_DATA =>
                    {
                        Ok(Res::new(
                            sess.tycx
                                .bound(TyKind::MultiPointer(inner.clone(), *is_mutable)),
                        ))
                    }
                    TyKind::Module(module_id) => {
                        let (res, _) = sess.check_top_level_symbol(
                            CallerInfo {
                                module_id: env.module_id(),
                                span: self.span,
                            },
                            *module_id,
                            access.member,
                        )?;
                        Ok(res)
                    }
                    ty => Err(TypeError::member_access_on_invalid_type(
                        access.expr.span,
                        ty.display(&sess.tycx),
                    )),
                }
            }
            ast::ExprKind::Ident(ident) => match sess.get_symbol(env, ident.symbol) {
                Some(id) => {
                    // this is a local binding
                    ident.binding_info_id = id;
                    sess.workspace.increment_binding_use(id);

                    let binding_info = sess.workspace.get_binding_info(id).unwrap();

                    let min_scope_level = sess
                        .function_frame()
                        .map_or(ScopeLevel::Global, |f| f.scope_level);

                    if !binding_info.kind.is_type()
                        && !binding_info.scope_level.is_global()
                        && binding_info.scope_level < min_scope_level
                    {
                        return Err(Diagnostic::error()
                            .with_message("can't capture dynamic environment yet - closures are not implemented yet")
                            .with_labels(vec![Label::primary(
                                self.span.file_id,
                                self.span.range(),
                            )]));
                    }

                    Ok(sess
                        .workspace
                        .get_binding_info(id)
                        .map(|binding_info| {
                            Res::new_maybe_const(binding_info.ty, binding_info.const_value)
                        })
                        .unwrap())
                }
                None => {
                    // this is either a top level binding, a builtin binding, or it doesn't exist
                    let (res, id) = sess.check_top_level_symbol(
                        CallerInfo {
                            module_id: env.module_id(),
                            span: self.span,
                        },
                        env.module_id(),
                        ident.symbol,
                    )?;

                    ident.binding_info_id = id;
                    sess.workspace.increment_binding_use(id);

                    Ok(res)
                }
            },
            ast::ExprKind::ArrayLiteral(kind) => match kind {
                ast::ArrayLiteralKind::List(elements) => {
                    let element_ty = sess.tycx.var();

                    for el in elements.iter_mut() {
                        let res = el.check(sess, env, Some(element_ty))?;
                        res.ty
                            .unify(&element_ty, sess)
                            .or_coerce_expr_into_ty(
                                el,
                                element_ty,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(&sess.tycx, element_ty, res.ty, el.span)?;
                    }

                    let ty = sess
                        .tycx
                        .bound(TyKind::Array(Box::new(element_ty.into()), elements.len()));

                    Ok(Res::new(ty))
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    let len_res = len.check(sess, env, None)?;
                    let len_value =
                        sess.extract_const_int(len_res.const_value, len_res.ty, len.span)?;

                    if len_value < 0 {
                        return Err(TypeError::negative_array_len(len.span, len_value));
                    }

                    let expr = expr.check(sess, env, None)?;

                    let ty = sess
                        .tycx
                        .bound(TyKind::Array(Box::new(expr.ty.into()), len_value as _));

                    Ok(Res::new(ty))
                }
            },
            ast::ExprKind::TupleLiteral(elements) => {
                let mut elements_res = vec![];

                for el in elements.iter_mut() {
                    let res = el.check(sess, env, None)?;
                    elements_res.push(res);
                }

                let is_tuple_type = elements_res
                    .iter()
                    .all(|res| res.const_value.as_ref().map_or(false, |v| v.is_type()));

                let element_tys: Vec<TyKind> = elements.iter().map(|e| TyKind::Var(e.ty)).collect();
                let kind = TyKind::Tuple(element_tys);
                let ty = sess.tycx.bound(kind.clone());

                if is_tuple_type {
                    Ok(Res::new_const(
                        sess.tycx.bound(kind.create_type()),
                        Value::Type(ty),
                    ))
                } else {
                    Ok(Res::new(ty))
                }
            }
            ast::ExprKind::StructLiteral(lit) => match &mut lit.type_expr {
                Some(type_expr) => {
                    let res = type_expr.check(sess, env, None)?;
                    let ty = sess.extract_const_type(res.const_value, res.ty, type_expr.span)?;

                    let kind = ty.normalize(&sess.tycx);

                    match kind {
                        TyKind::Struct(struct_ty) => check_named_struct_literal(
                            sess,
                            env,
                            struct_ty,
                            &mut lit.fields,
                            self.span,
                        ),
                        _ => Err(Diagnostic::error()
                            .with_message(format!(
                                "type `{}` does not support struct initialization syntax",
                                ty
                            ))
                            .with_labels(vec![Label::primary(
                                type_expr.span.file_id,
                                type_expr.span.range(),
                            )])),
                    }
                }
                None => match expected_ty {
                    Some(ty) => {
                        let kind = ty.normalize(&sess.tycx);
                        match kind {
                            TyKind::Struct(struct_ty) => check_named_struct_literal(
                                sess,
                                env,
                                struct_ty,
                                &mut lit.fields,
                                self.span,
                            ),
                            _ => check_anonymous_struct_literal(
                                sess,
                                env,
                                &mut lit.fields,
                                self.span,
                            ),
                        }
                    }
                    None => check_anonymous_struct_literal(sess, env, &mut lit.fields, self.span),
                },
            },
            ast::ExprKind::Literal(lit) => lit.check(sess, env, expected_ty),
            ast::ExprKind::PointerType(inner, is_mutable) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Pointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::MultiPointerType(inner, is_mutable) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::MultiPointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind)),
                ))
            }
            ast::ExprKind::ArrayType(inner, size) => {
                let inner_res = inner.check(sess, env, None)?;
                let inner_ty =
                    sess.extract_const_type(inner_res.const_value, inner_res.ty, inner.span)?;

                let size_res = size.check(sess, env, None)?;

                let size_value =
                    sess.extract_const_int(size_res.const_value, size_res.ty, size.span)?;

                if size_value < 0 {
                    return Err(TypeError::negative_array_len(size.span, size_value));
                }

                let kind = TyKind::Array(Box::new(inner_ty.into()), size_value as usize);

                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind)),
                ))
            }
            ast::ExprKind::SliceType(inner, is_mutable) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Slice(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind)),
                ))
            }
            ast::ExprKind::StructType(st) => {
                let is_anonymous = st.name.is_empty();

                st.name = if is_anonymous {
                    get_anonymous_struct_name(self.span)
                } else {
                    st.name
                };

                let inferred_ty = sess.tycx.var();

                let opaque_struct =
                    TyKind::Struct(StructTy::opaque(st.name, st.binding_info_id, st.kind))
                        .create_type();

                inferred_ty.unify(&opaque_struct, sess).unwrap();

                sess.self_types.push(inferred_ty);

                let mut field_map = UstrMap::<Span>::default();
                let mut struct_ty_fields = vec![];

                for field in st.fields.iter_mut() {
                    let res = field.ty.check(sess, env, None)?;
                    let ty = sess.extract_const_type(res.const_value, res.ty, field.span)?;

                    if let Some(defined_span) = field_map.insert(field.name, field.span) {
                        return Err(SyntaxError::duplicate_struct_field(
                            defined_span,
                            field.span,
                            field.name.to_string(),
                        ));
                    }

                    struct_ty_fields.push(StructTyField {
                        symbol: field.name,
                        ty: ty.into(),
                        span: field.span,
                    });
                }

                let struct_ty = TyKind::Struct(StructTy {
                    name: st.name,
                    binding_info_id: st.binding_info_id,
                    kind: st.kind,
                    fields: struct_ty_fields,
                });

                sess.self_types.pop();

                Ok(Res::new_const(
                    sess.tycx.bound(struct_ty.clone().create_type()),
                    Value::Type(sess.tycx.bound(struct_ty)),
                ))
            }
            ast::ExprKind::FnType(sig) => sig.check(sess, env, expected_ty),
            ast::ExprKind::SelfType => match sess.self_types.last() {
                Some(&ty) => Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                )),
                None => Err(Diagnostic::error()
                    .with_message("`Self` is only available within struct types")
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
        }?;

        self.ty = res.ty;

        Ok(res)
    }
}

impl Check for ast::FnCall {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_ty: Option<Ty>,
    ) -> CheckResult {
        let callee_res = self.callee.check(sess, env, None)?;

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
                            let res = arg.expr.check(sess, env, Some(param_ty))?;

                            res.ty
                                .unify(&param_ty, sess)
                                .or_coerce_expr_into_ty(
                                    &mut arg.expr,
                                    param_ty,
                                    &mut sess.tycx,
                                    sess.target_metrics.word_size,
                                )
                                .or_report_err(&sess.tycx, param_ty, res.ty, arg.expr.span)?;
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
                            let res = arg.expr.check(sess, env, Some(param_ty))?;

                            res.ty
                                .unify(&param_ty, sess)
                                .or_coerce_expr_into_ty(
                                    &mut arg.expr,
                                    param_ty,
                                    &mut sess.tycx,
                                    sess.target_metrics.word_size,
                                )
                                .or_report_err(&sess.tycx, param_ty, res.ty, arg.expr.span)?;
                        } else {
                            // this is a variadic argument, meaning that the argument's
                            // index is greater than the function's param length
                            arg.expr.check(sess, env, None)?;
                        }
                    };
                }

                Ok(Res::new(sess.tycx.bound(fn_ty.ret.as_ref().clone())))
            }
            ty => {
                for arg in self.args.iter_mut() {
                    arg.expr.check(sess, env, None)?;
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

                ty.unify(&inferred_fn_ty, sess).or_report_err(
                    &sess.tycx,
                    inferred_fn_ty,
                    ty,
                    self.callee.span,
                )?;

                Ok(Res::new(return_ty))
            }
        }
    }
}

impl Check for ast::Cast {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let res = self.expr.check(sess, env, None)?;

        self.target_ty = if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, env, None)?;
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
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let mut res = Res::new(sess.tycx.common_types.unit);

        env.push_scope();

        if !self.exprs.is_empty() {
            let last_index = self.exprs.len() - 1;

            for (index, expr) in self.exprs.iter_mut().enumerate() {
                res = expr.check(
                    sess,
                    env,
                    if index == last_index {
                        expected_ty
                    } else {
                        None
                    },
                )?;
            }
        }

        for expr in self.deferred.iter_mut() {
            expr.check(sess, env, None)?;
        }

        env.pop_scope();

        Ok(res)
    }
}

impl Check for ast::Literal {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        _env: &mut Env,
        _expected_ty: Option<Ty>,
    ) -> CheckResult {
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

#[inline]
fn check_named_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    struct_ty: StructTy,
    fields: &mut Vec<ast::StructLiteralField>,
    span: Span,
) -> CheckResult {
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
            Some(ty_field) => {
                uninit_fields.remove(&field.symbol);

                let expected_ty = sess.tycx.bound(ty_field.ty.clone());
                let field_res = field.value.check(sess, env, Some(expected_ty))?;

                field_res
                    .ty
                    .unify(&expected_ty, sess)
                    .or_coerce_expr_into_ty(
                        &mut field.value,
                        expected_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, expected_ty, field_res.ty, field.value.span)?;
            }
            None => {
                return Err(TypeError::invalid_struct_field(
                    field.span,
                    field.symbol,
                    TyKind::Struct(struct_ty).display(&sess.tycx),
                ));
            }
        }
    }

    if struct_ty.is_union() && fields.len() != 1 {
        return Err(Diagnostic::error()
            .with_message("union literal should have exactly one field")
            .with_labels(vec![Label::primary(span.file_id, span.range())]));
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
            .with_labels(vec![Label::primary(span.file_id, span.range())]));
    }

    Ok(Res::new(sess.tycx.bound(TyKind::Struct(struct_ty))))
}

#[inline]
fn check_anonymous_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    fields: &mut Vec<ast::StructLiteralField>,
    span: Span,
) -> CheckResult {
    let mut field_set = UstrSet::default();

    let name = get_anonymous_struct_name(span);
    let mut struct_ty = StructTy {
        name,
        binding_info_id: BindingInfoId::unknown(),
        kind: StructTyKind::Struct,
        fields: vec![],
    };

    for field in fields {
        if !field_set.insert(field.symbol) {
            return Err(SyntaxError::struct_field_specified_more_than_once(
                field.span,
                field.symbol.to_string(),
            ));
        }

        let res = field.value.check(sess, env, None)?;

        struct_ty.fields.push(StructTyField {
            symbol: field.symbol,
            ty: res.ty.into(),
            span: field.span,
        });
    }

    Ok(Res::new(sess.tycx.bound(TyKind::Struct(struct_ty))))
}

fn get_anonymous_struct_name(span: Span) -> Ustr {
    ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
}
