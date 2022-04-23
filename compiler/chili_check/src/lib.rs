mod bind;
mod builtin;
mod const_fold;
mod env;
mod top_level;

use chili_ast::{
    ast::{self, Expr, ForeignLibrary},
    pattern::{Pattern, SymbolPattern},
    ty::{FnTy, InferTy, PartialStructTy, StructTy, StructTyField, StructTyKind, Ty, TyKind},
    value::ConstValue,
    workspace::{
        BindingInfoFlags, BindingInfoId, ModuleId, PartialBindingInfo, ScopeLevel, Workspace,
    },
};
use chili_error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult, SyntaxError, TypeError,
};
use chili_infer::{
    cast::CanCast,
    coerce::{OrCoerceExprIntoTy, OrCoerceExprs},
    display::{DisplayTy, OrReportErr},
    normalize::NormalizeTy,
    substitute::substitute,
    ty_ctx::TyCtx,
    unify::UnifyTy,
};
use chili_interp::interp::{Interp, InterpResult};
use chili_span::Span;
use common::{
    builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    target::TargetMetrics,
};
use const_fold::binary::const_fold_binary;
use env::{Env, Scope};
use std::{
    collections::{BTreeMap, HashMap},
    iter::repeat_with,
};
use top_level::{CallerInfo, CheckTopLevel};
use ustr::{ustr, Ustr, UstrMap, UstrSet};

pub fn check(
    workspace: &mut Workspace,
    ast: Vec<ast::Ast>,
) -> DiagnosticResult<(ast::TypedAst, TyCtx)> {
    let mut sess = CheckSess::new(workspace, &ast);
    sess.start()?;
    substitute(
        &mut sess.workspace.diagnostics,
        &mut sess.tycx,
        &sess.new_ast,
    );
    Ok((sess.new_ast, sess.tycx))
}

pub(crate) struct CheckSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) target_metrics: TargetMetrics,

    pub(crate) interp: Interp,

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
    return_ty_span: Span,
    scope_level: ScopeLevel,
}

impl<'s> CheckSess<'s> {
    pub(crate) fn new(workspace: &'s mut Workspace, old_asts: &'s Vec<ast::Ast>) -> Self {
        let target_metrics = workspace.build_options.target_platform.metrics();
        Self {
            workspace,
            target_metrics,
            interp: Interp::new(),
            tycx: TyCtx::default(),
            old_asts,
            new_ast: ast::TypedAst::default(),
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

            for expr in ast.run_exprs.iter() {
                let mut expr = expr.clone();
                self.with_env(module_id, |sess, mut env| expr.check(sess, &mut env, None))?;
                interp_expr(&expr, self).unwrap();
            }

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
                            .with_label(Label::primary(span, "not supported yet")));
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
                .with_note("define function `let main = fn() {}` in your entry file"));
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
        const_value: Option<ConstValue>,
        ty: Ty,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        match const_value {
            Some(v) => {
                if let ConstValue::Type(t) = v {
                    return Ok(t);
                }
            }
            None => (),
        }

        Err(TypeError::expected(span, ty.display(&self.tycx), "a type"))
    }

    pub(crate) fn extract_const_int(
        &mut self,
        value: Option<ConstValue>,
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

            let id = sess.workspace.add_binding_info(PartialBindingInfo {
                module_id: Default::default(),
                symbol,
                visibility: ast::Visibility::Public,
                ty: sess.tycx.bound_builtin(ty.kind().create_type()),
                const_value: Some(ConstValue::Type(ty)),
                is_mutable: false,
                kind: ast::BindingKind::Type,
                scope_level: ScopeLevel::Global,
                scope_name: ustr(""),
                span: Span::unknown(),
            });

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
    const_value: Option<ConstValue>,
}

impl Res {
    pub(crate) fn new(ty: Ty) -> Self {
        Self {
            ty,
            const_value: None,
        }
    }

    pub(crate) fn new_maybe_const(ty: Ty, const_value: Option<ConstValue>) -> Self {
        Self { ty, const_value }
    }

    pub(crate) fn new_const(ty: Ty, const_value: ConstValue) -> Self {
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

        let mut ty = sess
            .tycx
            .bound(TyKind::Module(self.target_module_id), self.span);
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

        self.target_binding_info_id = target_binding_info;

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
            sess.tycx.var(self.pattern.span())
        };

        let const_value = if let Some(expr) = &mut self.expr {
            let res = expr.check(sess, env, Some(self.ty))?;

            res.ty
                .unify(&self.ty, &mut sess.tycx)
                .or_coerce_expr_into_ty(
                    expr,
                    self.ty,
                    &mut sess.tycx,
                    sess.target_metrics.word_size,
                )
                .or_report_err(
                    &sess.tycx,
                    self.ty,
                    self.ty_expr.as_ref().map(|e| e.span),
                    res.ty,
                    expr.span,
                )?;

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

        if self.expr.as_ref().map_or(false, |e| e.is_fn()) {
            // because functions can be recursive,
            // we special case them and use the id bounded after checking the function signature
            let fn_expr = self.expr.as_mut().unwrap().as_fn_mut();

            let binding_info = sess
                .workspace
                .get_binding_info_mut(fn_expr.binding_info_id.unwrap())
                .unwrap();

            binding_info.visibility = self.visibility;

            // If this binding matches the entry point function's requirements
            // Tag it as the entry function
            match &mut self.pattern {
                Pattern::Single(pat) => {
                    pat.binding_info_id = fn_expr.binding_info_id.unwrap();
                    if sess.workspace.entry_point_function_id.is_none() && pat.symbol == "main" {
                        sess.workspace.entry_point_function_id = Some(pat.binding_info_id);
                        fn_expr.is_entry_point = true;
                    }
                }
                _ => (),
            }
        } else {
            sess.bind_pattern(
                env,
                &mut self.pattern,
                self.visibility,
                self.ty,
                const_value,
                self.kind,
                self.expr
                    .as_ref()
                    .map(|e| e.span)
                    .or_else(|| self.ty_expr.as_ref().map(|e| e.span))
                    .unwrap_or(self.span),
            )?;
        }

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
        let fn_ty = sig_res.ty.normalize(&sess.tycx).into_fn();

        let return_ty = sess.tycx.bound(
            fn_ty.ret.as_ref().clone(),
            self.sig.ret.as_ref().map_or(self.sig.span, |e| e.span),
        );
        let return_ty_span = self.sig.ret.as_ref().map_or(self.sig.span, |e| e.span);

        if !self.sig.name.is_empty() {
            self.binding_info_id = Some(sess.bind_symbol(
                env,
                self.sig.name,
                ast::Visibility::Private,
                sig_res.ty,
                None,
                false,
                ast::BindingKind::Value,
                self.body.span,
            )?);
        }

        env.push_scope();

        for (param, param_ty) in self.sig.params.iter_mut().zip(fn_ty.params.iter()) {
            let ty = sess.tycx.bound(
                param_ty.clone(),
                param
                    .ty_expr
                    .as_ref()
                    .map_or(param.pattern.span(), |e| e.span),
            );

            let span = param.pattern.span();
            sess.bind_pattern(
                env,
                &mut param.pattern,
                ast::Visibility::Private,
                ty,
                None,
                ast::BindingKind::Value,
                span,
            )?;
        }

        let body_res = sess.with_function_frame(
            FunctionFrame {
                return_ty,
                return_ty_span,
                scope_level: env.scope_level(),
            },
            |sess| self.body.check(sess, env, None),
        )?;

        let mut unify_res = body_res.ty.unify(&return_ty, &mut sess.tycx);
        if let Some(last_expr) = self.body.exprs.last_mut() {
            unify_res = unify_res.or_coerce_expr_into_ty(
                last_expr,
                return_ty,
                &mut sess.tycx,
                sess.target_metrics.word_size,
            );
        }
        unify_res.or_report_err(
            &sess.tycx,
            return_ty,
            Some(return_ty_span),
            body_res.ty,
            self.body.span,
        )?;

        env.pop_scope();

        Ok(Res::new(sig_res.ty))
    }
}

impl Check for ast::FnSig {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let mut ty_params = vec![];
        let mut param_map = UstrMap::default();

        if !self.params.is_empty() {
            for param in self.params.iter_mut() {
                param.ty = if let Some(expr) = &mut param.ty_expr {
                    let res = expr.check(sess, env, None)?;
                    sess.extract_const_type(res.const_value, res.ty, expr.span)?
                } else {
                    sess.tycx.var(param.pattern.span())
                };

                ty_params.push(param.ty.into());

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
        } else {
            // if the function signature has no parameters, and the
            // parent type is a function with 1 parameter, add an implicit `it` parameter
            if let Some(ty) = expected_ty {
                match ty.normalize(&sess.tycx) {
                    TyKind::Fn(f) => {
                        if f.params.len() == 1 {
                            let symbol = ustr("it");

                            self.params.push(ast::FnParam {
                                pattern: Pattern::Single(SymbolPattern {
                                    binding_info_id: Default::default(),
                                    symbol,
                                    alias: None,
                                    span: Span::unknown(),
                                    is_mutable: false,
                                    ignore: false,
                                }),
                                ty_expr: None,
                                ty: sess.tycx.bound(f.params[0].clone(), self.span),
                            });

                            ty_params.push(f.params[0].clone());
                        }
                    }
                    _ => (),
                }
            }
        }

        let ret = if let Some(expr) = &mut self.ret {
            let res = expr.check(sess, env, None)?;
            sess.extract_const_type(res.const_value, res.ty, expr.span)?
        } else if self.lib_name.is_some() {
            sess.tycx.common_types.unit
        } else {
            sess.tycx.var(self.span)
        };

        self.ty = sess.tycx.bound(
            TyKind::Fn(FnTy {
                params: ty_params,
                ret: Box::new(ret.into()),
                variadic: self.variadic,
                lib_name: self.lib_name,
            }),
            self.span,
        );

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
            ast::ExprKind::Defer(defer) => {
                defer.expr.check(sess, env, None)?;
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Assign(assign) => {
                let lres = assign.lvalue.check(sess, env, None)?;
                let rres = assign.rvalue.check(sess, env, Some(lres.ty))?;

                rres.ty
                    .unify(&lres.ty, &mut sess.tycx)
                    .or_coerce_expr_into_ty(
                        &mut assign.rvalue,
                        lres.ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tycx,
                        lres.ty,
                        Some(assign.lvalue.span),
                        rres.ty,
                        assign.rvalue.span,
                    )?;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Cast(cast) => cast.check(sess, env, expected_ty),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    let res = expr.check(sess, env, None)?;
                    sess.extract_const_type(res.const_value, res.ty, expr.span)?;
                    Ok(Res::new(sess.tycx.common_types.uint))
                }
                ast::Builtin::Run(_) => {
                    todo!("interpret expression, return a constant");
                    // expr.check(sess, env, None)
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
                    .unify(&cond_ty, &mut sess.tycx)
                    .or_coerce_expr_into_ty(
                        &mut while_.cond,
                        cond_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, cond_ty, None, cond_res.ty, while_.cond.span)?;

                sess.loop_depth += 1;
                while_.block.check(sess, env, None)?;
                sess.loop_depth -= 1;

                if let Some(cond) = cond_res.const_value {
                    if cond.into_bool() {
                        Ok(Res::new(sess.tycx.common_types.never))
                    } else {
                        Ok(Res::new(sess.tycx.common_types.unit))
                    }
                } else {
                    Ok(Res::new(sess.tycx.common_types.unit))
                }
            }
            ast::ExprKind::For(for_) => {
                let iter_ty = match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        let start_res = start.check(sess, env, None)?;
                        let end_res = end.check(sess, env, None)?;

                        let anyint = sess.tycx.anyint(start.span);

                        start_res.ty.unify(&anyint, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            anyint,
                            None,
                            start_res.ty,
                            start.span,
                        )?;

                        start_res
                            .ty
                            .unify(&end_res.ty, &mut sess.tycx)
                            .or_coerce_exprs(
                                start,
                                end,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(
                                &sess.tycx,
                                start_res.ty,
                                Some(start.span),
                                end_res.ty,
                                end.span,
                            )?;

                        start_res.ty
                    }
                    ast::ForIter::Value(value) => {
                        let res = value.check(sess, env, None)?;
                        let ty = res.ty.normalize(&sess.tycx);

                        match ty.maybe_deref_once() {
                            TyKind::Array(inner, ..) | TyKind::Slice(inner, ..) => {
                                sess.tycx.bound(*inner, value.span)
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!("can't iterate over `{}`", ty))
                                    .with_label(Label::primary(value.span, "can't iterate")));
                            }
                        }
                    }
                };

                env.push_scope();

                for_.iter_id = sess.bind_symbol(
                    env,
                    for_.iter_name,
                    ast::Visibility::Private,
                    iter_ty,
                    None,
                    false,
                    ast::BindingKind::Value,
                    self.span, // TODO: use iter's actual span
                )?;

                for_.iter_index_id = sess.bind_symbol(
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

                env.pop_scope();

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Break(term) => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "break"));
                }

                for expr in term.deferred.iter_mut() {
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Continue(term) => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "continue"));
                }

                for expr in term.deferred.iter_mut() {
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
                        .unify(&expected, &mut sess.tycx)
                        .or_coerce_expr_into_ty(
                            expr,
                            expected,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tycx,
                            expected,
                            Some(function_frame.return_ty_span),
                            res.ty,
                            expr.span,
                        )?;
                } else {
                    let expected = sess.tycx.common_types.unit;
                    function_frame
                        .return_ty
                        .unify(&expected, &mut sess.tycx)
                        .or_report_err(
                            &sess.tycx,
                            expected,
                            None,
                            function_frame.return_ty,
                            self.span,
                        )?;
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
                    .unify(&cond_ty, &mut sess.tycx)
                    .or_coerce_expr_into_ty(
                        &mut if_.cond,
                        cond_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, cond_ty, None, cond_res.ty, if_.cond.span)?;

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
                                ast::ExprKind::Literal(ast::Literal {
                                    kind: ast::LiteralKind::Unit,
                                    span: self.span,
                                }),
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
                        .unify(&then_res.ty, &mut sess.tycx)
                        .or_coerce_exprs(
                            &mut if_.then,
                            otherwise,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tycx,
                            then_res.ty,
                            Some(if_.then.span),
                            otherwise_res.ty,
                            otherwise.span,
                        )?;

                    Ok(Res::new(then_res.ty))
                } else {
                    Ok(Res::new(unit))
                }
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
                    | ast::BinaryOp::BitwiseAnd => sess.tycx.anyint(binary.span),
                    ast::BinaryOp::Eq | ast::BinaryOp::Neq => sess.tycx.var(binary.span),
                    ast::BinaryOp::And | ast::BinaryOp::Or => sess.tycx.common_types.bool,
                };

                lhs_res
                    .ty
                    .unify(&expected_ty, &mut sess.tycx)
                    .or_report_err(&sess.tycx, expected_ty, None, lhs_res.ty, binary.lhs.span)?;

                rhs_res
                    .ty
                    .unify(&lhs_res.ty, &mut sess.tycx)
                    .or_coerce_exprs(
                        &mut binary.lhs,
                        &mut binary.rhs,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, lhs_res.ty, None, rhs_res.ty, binary.rhs.span)?;

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
                    | ast::BinaryOp::Neq
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
                        let ty = sess.tycx.bound(
                            TyKind::Pointer(Box::new(res.ty.into()), is_mutable),
                            unary.span,
                        );
                        Ok(Res::new(ty))
                    }
                    ast::UnaryOp::Deref => {
                        let pointee = sess.tycx.var(unary.span);

                        let ptr_ty = sess
                            .tycx
                            .bound(TyKind::Pointer(Box::new(pointee.into()), false), unary.span);

                        res.ty.unify(&ptr_ty, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            ptr_ty,
                            None,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        Ok(Res::new(pointee))
                    }
                    ast::UnaryOp::Not => {
                        let anyint = sess.tycx.anyint(unary.span);
                        let bool = sess.tycx.common_types.bool;

                        res.ty
                            .unify(&bool, &mut sess.tycx)
                            .or(res.ty.unify(&anyint, &mut sess.tycx))
                            .or_report_err(&sess.tycx, bool, None, res.ty, unary.lhs.span)?;

                        if let Some(value) = res.const_value {
                            let value = match value {
                                ConstValue::Bool(v) => ConstValue::Bool(!v),
                                ConstValue::Int(v) => ConstValue::Int(!v),
                                _ => panic!(),
                            };
                            *self = value.into_literal().into_expr(res.ty, self.span);
                            Ok(Res::new_const(res.ty, value))
                        } else {
                            Ok(Res::new(res.ty))
                        }
                    }
                    ast::UnaryOp::Neg => {
                        let anyint = sess.tycx.anyint(unary.span);

                        res.ty.unify(&anyint, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            anyint,
                            None,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        if let Some(value) = res.const_value {
                            let value = match value {
                                ConstValue::Int(i) => ConstValue::Int(-i),
                                ConstValue::Float(f) => ConstValue::Float(-f),
                                _ => unreachable!("got {}", value),
                            };

                            *self = value.into_literal().into_expr(res.ty, self.span);

                            Ok(Res::new_const(res.ty, value))
                        } else {
                            Ok(Res::new(res.ty))
                        }
                    }
                    ast::UnaryOp::Plus => {
                        let anyint = sess.tycx.anyint(unary.span);

                        res.ty.unify(&anyint, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            anyint,
                            None,
                            res.ty,
                            unary.lhs.span,
                        )?;

                        Ok(Res::new_maybe_const(res.ty, res.const_value))
                    }
                }
            }
            ast::ExprKind::Subscript(sub) => {
                let index_res = sub.index.check(sess, env, None)?;
                let uint = sess.tycx.common_types.uint;

                index_res
                    .ty
                    .unify(&uint, &mut sess.tycx)
                    .or_coerce_expr_into_ty(
                        &mut sub.index,
                        uint,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, uint, None, index_res.ty, sub.index.span)?;

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
                            return Err(Diagnostic::error().with_message(msg).with_label(
                                Label::primary(sub.index.span, "index out of bounds"),
                            ));
                        }
                    }
                }

                match kind_deref {
                    TyKind::Array(inner, ..)
                    | TyKind::Slice(inner, ..)
                    | TyKind::MultiPointer(inner, ..) => {
                        let ty = sess.tycx.bound(*inner, self.span);
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
                        .unify(&uint, &mut sess.tycx)
                        .or_coerce_expr_into_ty(
                            low,
                            uint,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tycx, uint, None, res.ty, low.span)?;
                }

                if let Some(high) = &mut slice.high {
                    let res = high.check(sess, env, None)?;

                    res.ty
                        .unify(&uint, &mut sess.tycx)
                        .or_coerce_expr_into_ty(
                            high,
                            uint,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tycx,
                            uint,
                            slice.low.as_ref().map(|e| e.span),
                            res.ty,
                            high.span,
                        )?;
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

                let ty = sess
                    .tycx
                    .bound(TyKind::Slice(result_ty, is_mutable), self.span);

                Ok(Res::new(ty))
            }
            ast::ExprKind::Call(call) => call.check(sess, env, expected_ty),
            ast::ExprKind::MemberAccess(access) => {
                let res = access.expr.check(sess, env, None)?;

                // if this parsing operation succeeds, this is a tuple member access - `tup.0`
                // otherwise, this is a struct field access - `strct.field`
                let member_tuple_index = access.member.as_str().parse::<usize>();

                let member_ty = sess.tycx.var(self.span);
                let partial_ty = match member_tuple_index {
                    Ok(index) => {
                        let elements = repeat_with(|| sess.tycx.var(self.span))
                            .take(index + 1)
                            .map(TyKind::Var)
                            .collect::<Vec<TyKind>>();
                        sess.tycx.partial_tuple(elements, self.span)
                    }
                    Err(_) => {
                        let fields = BTreeMap::from([(access.member, TyKind::Var(member_ty))]);
                        sess.tycx.partial_struct(PartialStructTy(fields), self.span)
                    }
                };

                res.ty
                    .unify(&partial_ty, &mut sess.tycx)
                    .or_report_err(&sess.tycx, partial_ty, None, res.ty, self.span)?;

                let kind = res.ty.normalize(&sess.tycx);

                match &kind.maybe_deref_once() {
                    ty @ TyKind::Tuple(elements)
                    | ty @ TyKind::Infer(_, InferTy::PartialTuple(elements)) => {
                        match member_tuple_index {
                            Ok(index) => match elements.get(index) {
                                Some(field_ty) => {
                                    Ok(Res::new(sess.tycx.bound(field_ty.clone(), self.span)))
                                }
                                None => Err(TypeError::tuple_field_out_of_bounds(
                                    access.expr.span,
                                    &access.member,
                                    ty.display(&sess.tycx),
                                    elements.len() - 1,
                                )),
                            },
                            Err(_) => Err(TypeError::non_numeric_tuple_field(
                                access.expr.span,
                                &access.member,
                                ty.display(&sess.tycx),
                            )),
                        }
                    }
                    ty @ TyKind::Struct(st) => {
                        match st.fields.iter().find(|f| f.symbol == access.member) {
                            Some(field) => {
                                Ok(Res::new(sess.tycx.bound(field.ty.clone(), self.span)))
                            }
                            None => Err(TypeError::invalid_struct_field(
                                access.expr.span,
                                access.member,
                                ty.display(&sess.tycx),
                            )),
                        }
                    }
                    ty @ TyKind::Infer(_, InferTy::PartialStruct(partial)) => {
                        match partial.get(&access.member) {
                            Some(ty) => Ok(Res::new(sess.tycx.bound(ty.clone(), self.span))),
                            None => Err(TypeError::invalid_struct_field(
                                access.expr.span,
                                access.member,
                                ty.display(&sess.tycx),
                            )),
                        }
                    }
                    TyKind::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => Ok(
                        Res::new_const(sess.tycx.common_types.uint, ConstValue::Int(*size as _)),
                    ),
                    TyKind::Slice(..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        Ok(Res::new(sess.tycx.common_types.uint))
                    }
                    TyKind::Slice(inner, is_mutable)
                        if access.member.as_str() == BUILTIN_FIELD_DATA =>
                    {
                        Ok(Res::new(sess.tycx.bound(
                            TyKind::MultiPointer(inner.clone(), *is_mutable),
                            self.span,
                        )))
                    }
                    TyKind::Module(module_id) => {
                        let (res, id) = sess.check_top_level_symbol(
                            CallerInfo {
                                module_id: env.module_id(),
                                span: self.span,
                            },
                            *module_id,
                            access.member,
                        )?;
                        sess.workspace.increment_binding_use(id);
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
                            .with_message(
                                "can't capture environment - closures are not implemented yet",
                            )
                            .with_label(Label::primary(self.span, "can't capture")));
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
            ast::ExprKind::ArrayLiteral(lit) => match &mut lit.kind {
                ast::ArrayLiteralKind::List(elements) => {
                    let element_ty_span = elements.first().map_or(self.span, |e| e.span);
                    let element_ty = sess.tycx.var(element_ty_span);

                    for el in elements.iter_mut() {
                        let res = el.check(sess, env, Some(element_ty))?;
                        res.ty
                            .unify(&element_ty, &mut sess.tycx)
                            .or_coerce_expr_into_ty(
                                el,
                                element_ty,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(
                                &sess.tycx,
                                element_ty,
                                Some(element_ty_span),
                                res.ty,
                                el.span,
                            )?;
                    }

                    let ty = sess.tycx.bound(
                        TyKind::Array(Box::new(element_ty.into()), elements.len()),
                        self.span,
                    );

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

                    let ty = sess.tycx.bound(
                        TyKind::Array(Box::new(expr.ty.into()), len_value as _),
                        self.span,
                    );

                    Ok(Res::new(ty))
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                let mut elements_res = vec![];

                for el in lit.elements.iter_mut() {
                    let res = el.check(sess, env, None)?;
                    elements_res.push(res);
                }

                let is_tuple_type = elements_res
                    .iter()
                    .all(|res| res.const_value.as_ref().map_or(false, |v| v.is_type()));

                let element_tys: Vec<TyKind> = lit.elements.iter().map(|e| e.ty.kind()).collect();
                let kind = TyKind::Tuple(element_tys);
                let ty = sess.tycx.bound(kind.clone(), self.span);

                if is_tuple_type {
                    Ok(Res::new_const(
                        sess.tycx.bound(kind.create_type(), self.span),
                        ConstValue::Type(ty),
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
                            .with_label(Label::primary(type_expr.span, "not a struct type"))),
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
            ast::ExprKind::PointerType(ast::ExprAndMut { inner, is_mutable }) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Pointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type(), self.span),
                    ConstValue::Type(sess.tycx.bound(kind.clone(), self.span)),
                ))
            }
            ast::ExprKind::MultiPointerType(ast::ExprAndMut { inner, is_mutable }) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::MultiPointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type(), self.span),
                    ConstValue::Type(sess.tycx.bound(kind, self.span)),
                ))
            }
            ast::ExprKind::ArrayType(ast::ArrayType { inner, size }) => {
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
                    sess.tycx.bound(kind.clone().create_type(), self.span),
                    ConstValue::Type(sess.tycx.bound(kind, self.span)),
                ))
            }
            ast::ExprKind::SliceType(ast::ExprAndMut { inner, is_mutable }) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Slice(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type(), self.span),
                    ConstValue::Type(sess.tycx.bound(kind, self.span)),
                ))
            }
            ast::ExprKind::StructType(st) => {
                let is_anonymous = st.name.is_empty();

                st.name = if is_anonymous {
                    get_anonymous_struct_name(self.span)
                } else {
                    st.name
                };

                let inferred_ty = sess.tycx.var(self.span);

                let opaque_struct =
                    TyKind::Struct(StructTy::opaque(st.name, st.binding_info_id, st.kind))
                        .create_type();

                inferred_ty
                    .unify(&opaque_struct.clone().create_type(), &mut sess.tycx)
                    .unwrap();

                sess.self_types.push(inferred_ty);
                env.push_scope();

                let opaque_struct_ty = sess.tycx.bound(opaque_struct, self.span);
                st.binding_info_id = sess.bind_symbol(
                    env,
                    st.name,
                    ast::Visibility::Private,
                    inferred_ty,
                    Some(ConstValue::Type(opaque_struct_ty)),
                    false,
                    ast::BindingKind::Type,
                    self.span,
                )?;

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

                env.pop_scope();
                sess.self_types.pop();

                let struct_ty = TyKind::Struct(StructTy {
                    name: st.name,
                    binding_info_id: st.binding_info_id,
                    kind: st.kind,
                    fields: struct_ty_fields,
                });

                Ok(Res::new_const(
                    sess.tycx.bound(struct_ty.clone().create_type(), self.span),
                    ConstValue::Type(sess.tycx.bound(struct_ty, self.span)),
                ))
            }
            ast::ExprKind::FnType(sig) => {
                let res = sig.check(sess, env, expected_ty)?;

                if sig.lib_name.is_some() {
                    Ok(Res::new(res.ty))
                } else {
                    Ok(Res::new_const(
                        sess.tycx.bound(res.ty.kind().create_type(), self.span),
                        ConstValue::Type(res.ty),
                    ))
                }
            }
            ast::ExprKind::SelfType => match sess.self_types.last() {
                Some(&ty) => Ok(Res::new_const(
                    sess.tycx.bound(ty.kind().create_type(), self.span),
                    ConstValue::Type(ty),
                )),
                None => Err(Diagnostic::error()
                    .with_message("`Self` is only available within struct types")
                    .with_label(Label::primary(self.span, "`Self` is invalid here"))),
            },
            ast::ExprKind::NeverType => {
                let ty = sess.tycx.common_types.never;
                Ok(Res::new_const(
                    sess.tycx.bound(ty.kind().create_type(), self.span),
                    ConstValue::Type(ty),
                ))
            }
            ast::ExprKind::UnitType => {
                let ty = sess.tycx.common_types.unit;
                Ok(Res::new_const(
                    sess.tycx.bound(ty.kind().create_type(), self.span),
                    ConstValue::Type(ty),
                ))
            }
            ast::ExprKind::PlaceholderType => {
                let ty = sess.tycx.var(self.span);
                Ok(Res::new_const(
                    sess.tycx.bound(ty.kind().create_type(), self.span),
                    ConstValue::Type(ty),
                ))
            }
            ast::ExprKind::Error => Ok(Res::new(sess.tycx.var(self.span))),
        }?;

        self.ty = res.ty;

        Ok(res)
    }
}

impl Check for ast::Call {
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

                for (index, arg) in self.args.iter_mut().enumerate() {
                    if let Some(param) = fn_ty.params.get(index) {
                        let param_ty = sess.tycx.bound(param.clone(), self.span);
                        let res = arg.check(sess, env, Some(param_ty))?;

                        res.ty
                            .unify(&param_ty, &mut sess.tycx)
                            .or_coerce_expr_into_ty(
                                arg,
                                param_ty,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(&sess.tycx, param_ty, None, res.ty, arg.span)?;
                    } else {
                        // this is a variadic argument, meaning that the argument's
                        // index is greater than the function's param length
                        arg.check(sess, env, None)?;
                    }
                }

                Ok(Res::new(
                    sess.tycx.bound(fn_ty.ret.as_ref().clone(), self.span),
                ))
            }
            ty => {
                for arg in self.args.iter_mut() {
                    arg.check(sess, env, None)?;
                }

                let return_ty = sess.tycx.var(self.span);

                let inferred_fn_ty = TyKind::Fn(FnTy {
                    params: self.args.iter().map(|arg| arg.ty.into()).collect(),
                    ret: Box::new(return_ty.into()),
                    variadic: false,
                    lib_name: None,
                });

                ty.unify(&inferred_fn_ty, &mut sess.tycx).or_report_err(
                    &sess.tycx,
                    inferred_fn_ty,
                    None,
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
                        .with_label(Label::primary(self.expr.span, "can't infer")))
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
                .with_label(Label::primary(
                    self.expr.span,
                    format!("invalid cast to `{}`", target_ty),
                )))
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
        let res = match &self.kind {
            ast::LiteralKind::Unit => Res::new(sess.tycx.common_types.unit),
            ast::LiteralKind::Nil => Res::new(sess.tycx.var(self.span)),
            ast::LiteralKind::Bool(b) => {
                Res::new_const(sess.tycx.common_types.bool, ConstValue::Bool(*b))
            }
            ast::LiteralKind::Int(i) => {
                Res::new_const(sess.tycx.anyint(self.span), ConstValue::Int(*i))
            }
            ast::LiteralKind::Float(f) => {
                Res::new_const(sess.tycx.anyfloat(self.span), ConstValue::Float(*f))
            }
            ast::LiteralKind::Str(_) => Res::new(sess.tycx.common_types.str),
            ast::LiteralKind::Char(_) => Res::new(sess.tycx.common_types.u8),
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

                let expected_ty = sess.tycx.bound(ty_field.ty.clone(), ty_field.span);
                let field_res = field.value.check(sess, env, Some(expected_ty))?;

                field_res
                    .ty
                    .unify(&expected_ty, &mut sess.tycx)
                    .or_coerce_expr_into_ty(
                        &mut field.value,
                        expected_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tycx,
                        expected_ty,
                        Some(ty_field.span),
                        field_res.ty,
                        field.value.span,
                    )?;
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
            .with_label(Label::primary(span, format!("type is `{}`", struct_ty))));
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
            .with_label(Label::primary(span, "missing fields")));
    }

    Ok(Res::new(sess.tycx.bound(TyKind::Struct(struct_ty), span)))
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

    Ok(Res::new(sess.tycx.bound(TyKind::Struct(struct_ty), span)))
}

fn get_anonymous_struct_name(span: Span) -> Ustr {
    ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
}

fn interp_expr(expr: &Expr, sess: &mut CheckSess) -> InterpResult {
    let mut interp_sess = sess.interp.create_session(sess.workspace, &sess.new_ast);
    interp_sess.eval(expr)
}
