mod bind;
mod builtin;
mod const_fold;
mod defer;
mod env;
mod top_level;

use crate::ast::{
    ast::{self, BindingKind, FunctionId, TypedAst},
    const_value::{ConstArray, ConstElement, ConstFunction, ConstValue},
    pattern::{HybridPattern, Pattern, SymbolPattern, UnpackPatternKind},
    ty::{
        FunctionType, FunctionTypeKind, FunctionTypeVarargs, InferTy, PartialStructType,
        StructType, StructTypeField, StructTypeKind, Type, TypeId,
    },
    workspace::{
        BindingInfoFlags, BindingInfoId, ModuleId, PartialBindingInfo, ScopeLevel, Workspace,
    },
};
use crate::common::{
    builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
    target::TargetMetrics,
};
use crate::error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult, SyntaxError, TypeError,
};
use crate::infer::{
    cast::CanCast,
    coerce::{OrCoerceExprIntoTy, OrCoerceExprs},
    display::{DisplayTy, OrReportErr},
    normalize::Normalize,
    substitute::substitute,
    ty_ctx::TyCtx,
    unify::{occurs, UnifyTy, UnifyTyErr},
};
use crate::interp::interp::{Interp, InterpResult};
use crate::span::Span;
use const_fold::binary::const_fold_binary;
use env::{Env, Scope, ScopeKind};
use indexmap::IndexMap;
use std::{collections::HashMap, iter::repeat_with};
use top_level::CallerInfo;
use ustr::{ustr, Ustr, UstrMap, UstrSet};

pub fn check(
    workspace: &mut Workspace,
    ast: Vec<ast::Ast>,
) -> Result<(ast::TypedAst, TyCtx), (TyCtx, TypedAst, Diagnostic)> {
    let mut sess = CheckSess::new(workspace, &ast);

    if let Err(diag) = sess.start() {
        return Err((sess.tycx, sess.new_typed_ast, diag));
    }

    substitute(
        &mut sess.workspace.diagnostics,
        &mut sess.tycx,
        &sess.new_typed_ast,
    );

    for binding in sess
        .new_typed_ast
        .bindings
        .iter()
        .map(|(_, b)| b)
        .filter(|b| b.kind.is_extern())
    {
        if !ty_is_extern(&binding.ty.normalize(&sess.tycx)) {
            sess.workspace.diagnostics.push(
                Diagnostic::error()
                    .with_message("type is not valid in extern context")
                    .with_label(Label::primary(
                        binding.ty_expr.as_ref().unwrap().span,
                        "cannot be used in extern context",
                    )),
            )
        }
    }

    Ok((sess.new_typed_ast, sess.tycx))
}

fn ty_is_extern(ty: &Type) -> bool {
    match ty {
        Type::Never | Type::Unit | Type::Bool | Type::Int(_) | Type::Uint(_) | Type::Float(_) => {
            true
        }

        Type::Module(_)
        | Type::Type(_)
        | Type::AnyType
        | Type::Var(_)
        | Type::Infer(_, _)
        | Type::Unknown => false,

        Type::Pointer(inner, _)
        | Type::MultiPointer(inner, _)
        | Type::Array(inner, _)
        | Type::Slice(inner, _) => ty_is_extern(inner),

        Type::Function(f) => {
            ty_is_extern(&f.ret)
                && f.varargs
                    .as_ref()
                    .map_or(true, |v| v.ty.as_ref().map_or(true, |ty| ty_is_extern(ty)))
                && f.params.iter().all(ty_is_extern)
        }

        Type::Tuple(tys) => tys.iter().all(ty_is_extern),

        Type::Struct(st) => st.fields.iter().all(|f| ty_is_extern(&f.ty)),
    }
}

pub struct CheckSess<'s> {
    pub workspace: &'s mut Workspace,
    pub target_metrics: TargetMetrics,

    pub interp: Interp,

    pub tycx: TyCtx,

    // The ast's being processed
    pub old_asts: &'s Vec<ast::Ast>,

    // The new typed ast being generated
    pub new_typed_ast: ast::TypedAst,
    pub checked_modules: HashMap<ModuleId, TypeId>,

    // Information that's relevant for the global context
    pub global_scopes: HashMap<ModuleId, Scope>,
    pub builtin_types: UstrMap<BindingInfoId>,

    // Stack of function frames, each ast::Function creates its own frame
    pub function_frames: Vec<FunctionFrame>,

    // Stack of `Self` types
    pub self_types: Vec<TypeId>,

    // The current loop (while/for) depth
    // 0 meaning we are not in a loop, > 1 means we are in a loop
    pub loop_depth: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionFrame {
    return_ty: TypeId,
    return_ty_span: Span,
    scope_level: ScopeLevel,
}

impl<'s> CheckSess<'s> {
    pub fn new(workspace: &'s mut Workspace, old_asts: &'s Vec<ast::Ast>) -> Self {
        let target_metrics = workspace.build_options.target_platform.metrics();
        let interp = Interp::new(workspace.build_options.clone());

        Self {
            workspace,
            target_metrics,
            interp,
            tycx: TyCtx::default(),
            old_asts,
            new_typed_ast: ast::TypedAst::default(),
            checked_modules: HashMap::default(),
            global_scopes: HashMap::default(),
            builtin_types: UstrMap::default(),
            function_frames: vec![],
            self_types: vec![],
            loop_depth: 0,
        }
    }

    pub fn start(&mut self) -> DiagnosticResult<()> {
        self.add_builtin_types();

        for ast in self.old_asts.iter() {
            self.check_ast(ast)?;
        }

        Ok(())
    }

    pub fn with_function_frame<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        frame: FunctionFrame,
        mut f: F,
    ) -> T {
        self.function_frames.push(frame);
        let result = f(self);
        self.function_frames.pop();
        result
    }

    pub fn with_env<T, F: FnMut(&mut Self, Env) -> T>(
        &mut self,
        module_id: ModuleId,
        mut f: F,
    ) -> T {
        let module_info = *self.workspace.get_module_info(module_id).unwrap();
        f(self, Env::new(module_id, module_info))
    }

    pub fn function_frame(&self) -> Option<FunctionFrame> {
        self.function_frames.last().map(|&f| f)
    }

    pub fn extract_const_type(
        &self,
        const_value: Option<ConstValue>,
        ty: TypeId,
        span: Span,
    ) -> DiagnosticResult<TypeId> {
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

    pub fn extract_const_int(
        &mut self,
        value: Option<ConstValue>,
        ty: TypeId,
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
        let mk = |sess: &mut CheckSess, symbol: &str, ty: TypeId| {
            let symbol = ustr(symbol);

            let id = sess.workspace.add_binding_info(PartialBindingInfo {
                module_id: Default::default(),
                symbol,
                visibility: ast::Visibility::Public,
                ty: sess
                    .tycx
                    .bound_maybe_spanned(ty.as_kind().create_type(), None),
                const_value: Some(ConstValue::Type(ty)),
                is_mutable: false,
                kind: ast::BindingKind::Normal,
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

        mk(self, builtin::SYM_NEVER, self.tycx.common_types.never);
        mk(self, builtin::SYM_TYPE, self.tycx.common_types.anytype);

        mk(self, builtin::SYM_STR, self.tycx.common_types.str);
    }

    pub fn is_mutable(&self, expr: &ast::ExprKind) -> bool {
        match expr {
            ast::ExprKind::MemberAccess(access) => self.is_mutable(&access.expr.kind),
            ast::ExprKind::Ident(ident) => {
                self.workspace
                    .get_binding_info(ident.binding_info_id)
                    .unwrap()
                    .is_mutable
            }
            _ => true,
        }
    }
}

pub type CheckResult = DiagnosticResult<Res>;

#[derive(Debug)]
pub struct Res {
    ty: TypeId,
    const_value: Option<ConstValue>,
}

impl Res {
    pub fn new(ty: TypeId) -> Self {
        Self {
            ty,
            const_value: None,
        }
    }

    pub fn new_maybe_const(ty: TypeId, const_value: Option<ConstValue>) -> Self {
        Self { ty, const_value }
    }

    pub fn new_const(ty: TypeId, const_value: ConstValue) -> Self {
        Self {
            ty,
            const_value: Some(const_value),
        }
    }
}

pub trait Check
where
    Self: Sized,
{
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<TypeId>,
    ) -> CheckResult;
}

impl Check for ast::Binding {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_ty: Option<TypeId>,
    ) -> CheckResult {
        self.module_id = env.module_id();

        if let ast::BindingKind::Extern(Some(lib)) = &self.kind {
            // Collect extern library to be linked later
            sess.workspace.extern_libraries.insert(lib.clone());
        }

        self.ty = if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
            sess.extract_const_type(res.const_value, res.ty, ty_expr.span)?
        } else {
            sess.tycx.var(self.pattern.span())
        };

        if let ast::BindingKind::Intrinsic(_) = &self.kind {
            if let Pattern::Symbol(pattern) = &mut self.pattern {
                let id = sess.bind_symbol(
                    env,
                    pattern.symbol,
                    self.visibility,
                    self.ty,
                    None,
                    pattern.is_mutable,
                    self.kind.clone(),
                    pattern.span,
                )?;

                pattern.id = id;

                sess.new_typed_ast.push_binding(&[id], self.clone());

                return Ok(Res::new(self.ty));
            } else {
                panic!();
            }
        }

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
            match &self.pattern {
                Pattern::StructUnpack(pat)
                | Pattern::TupleUnpack(pat)
                | Pattern::Hybrid(HybridPattern {
                    unpack: UnpackPatternKind::Struct(pat) | UnpackPatternKind::Tuple(pat),
                    ..
                }) => {
                    return Err(Diagnostic::error()
                        .with_message("unpack pattern requires a value to unpack")
                        .with_label(Label::primary(pat.span, "illegal pattern use")));
                }
                Pattern::Symbol(_) => (),
            }

            None
        };

        let ty_kind = self.ty.normalize(&sess.tycx);

        let is_type_or_module = ty_kind.is_type() || ty_kind.is_module();
        let is_any_pattern_mut = self.pattern.iter().any(|p| p.is_mutable);

        // Global immutable bindings must resolve to a const value, unless it is:
        // - of type `type` or `module`
        // - an extern binding
        if env.scope_level().is_global()
            && !is_type_or_module
            && !is_any_pattern_mut
            && const_value.is_none()
            && !matches!(
                self.kind,
                BindingKind::Extern(_) | BindingKind::Intrinsic(_)
            )
        {
            return Err(Diagnostic::error()
                .with_message(format!("immutable top level binding must be constant"))
                .with_label(Label::primary(self.pattern.span(), "must be constant"))
                .maybe_with_label(self.expr.as_ref().map(|expr| {
                    Label::secondary(expr.span, "doesn't resolve to a constant value")
                })));
        }

        // Bindings of type `type` and `module` cannot be assigned to mutable bindings
        if is_type_or_module {
            self.pattern
                .iter()
                .filter(|pat| pat.is_mutable)
                .for_each(|pat| {
                    sess.workspace.diagnostics.push(
                        Diagnostic::error()
                            .with_message("variable of type `type` or `module` must be immutable")
                            .with_label(Label::primary(pat.span, "variable is mutable"))
                            .with_note("try removing the `mut` from the declaration"),
                    );
                });
        }

        sess.bind_pattern(
            env,
            &mut self.pattern,
            self.visibility,
            self.ty,
            const_value.clone(),
            &self.kind,
            self.expr
                .as_ref()
                .map(|e| e.span)
                .or_else(|| self.ty_expr.as_ref().map(|e| e.span))
                .unwrap_or(self.span),
        )?;

        // If this binding matches the entry point function's requirements,
        // Tag it as the entry function
        // Requirements:
        // - Is declared in the root module
        // - It is in global scope
        // - Its name is the same as the required `entry_point_function_name`
        if let Some(ConstValue::Function(_)) = &const_value {
            if let Pattern::Symbol(pattern) = &mut self.pattern {
                if sess.workspace.build_options.need_entry_point_function()
                    && self.module_id == sess.workspace.root_module_id
                    && env.scope_level().is_global()
                    && pattern.symbol
                        == sess
                            .workspace
                            .build_options
                            .entry_point_function_name()
                            .unwrap()
                {
                    if let Some(_) = sess.workspace.entry_point_function_id {
                        // TODO: When attributes are implemented:
                        // TODO: Emit error that we can't have two entry point functions
                        // TODO: Also, this should be moved to another place...
                    } else {
                        sess.workspace.entry_point_function_id = Some(pattern.id);
                    }
                }
            }
        }

        Ok(Res::new_maybe_const(self.ty, const_value))
    }
}

impl Check for ast::FunctionSig {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<TypeId>,
    ) -> CheckResult {
        let mut ty_params = vec![];

        if !self.params.is_empty() {
            let mut defined_params = UstrMap::default();

            for param in self.params.iter_mut() {
                param.ty = if let Some(expr) = &mut param.ty_expr {
                    let res = expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                    sess.extract_const_type(res.const_value, res.ty, expr.span)?
                } else {
                    sess.tycx.var(param.pattern.span())
                };

                ty_params.push(param.ty.into());

                for pat in param.pattern.iter() {
                    if let Some(already_defined_span) = defined_params.insert(pat.symbol, pat.span)
                    {
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
                    Type::Function(f) => {
                        if f.params.len() == 1 {
                            let symbol = ustr("it");

                            self.params.push(ast::FunctionParam {
                                pattern: Pattern::Symbol(SymbolPattern {
                                    id: Default::default(),
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
            let res = expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
            sess.extract_const_type(res.const_value, res.ty, expr.span)?
        } else if self.kind.is_extern() {
            sess.tycx.common_types.unit
        } else {
            sess.tycx.var(self.span)
        };

        let varargs = if let Some(varargs) = &mut self.varargs {
            let ty = if let Some(ty) = &mut varargs.ty {
                let res = ty.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let ty = sess.extract_const_type(res.const_value, res.ty, ty.span)?;

                Some(ty.as_kind())
            } else {
                None
            };

            if ty.is_none() && !self.kind.is_extern() {
                return Err(Diagnostic::error()
                    .with_message(
                        "varargs without type annotation are only valid in extern functions",
                    )
                    .with_label(Label::primary(varargs.span, "missing a type annotation")));
            }

            Some(Box::new(FunctionTypeVarargs { ty }))
        } else {
            None
        };

        self.ty = sess.tycx.bound(
            Type::Function(FunctionType {
                params: ty_params,
                ret: Box::new(ret.into()),
                varargs,
                kind: self.kind.clone(),
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
        expected_ty: Option<TypeId>,
    ) -> CheckResult {
        let res = match &mut self.kind {
            ast::ExprKind::Binding(binding) => {
                binding.check(sess, env, None)?;
                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Defer(defer) => {
                defer.expr.check(sess, env, None)?;

                // Add the expression to this scope's defer stack
                env.scope_mut()
                    .defer_stack
                    .deferred
                    .push(defer.expr.as_ref().clone());

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
                ast::Builtin::Import(path) => sess.check_import(path),
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    let res = expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                    sess.extract_const_type(res.const_value, res.ty, expr.span)?;
                    Ok(Res::new(sess.tycx.common_types.uint))
                }
                ast::Builtin::Run(expr, run_result) => {
                    let res = expr.check(sess, env, None)?;

                    if sess.workspace.build_options.check_mode {
                        Ok(Res::new(res.ty))
                    } else {
                        // TODO (Ron): unwrap interp result into a diagnostic
                        let interp_value = interp_expr(&expr, sess, env.module_id()).unwrap();

                        let ty = res.ty.normalize(&sess.tycx);

                        match interp_value.try_into_const_value(&mut sess.tycx, &ty, self.span) {
                            Ok(const_value) => {
                                *run_result = Some(const_value.clone());
                                Ok(Res::new_const(res.ty, const_value))
                            }
                            Err(value_str) => Err(Diagnostic::error()
                                .with_message(format!(
                                    "compile-time evaluation cannot result in `{}`",
                                    value_str,
                                ))
                                .with_label(Label::primary(self.span, "evaluated here"))),
                        }
                    }
                }
                ast::Builtin::Panic(expr) => {
                    if let Some(expr) = expr {
                        expr.check(sess, env, None)?;
                    }
                    Ok(Res::new(sess.tycx.common_types.unit))
                }
            },
            ast::ExprKind::Function(function) => {
                let sig_res = function.sig.check(sess, env, expected_ty)?;
                let fn_ty = sig_res.ty.normalize(&sess.tycx).into_fn();

                let return_ty = sess.tycx.bound(
                    fn_ty.ret.as_ref().clone(),
                    function
                        .sig
                        .ret
                        .as_ref()
                        .map_or(function.sig.span, |e| e.span),
                );

                let return_ty_span = function
                    .sig
                    .ret
                    .as_ref()
                    .map_or(function.sig.span, |e| e.span);

                env.push_scope(ScopeKind::Function);

                for (param, param_ty) in function.sig.params.iter_mut().zip(fn_ty.params.iter()) {
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
                        &ast::BindingKind::Normal,
                        span,
                    )?;
                }

                function.id = sess.new_typed_ast.push_function(ast::Function {
                    id: FunctionId::unknown(),
                    module_id: env.module_id(),
                    ty: sig_res.ty,
                    kind: ast::FunctionKind::Orphan {
                        sig: function.sig.clone(),
                        body: None,
                    },
                });

                env.insert_function(function.sig.name, function.id);

                let body_res = sess.with_function_frame(
                    FunctionFrame {
                        return_ty,
                        return_ty_span,
                        scope_level: env.scope_level(),
                    },
                    |sess| function.body.check(sess, env, None),
                )?;

                let mut unify_res = body_res.ty.unify(&return_ty, &mut sess.tycx);

                if let Some(last_expr) = function.body.exprs.last_mut() {
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
                    function.body.span,
                )?;

                env.pop_scope();

                match &mut sess
                    .new_typed_ast
                    .get_function_mut(function.id)
                    .unwrap()
                    .kind
                {
                    ast::FunctionKind::Orphan { body, .. } => *body = Some(function.body.clone()),
                }

                let const_value = ConstValue::Function(ConstFunction {
                    id: function.id,
                    name: function.sig.name,
                });

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    sig_res.ty,
                    self.span,
                );

                Ok(Res::new_const(sig_res.ty, const_value))
            }
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

                env.push_scope(ScopeKind::Loop);
                sess.loop_depth += 1;

                while_.block.check(sess, env, None)?;

                sess.loop_depth -= 1;
                env.pop_scope();

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
                            Type::Array(inner, ..) | Type::Slice(inner, ..) => {
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

                env.push_scope(ScopeKind::Loop);
                sess.loop_depth += 1;

                for_.iter_binding.id = sess.bind_symbol(
                    env,
                    for_.iter_binding.name,
                    ast::Visibility::Private,
                    iter_ty,
                    None,
                    false,
                    ast::BindingKind::Normal,
                    self.span, // TODO: use iter's actual span
                )?;

                if let Some(index_binding) = &mut for_.index_binding {
                    index_binding.id = sess.bind_symbol(
                        env,
                        index_binding.name,
                        ast::Visibility::Private,
                        sess.tycx.common_types.uint,
                        None,
                        false,
                        ast::BindingKind::Normal,
                        self.span, // TODO: use iter_index's actual span
                    )?;
                }

                for_.block.check(sess, env, None)?;

                sess.loop_depth -= 1;
                env.pop_scope();

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Break(_) if sess.loop_depth == 0 => {
                return Err(SyntaxError::outside_of_loop(self.span, "break"));
            }
            ast::ExprKind::Continue(_) if sess.loop_depth == 0 => {
                return Err(SyntaxError::outside_of_loop(self.span, "continue"));
            }
            ast::ExprKind::Break(term) | ast::ExprKind::Continue(term) => {
                for scope in env.scopes().iter().rev() {
                    if let ScopeKind::Loop = scope.kind {
                        break;
                    }

                    for expr in scope.defer_stack.deferred.iter().rev() {
                        term.deferred.push(expr.clone())
                    }
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

                // Recursively add all defers in the stack, in reverse
                for scope in env.scopes().iter().rev() {
                    for expr in scope.defer_stack.deferred.iter().rev() {
                        ret.deferred.push(expr.clone())
                    }
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
                                ast::ExprKind::ConstValue(ConstValue::Unit(())),
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
                let lhs_res = binary.lhs.check(sess, env, None)?;
                let rhs_res = binary.rhs.check(sess, env, Some(lhs_res.ty))?;

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
                        let const_value = const_fold_binary(lhs, rhs, binary.op, self.span)?;
                        *self = ast::Expr::typed(
                            ast::ExprKind::ConstValue(const_value.clone()),
                            result_ty,
                            self.span,
                        );
                        Ok(Res::new_const(result_ty, const_value))
                    }
                    _ => Ok(Res::new(result_ty)),
                }
            }
            ast::ExprKind::Unary(unary) => {
                let res = unary.lhs.check(sess, env, None)?;

                match unary.op {
                    ast::UnaryOp::Ref(is_mutable) => {
                        let ty = sess.tycx.bound(
                            Type::Pointer(Box::new(res.ty.into()), is_mutable),
                            unary.span,
                        );
                        Ok(Res::new(ty))
                    }
                    ast::UnaryOp::Deref => {
                        let pointee = sess.tycx.var(unary.span);

                        let ptr_ty = sess
                            .tycx
                            .bound(Type::Pointer(Box::new(pointee.into()), false), unary.span);

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

                        if let Some(const_value) = res.const_value {
                            let const_value = match const_value {
                                ConstValue::Bool(v) => ConstValue::Bool(!v),
                                ConstValue::Int(v) => ConstValue::Int(!v),
                                _ => panic!(),
                            };

                            *self = ast::Expr::typed(
                                ast::ExprKind::ConstValue(const_value.clone()),
                                res.ty,
                                self.span,
                            );

                            Ok(Res::new_const(res.ty, const_value))
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

                        if let Some(const_value) = res.const_value.as_ref() {
                            let const_value = match const_value {
                                ConstValue::Int(i) => ConstValue::Int(-i),
                                ConstValue::Float(f) => ConstValue::Float(-f),
                                _ => unreachable!("got {:?}", const_value),
                            };

                            *self = ast::Expr::typed(
                                ast::ExprKind::ConstValue(const_value.clone()),
                                res.ty,
                                self.span,
                            );

                            Ok(Res::new_const(res.ty, const_value))
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

                let const_value = if let Some(ConstValue::Int(const_index)) = index_res.const_value
                {
                    // compile-time array bounds check
                    if let Type::Array(_, size) = kind_deref {
                        if const_index < 0 || const_index >= size as _ {
                            let msg = format!(
                                "index out of array bounds - expected 0 to {}, but found {}",
                                size - 1,
                                const_index
                            );
                            return Err(Diagnostic::error().with_message(msg).with_label(
                                Label::primary(sub.index.span, "index out of bounds"),
                            ));
                        }
                    }

                    if let Some(ConstValue::Array(const_array)) = &res.const_value {
                        Some(const_array.values[const_index as usize].clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                match kind_deref {
                    Type::Array(inner, ..)
                    | Type::Slice(inner, ..)
                    | Type::MultiPointer(inner, ..) => {
                        let ty = sess.tycx.bound(*inner, self.span);

                        if let Some(const_value) = &const_value {
                            *self = ast::Expr::typed(
                                ast::ExprKind::ConstValue(const_value.clone()),
                                ty,
                                self.span,
                            );
                        }

                        Ok(Res::new_maybe_const(ty, const_value))
                    }
                    _ => Err(Diagnostic::error()
                        .with_message(format!("cannot index type `{}`", kind.display(&sess.tycx)))
                        .with_label(Label::primary(sub.expr.span, "cannot index"))),
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
                    Type::Array(inner, ..) => (inner, sess.is_mutable(&slice.expr.kind)),
                    Type::Slice(inner, is_mutable) | Type::MultiPointer(inner, is_mutable) => {
                        (inner, is_mutable)
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "cannot slice type `{}`",
                                expr_ty.display(&sess.tycx)
                            ))
                            .with_label(Label::primary(slice.expr.span, "cannot slice")))
                    }
                };

                let ty = sess
                    .tycx
                    .bound(Type::Slice(result_ty, is_mutable), self.span);

                Ok(Res::new(ty))
            }
            ast::ExprKind::Call(call) => call.check(sess, env, expected_ty),
            ast::ExprKind::MemberAccess(access) => {
                let res = access.expr.check(sess, env, None)?;

                let member_tuple_index = access.member.as_str().parse::<usize>();

                // if the accessed expression's type is not resolved yet - try unifying it with a partial type
                match res.ty.normalize(&sess.tycx) {
                    Type::Var(_)
                    | Type::Infer(_, InferTy::PartialStruct(_))
                    | Type::Infer(_, InferTy::PartialTuple(_)) => {
                        // if this parsing operation succeeds, this is a tuple member access - `tup.0`
                        // otherwise, this is a struct field access - `strct.field`

                        let member_ty = sess.tycx.var(self.span);
                        let partial_ty = match member_tuple_index {
                            Ok(index) => {
                                let elements = repeat_with(|| sess.tycx.var(self.span))
                                    .take(index + 1)
                                    .map(Type::Var)
                                    .collect::<Vec<Type>>();
                                sess.tycx.partial_tuple(elements, self.span)
                            }
                            Err(_) => {
                                let fields =
                                    IndexMap::from([(access.member, Type::Var(member_ty))]);
                                sess.tycx
                                    .partial_struct(PartialStructType(fields), self.span)
                            }
                        };

                        res.ty.unify(&partial_ty, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            partial_ty,
                            None,
                            res.ty,
                            access.expr.span,
                        )?;
                    }
                    _ => (),
                }

                let kind = res.ty.normalize(&sess.tycx);

                match &kind.maybe_deref_once() {
                    ty @ Type::Tuple(elements)
                    | ty @ Type::Infer(_, InferTy::PartialTuple(elements)) => {
                        match member_tuple_index {
                            Ok(index) => match elements.get(index) {
                                Some(field_ty) => {
                                    let const_value =
                                        if let Some(ConstValue::Tuple(const_elements)) =
                                            &res.const_value
                                        {
                                            Some(const_elements[index].value.clone())
                                        } else {
                                            None
                                        };

                                    Ok(Res::new_maybe_const(
                                        sess.tycx.bound(field_ty.clone(), self.span),
                                        const_value,
                                    ))
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
                    ty @ Type::Struct(st) => match st.find_field(access.member) {
                        Some(field) => {
                            let const_value =
                                if let Some(ConstValue::Struct(const_fields)) = &res.const_value {
                                    Some(const_fields[&field.symbol].value.clone())
                                } else {
                                    None
                                };

                            Ok(Res::new_maybe_const(
                                sess.tycx.bound(field.ty.clone(), self.span),
                                const_value,
                            ))
                        }
                        None => Err(TypeError::invalid_struct_field(
                            access.expr.span,
                            access.member,
                            ty.display(&sess.tycx),
                        )),
                    },
                    ty @ Type::Infer(_, InferTy::PartialStruct(partial_struct)) => {
                        match partial_struct.get(&access.member) {
                            Some(field_ty) => {
                                let const_value = if let Some(ConstValue::Struct(const_fields)) =
                                    &res.const_value
                                {
                                    Some(const_fields[&access.member].value.clone())
                                } else {
                                    None
                                };

                                Ok(Res::new_maybe_const(
                                    sess.tycx.bound(field_ty.clone(), self.span),
                                    const_value,
                                ))
                            }
                            None => Err(TypeError::invalid_struct_field(
                                access.expr.span,
                                access.member,
                                ty.display(&sess.tycx),
                            )),
                        }
                    }
                    Type::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => Ok(
                        Res::new_const(sess.tycx.common_types.uint, ConstValue::Int(*size as _)),
                    ),
                    Type::Slice(..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        let const_value = if let Some(ConstValue::Str(s)) = &res.const_value {
                            Some(ConstValue::Uint(s.len() as _))
                        } else {
                            None
                        };

                        Ok(Res::new_maybe_const(
                            sess.tycx.common_types.uint,
                            const_value,
                        ))
                    }
                    Type::Slice(inner, is_mutable)
                        if access.member.as_str() == BUILTIN_FIELD_DATA =>
                    {
                        Ok(Res::new(sess.tycx.bound(
                            Type::MultiPointer(inner.clone(), *is_mutable),
                            self.span,
                        )))
                    }
                    Type::Module(module_id) => {
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
                    ty => Err(Diagnostic::error()
                        .with_message(format!(
                            "type `{}` has no member `{}`",
                            ty.display(&sess.tycx),
                            access.member
                        ))
                        .with_label(Label::primary(access.expr.span, ""))),
                }
            }
            ast::ExprKind::Ident(ident) => {
                if let Some(id) = env.find_function(ident.symbol) {
                    let function = sess.new_typed_ast.get_function(id).unwrap();

                    let ty = function.ty;
                    let const_value = ConstValue::Function(function.as_const_function());

                    *self = ast::Expr::typed(
                        ast::ExprKind::ConstValue(const_value.clone()),
                        ty,
                        self.span,
                    );

                    Ok(Res::new_const(ty, const_value))
                } else {
                    match sess.get_symbol(env, ident.symbol) {
                        Some(id) => {
                            // this is a local binding
                            ident.binding_info_id = id;

                            sess.workspace.add_binding_info_use(id, self.span);

                            let binding_info = sess.workspace.get_binding_info(id).unwrap();

                            let binding_ty = binding_info.ty.normalize(&sess.tycx);

                            let min_scope_level = sess
                                .function_frame()
                                .map_or(ScopeLevel::Global, |f| f.scope_level);

                            if !binding_ty.is_type()
                                && !binding_ty.is_module()
                                && !binding_info.scope_level.is_global()
                                && binding_info.scope_level < min_scope_level
                            {
                                return Err(Diagnostic::error()
                            .with_message(
                                "can't capture environment - closures are not implemented yet",
                            )
                            .with_label(Label::primary(self.span, "can't capture")));
                            }

                            let ty = binding_info.ty;

                            if let Some(const_value) = &binding_info.const_value {
                                *self = ast::Expr::typed(
                                    ast::ExprKind::ConstValue(const_value.clone()),
                                    ty,
                                    self.span,
                                );
                            }

                            Ok(Res::new_maybe_const(ty, binding_info.const_value.clone()))
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

                            if let Some(const_value) = &res.const_value {
                                *self = ast::Expr::typed(
                                    ast::ExprKind::ConstValue(const_value.clone()),
                                    res.ty,
                                    self.span,
                                );
                            }

                            Ok(res)
                        }
                    }
                }
            }
            ast::ExprKind::ArrayLiteral(lit) => match &mut lit.kind {
                ast::ArrayLiteralKind::List(elements) => {
                    let element_ty_span = elements.first().map_or(self.span, |e| e.span);
                    let element_ty = sess.tycx.var(element_ty_span);

                    let mut elements_res: Vec<Res> = vec![];

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

                        elements_res.push(res);
                    }

                    let ty = sess.tycx.bound(
                        Type::Array(Box::new(element_ty.into()), elements.len()),
                        self.span,
                    );

                    let is_const_array = elements_res.iter().all(|res| res.const_value.is_some());

                    if is_const_array {
                        let const_array = ConstValue::Array(ConstArray {
                            values: elements_res
                                .iter()
                                .map(|res| res.const_value.clone().unwrap())
                                .collect(),
                            element_ty,
                        });

                        *self = ast::Expr::typed(
                            ast::ExprKind::ConstValue(const_array.clone()),
                            ty,
                            self.span,
                        );

                        Ok(Res::new_const(ty, const_array))
                    } else {
                        Ok(Res::new(ty))
                    }
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    let len_res = len.check(sess, env, None)?;
                    let array_len =
                        sess.extract_const_int(len_res.const_value, len_res.ty, len.span)?;

                    if array_len < 0 {
                        return Err(TypeError::negative_array_len(len.span, array_len));
                    }

                    let res = expr.check(sess, env, None)?;

                    let ty = sess.tycx.bound(
                        Type::Array(Box::new(res.ty.into()), array_len as _),
                        self.span,
                    );

                    if let Some(const_value) = &res.const_value {
                        let const_array = ConstValue::Array(ConstArray {
                            values: vec![const_value.clone(); array_len as usize],
                            element_ty: res.ty,
                        });

                        *self = ast::Expr::typed(
                            ast::ExprKind::ConstValue(const_array.clone()),
                            ty,
                            self.span,
                        );

                        Ok(Res::new_const(ty, const_array))
                    } else {
                        Ok(Res::new(ty))
                    }
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                // when a tuple literal is empty, it is either a unit value or unit type
                if lit.elements.is_empty() {
                    let unit_ty = sess.tycx.common_types.unit;

                    let (ty, const_value) =
                        if expected_ty.map_or(false, |ty| ty.normalize(&sess.tycx).is_type()) {
                            (
                                sess.tycx.bound(unit_ty.as_kind().create_type(), self.span),
                                ConstValue::Type(unit_ty),
                            )
                        } else {
                            (unit_ty, ConstValue::Unit(()))
                        };

                    *self = ast::Expr::typed(
                        ast::ExprKind::ConstValue(const_value.clone()),
                        ty,
                        self.span,
                    );

                    return Ok(Res::new_const(ty, const_value));
                }

                let mut elements_res = vec![];

                for el in lit.elements.iter_mut() {
                    let res = el.check(sess, env, None)?;
                    elements_res.push(res);
                }

                let is_const_tuple = elements_res.iter().all(|res| res.const_value.is_some());

                if is_const_tuple {
                    let const_values: Vec<&ConstValue> = elements_res
                        .iter()
                        .map(|res| res.const_value.as_ref().unwrap())
                        .collect();

                    let is_tuple_type = const_values.iter().all(|v| v.is_type());

                    if is_tuple_type {
                        // unwrap element type constants
                        let element_tys: Vec<Type> = elements_res
                            .iter()
                            .map(|res| {
                                res.const_value
                                    .as_ref()
                                    .unwrap()
                                    .as_type()
                                    .normalize(&sess.tycx)
                                    .clone()
                            })
                            .collect();

                        let kind = Type::Tuple(element_tys);

                        let ty = sess.tycx.bound(kind.clone(), self.span);

                        let const_value = ConstValue::Type(ty);

                        *self = ast::Expr::typed(
                            ast::ExprKind::ConstValue(const_value.clone()),
                            ty,
                            self.span,
                        );

                        Ok(Res::new_const(
                            sess.tycx.bound(kind.create_type(), self.span),
                            const_value,
                        ))
                    } else {
                        let element_tys: Vec<TypeId> = lit.elements.iter().map(|e| e.ty).collect();

                        let element_ty_kinds: Vec<Type> =
                            element_tys.iter().map(|ty| ty.as_kind()).collect();

                        let kind = Type::Tuple(element_ty_kinds.clone());

                        let ty = sess.tycx.bound(kind.clone(), self.span);

                        let const_value = ConstValue::Tuple(
                            const_values
                                .iter()
                                .cloned()
                                .cloned()
                                .zip(element_tys)
                                .map(|(value, ty)| ConstElement { value, ty })
                                .collect(),
                        );

                        *self = ast::Expr::typed(
                            ast::ExprKind::ConstValue(const_value.clone()),
                            ty,
                            self.span,
                        );

                        Ok(Res::new_const(ty, const_value))
                    }
                } else {
                    let element_tys: Vec<Type> =
                        lit.elements.iter().map(|e| e.ty.as_kind()).collect();

                    let kind = Type::Tuple(element_tys);

                    let ty = sess.tycx.bound(kind.clone(), self.span);

                    Ok(Res::new(ty))
                }
            }
            ast::ExprKind::StructLiteral(lit) => {
                let res = match &mut lit.type_expr {
                    Some(type_expr) => {
                        let res =
                            type_expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                        let ty =
                            sess.extract_const_type(res.const_value, res.ty, type_expr.span)?;

                        let kind = ty.normalize(&sess.tycx);

                        match kind {
                            Type::Struct(struct_ty) => check_named_struct_literal(
                                sess,
                                env,
                                struct_ty,
                                &mut lit.fields,
                                self.span,
                            )?,
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "type `{}` does not support struct initialization syntax",
                                        ty
                                    ))
                                    .with_label(Label::primary(
                                        type_expr.span,
                                        "not a struct type",
                                    )))
                            }
                        }
                    }
                    None => match expected_ty {
                        Some(ty) => {
                            let kind = ty.normalize(&sess.tycx);
                            match kind {
                                Type::Struct(struct_ty) => check_named_struct_literal(
                                    sess,
                                    env,
                                    struct_ty,
                                    &mut lit.fields,
                                    self.span,
                                )?,
                                _ => check_anonymous_struct_literal(
                                    sess,
                                    env,
                                    &mut lit.fields,
                                    self.span,
                                )?,
                            }
                        }
                        None => {
                            check_anonymous_struct_literal(sess, env, &mut lit.fields, self.span)?
                        }
                    },
                };

                if let Some(const_value) = &res.const_value {
                    *self = ast::Expr::typed(
                        ast::ExprKind::ConstValue(const_value.clone()),
                        res.ty,
                        self.span,
                    );
                }

                Ok(res)
            }
            ast::ExprKind::Literal(lit) => {
                let const_value: ConstValue = lit.kind.into();

                let ty = match &lit.kind {
                    ast::LiteralKind::Nil => sess.tycx.var(self.span),
                    ast::LiteralKind::Bool(_) => sess.tycx.common_types.bool,
                    ast::LiteralKind::Int(_) => sess.tycx.anyint(self.span),
                    ast::LiteralKind::Float(_) => sess.tycx.anyfloat(self.span),
                    ast::LiteralKind::Str(_) => sess.tycx.common_types.str,
                    ast::LiteralKind::Char(_) => sess.tycx.common_types.u8,
                };

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::PointerType(ast::ExprAndMut { inner, is_mutable }) => {
                let res = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = Type::Pointer(Box::new(inner_kind.into()), *is_mutable);

                let ty = sess.tycx.bound(kind.clone().create_type(), self.span);
                let const_value = ConstValue::Type(sess.tycx.bound(kind, self.span));

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::MultiPointerType(ast::ExprAndMut { inner, is_mutable }) => {
                let res = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = Type::MultiPointer(Box::new(inner_kind.into()), *is_mutable);

                let ty = sess.tycx.bound(kind.clone().create_type(), self.span);
                let const_value = ConstValue::Type(sess.tycx.bound(kind, self.span));

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::ArrayType(ast::ArrayType { inner, size }) => {
                let inner_res = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_ty =
                    sess.extract_const_type(inner_res.const_value, inner_res.ty, inner.span)?;

                let size_res = size.check(sess, env, None)?;

                let size_value =
                    sess.extract_const_int(size_res.const_value, size_res.ty, size.span)?;

                if size_value < 0 {
                    return Err(TypeError::negative_array_len(size.span, size_value));
                }

                let kind = Type::Array(Box::new(inner_ty.into()), size_value as usize);

                let ty = sess.tycx.bound(kind.clone().create_type(), self.span);
                let const_value = ConstValue::Type(sess.tycx.bound(kind, self.span));

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::SliceType(ast::ExprAndMut { inner, is_mutable }) => {
                let res = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = Type::Slice(Box::new(inner_kind.into()), *is_mutable);

                let ty = sess.tycx.bound(kind.clone().create_type(), self.span);
                let const_value = ConstValue::Type(sess.tycx.bound(kind, self.span));

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::StructType(st) => {
                st.name = if st.name.is_empty() {
                    get_anonymous_struct_name(self.span)
                } else {
                    st.name
                };

                // the struct's main type variable
                let struct_ty_var = sess.tycx.bound(
                    Type::Struct(StructType::opaque(st.name, st.binding_info_id, st.kind)),
                    self.span,
                );

                // the struct's main type variable, in its `type` variation
                let struct_ty_type_var = sess
                    .tycx
                    .bound(struct_ty_var.as_kind().create_type(), self.span);

                sess.self_types.push(struct_ty_var);

                env.push_scope(ScopeKind::Block);

                st.binding_info_id = sess.bind_symbol(
                    env,
                    st.name,
                    ast::Visibility::Private,
                    struct_ty_type_var,
                    Some(ConstValue::Type(struct_ty_var)),
                    false,
                    ast::BindingKind::Normal,
                    self.span,
                )?;

                let mut field_map = UstrMap::<Span>::default();
                let mut struct_ty_fields = vec![];

                for field in st.fields.iter_mut() {
                    let res = field
                        .ty
                        .check(sess, env, Some(sess.tycx.common_types.anytype))?;
                    let ty = sess.extract_const_type(res.const_value, res.ty, field.span)?;

                    if let Some(defined_span) = field_map.insert(field.name, field.span) {
                        return Err(SyntaxError::duplicate_struct_field(
                            defined_span,
                            field.span,
                            field.name.to_string(),
                        ));
                    }

                    struct_ty_fields.push(StructTypeField {
                        symbol: field.name,
                        ty: ty.into(),
                        span: field.span,
                    });
                }

                env.pop_scope();

                sess.self_types.pop();

                let struct_ty = Type::Struct(StructType {
                    name: st.name,
                    binding_info_id: st.binding_info_id,
                    kind: st.kind,
                    fields: struct_ty_fields,
                });

                if occurs(struct_ty_var, &struct_ty, &sess.tycx) {
                    return Err(UnifyTyErr::Occurs.into_diagnostic(
                        &sess.tycx,
                        struct_ty,
                        None,
                        struct_ty_var,
                        self.span,
                    ));
                }

                let ty = sess.tycx.bound(struct_ty.clone().create_type(), self.span);
                let const_value = ConstValue::Type(sess.tycx.bound(struct_ty, self.span));

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::FunctionType(sig) => {
                let res = sig.check(sess, env, expected_ty)?;

                let ty = sess.tycx.bound(res.ty.as_kind().create_type(), self.span);
                let const_value = ConstValue::Type(res.ty);

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::SelfType => match sess.self_types.last() {
                Some(&ty) => {
                    let ty = sess.tycx.bound(ty.as_kind().create_type(), self.span);
                    let const_value = ConstValue::Type(ty);

                    *self = ast::Expr::typed(
                        ast::ExprKind::ConstValue(const_value.clone()),
                        ty,
                        self.span,
                    );

                    Ok(Res::new_const(ty, const_value))
                }
                None => Err(Diagnostic::error()
                    .with_message("`Self` is only available within struct types")
                    .with_label(Label::primary(self.span, "`Self` is invalid here"))),
            },
            ast::ExprKind::Placeholder => {
                let ty = sess.tycx.var(self.span);
                let const_value = ConstValue::Type(ty);

                *self = ast::Expr::typed(
                    ast::ExprKind::ConstValue(const_value.clone()),
                    ty,
                    self.span,
                );

                Ok(Res::new_const(ty, const_value))
            }
            ast::ExprKind::ConstValue(const_value) => const_value.check(sess, env, expected_ty),
            ast::ExprKind::Error(_) => Ok(Res::new(sess.tycx.var(self.span))),
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
        _expected_ty: Option<TypeId>,
    ) -> CheckResult {
        let callee_res = self.callee.check(sess, env, None)?;

        match callee_res.ty.normalize(&sess.tycx) {
            Type::Function(fn_ty) => {
                let arg_mismatch = match &fn_ty.varargs {
                    Some(_) if self.args.len() < fn_ty.params.len() => true,
                    None if self.args.len() != fn_ty.params.len() => true,
                    _ => false,
                };

                if arg_mismatch {
                    let expected = fn_ty.params.len();
                    let actual = self.args.len();

                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "function expects {} argument{}, but {} {} supplied",
                            expected,
                            if expected > 1 { "s" } else { "" },
                            actual,
                            if actual > 1 { "were" } else { "was" },
                        ))
                        .with_label(Label::primary(
                            self.span,
                            format!(
                                "expected {} argument{}, got {}",
                                expected,
                                if expected > 1 { "s" } else { "" },
                                actual
                            ),
                        ))
                        .with_note(format!("function is of type `{}`", fn_ty)));
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

                let inferred_fn_ty = Type::Function(FunctionType {
                    params: self.args.iter().map(|arg| arg.ty.into()).collect(),
                    ret: Box::new(return_ty.into()),
                    varargs: None,
                    kind: FunctionTypeKind::Orphan,
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
        expected_ty: Option<TypeId>,
    ) -> CheckResult {
        let res = self.expr.check(sess, env, None)?;

        self.target_ty = if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
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
        expected_ty: Option<TypeId>,
    ) -> CheckResult {
        let mut res = Res::new(sess.tycx.common_types.unit);

        env.push_scope(ScopeKind::Block);

        if !self.exprs.is_empty() {
            let last_index = self.exprs.len() - 1;

            for (i, expr) in self.exprs.iter_mut().enumerate() {
                let expected_ty = if i == last_index { expected_ty } else { None };
                res = expr.check(sess, env, expected_ty)?;
            }
        }

        // Add all defers in the current scope
        self.deferred = env
            .scope()
            .defer_stack
            .deferred
            .iter()
            .rev()
            .cloned()
            .collect();

        env.pop_scope();

        if self.yields {
            Ok(res)
        } else {
            let res_ty = res.ty.normalize(&sess.tycx);

            Ok(Res::new_const(
                if res_ty.is_never() {
                    sess.tycx.common_types.never
                } else {
                    sess.tycx.common_types.unit
                },
                ConstValue::Unit(()),
            ))
        }
    }
}

impl Check for ConstValue {
    #[inline]
    fn check(
        &mut self,
        _sess: &mut CheckSess,
        _env: &mut Env,
        _expected_ty: Option<TypeId>,
    ) -> CheckResult {
        todo!("check: const value")
    }
}

#[inline]
fn check_named_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    struct_ty: StructType,
    fields: &mut Vec<ast::StructLiteralField>,
    span: Span,
) -> CheckResult {
    let mut field_set = UstrSet::default();
    let mut uninit_fields = UstrSet::from_iter(struct_ty.fields.iter().map(|f| f.symbol));

    let mut fields_res: Vec<(Ustr, Res)> = vec![];

    for field in fields.iter_mut() {
        if !field_set.insert(field.symbol) {
            return Err(SyntaxError::struct_field_specified_more_than_once(
                field.span,
                field.symbol.to_string(),
            ));
        }

        match struct_ty.find_field(field.symbol) {
            Some(ty_field) => {
                uninit_fields.remove(&field.symbol);

                let expected_ty = sess.tycx.bound(ty_field.ty.clone(), ty_field.span);
                let res = field.expr.check(sess, env, Some(expected_ty))?;

                res.ty
                    .unify(&expected_ty, &mut sess.tycx)
                    .or_coerce_expr_into_ty(
                        &mut field.expr,
                        expected_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tycx,
                        expected_ty,
                        Some(ty_field.span),
                        res.ty,
                        field.expr.span,
                    )?;

                fields_res.push((field.symbol, res));
            }
            None => {
                return Err(TypeError::invalid_struct_field(
                    field.span,
                    field.symbol,
                    Type::Struct(struct_ty).display(&sess.tycx),
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
        let mut uninit_fields_str = uninit_fields
            .iter()
            .map(|f| f.as_str())
            .collect::<Vec<&str>>();

        uninit_fields_str.reverse();
        return Err(Diagnostic::error()
            .with_message(format!(
                "missing struct fields: {}",
                uninit_fields_str.join(", ")
            ))
            .with_label(Label::primary(span, "missing fields")));
    }

    // TODO: named and anonymous structs share the same logic for constant propogation, this could be unified to a shared function
    let is_const_struct = fields_res.iter().all(|(_, res)| res.const_value.is_some());

    if is_const_struct {
        let mut const_value_fields = IndexMap::<Ustr, ConstElement>::new();

        for (name, res) in fields_res.iter() {
            const_value_fields.insert(
                *name,
                ConstElement {
                    value: res.const_value.clone().unwrap(),
                    ty: res.ty,
                },
            );
        }

        let const_value = ConstValue::Struct(const_value_fields);

        Ok(Res::new_const(
            sess.tycx.bound(Type::Struct(struct_ty), span),
            const_value,
        ))
    } else {
        Ok(Res::new(sess.tycx.bound(Type::Struct(struct_ty), span)))
    }
}

#[inline]
fn check_anonymous_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    fields: &mut Vec<ast::StructLiteralField>,
    span: Span,
) -> CheckResult {
    // TODO: anonymous structs should support unsafe union initialization
    let mut field_set = UstrSet::default();

    let name = get_anonymous_struct_name(span);

    let mut fields_res: Vec<(Ustr, Res)> = vec![];

    let mut struct_ty = StructType {
        name,
        binding_info_id: BindingInfoId::unknown(),
        kind: StructTypeKind::Struct,
        fields: vec![],
    };

    for field in fields {
        if !field_set.insert(field.symbol) {
            return Err(SyntaxError::struct_field_specified_more_than_once(
                field.span,
                field.symbol.to_string(),
            ));
        }

        let res = field.expr.check(sess, env, None)?;

        struct_ty.fields.push(StructTypeField {
            symbol: field.symbol,
            ty: res.ty.into(),
            span: field.span,
        });

        fields_res.push((field.symbol, res));
    }

    // TODO: named and anonymous structs share the same logic for constant propogation, this could be unified to a shared function
    let is_const_struct = fields_res.iter().all(|(_, res)| res.const_value.is_some());

    if is_const_struct {
        let mut const_value_fields = IndexMap::<Ustr, ConstElement>::new();

        for (name, res) in fields_res.iter() {
            const_value_fields.insert(
                *name,
                ConstElement {
                    value: res.const_value.clone().unwrap(),
                    ty: res.ty,
                },
            );
        }

        let const_value = ConstValue::Struct(const_value_fields);

        Ok(Res::new_const(
            sess.tycx.bound(Type::Struct(struct_ty), span),
            const_value,
        ))
    } else {
        Ok(Res::new(sess.tycx.bound(Type::Struct(struct_ty), span)))
    }
}

fn get_anonymous_struct_name(span: Span) -> Ustr {
    ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
}

fn interp_expr(expr: &ast::Expr, sess: &mut CheckSess, module_id: ModuleId) -> InterpResult {
    let mut interp_sess =
        sess.interp
            .create_session(sess.workspace, &sess.tycx, &sess.new_typed_ast);
    interp_sess.eval(expr, module_id)
}