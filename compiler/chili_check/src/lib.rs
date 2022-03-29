mod bind;
mod builtin;
mod cast;
pub mod display;
mod env;
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
    ast::{self, ForeignLibrary},
    pattern::Pattern,
    ty::*,
    value::Value,
    workspace::{BindingInfoFlags, BindingInfoId, ModuleId, ScopeLevel, Workspace},
};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use env::{Env, Scope};
use std::collections::HashMap;
use top_level::CallerInfo;
use ustr::{ustr, UstrMap};

pub fn check(
    workspace: &mut Workspace,
    ast: Vec<ast::Ast>,
) -> DiagnosticResult<(ast::TypedAst, TyCtx)> {
    let mut sess = CheckSess::new(workspace, &ast);
    sess.start()?;
    Ok((sess.new_ast, sess.tycx))
}

pub(crate) type ConstBindingsMap = HashMap<BindingInfoId, Value>;

pub(crate) struct CheckSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) tycx: TyCtx,

    // The ast's being processed
    pub(crate) old_asts: &'s Vec<ast::Ast>,

    // The new typed ast being generated
    pub(crate) new_ast: ast::TypedAst,

    // Map of bindings to their const value, if there's any
    pub(crate) const_bindings: ConstBindingsMap,

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
        Self {
            workspace,
            tycx: TyCtx::new(),
            old_asts: old_ast,
            new_ast: ast::TypedAst::new(),
            const_bindings: HashMap::default(),
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

    fn add_builtin_types(&mut self) {
        let mut mk = |sess: &mut CheckSess, symbol: &str, ty: Ty| {
            let symbol = ustr(symbol);

            let id = sess.workspace.add_binding_info(
                Default::default(),
                symbol,
                ast::Visibility::Public,
                sess.tycx.bound(ty.kind().create_type()),
                false,
                ast::BindingKind::Type,
                ScopeLevel::Global,
                ustr(""),
                Span::unknown(),
            );

            let info = sess.workspace.get_binding_info_mut(id).unwrap();
            info.flags.insert(BindingInfoFlags::BUILTIN_TYPE);

            sess.const_bindings.insert(id, Value::Type(ty));
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
                        return Err(TypeError::expected(node.span, ty.to_string(), "a module"));
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

        let binding_ty = if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.check(sess, env, None)?;
            sess.extract_const_type(res.const_value, res.ty, ty_expr.span)?
        } else {
            sess.tycx.var()
        };

        let const_value = if let Some(expr) = &mut self.expr {
            let res = expr.check(sess, env, Some(binding_ty))?;

            res.ty
                .unify(&binding_ty, sess)
                .map_err(|e| map_unify_err(e, binding_ty, res.ty, expr.span, &sess.tycx))?;

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

            res.const_value
        } else {
            None
        };

        sess.bind_pattern(
            env,
            &mut self.pattern,
            self.visibility,
            binding_ty,
            self.kind,
        )?;

        match &self.pattern {
            Pattern::Single(pat) => {
                // don't allow const values with mutable bindings or patterns
                // that are not `Single` and are not mutable
                if let Some(const_value) = const_value {
                    if !pat.is_mutable {
                        sess.const_bindings.insert(pat.binding_info_id, const_value);
                    }
                }
            }
            _ => (),
        }

        Ok(Res::new_maybe_const(binding_ty, const_value))
    }
}

impl Check for ast::Fn {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let res = self.sig.check(sess, env, expected_ty)?;

        let fn_ty = sess.tycx.ty_kind(res.ty);
        let fn_ty = fn_ty.as_fn();

        let return_ty = sess.tycx.bound(fn_ty.ret.as_ref().clone());

        env.push_named_scope(self.sig.name);

        for (param, param_ty) in self.sig.params.iter_mut().zip(fn_ty.params.iter()) {
            let ty = sess.tycx.bound(param_ty.ty.clone());
            sess.bind_pattern(
                env,
                &mut param.pattern,
                ast::Visibility::Private,
                ty,
                ast::BindingKind::Let,
            )?;
        }

        let body_res = sess.with_function_frame(
            FunctionFrame {
                return_ty,
                scope_level: env.scope_level(),
            },
            |sess| self.body.check(sess, env, None),
        )?;

        body_res
            .ty
            .unify(&return_ty, sess)
            .map_err(|e| map_unify_err(e, return_ty, body_res.ty, self.body.span, &sess.tycx))?;

        env.pop_scope();

        Ok(Res::new(res.ty))
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

        for param in self.params.iter_mut() {
            let ty = if let Some(expr) = &mut param.ty {
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
                ty: ty.into(),
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
            ast::ExprKind::Assign { lvalue, rvalue } => todo!(),
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
            ast::ExprKind::While { cond, block } => {
                let cond_ty = sess.tycx.common_types.bool;
                let cond_res = cond.check(sess, env, Some(cond_ty))?;

                cond_res
                    .ty
                    .unify(&cond_ty, sess)
                    .map_err(|e| map_unify_err(e, cond_ty, cond_res.ty, cond.span, &sess.tycx))?;

                sess.loop_depth += 1;
                block.check(sess, env, None)?;
                sess.loop_depth -= 1;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::For(for_) => {
                let iter_ty = match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        let start_res = start.check(sess, env, None)?;
                        let end_res = end.check(sess, env, None)?;

                        start_res.ty.unify(&end_res.ty, sess).map_err(|e| {
                            map_unify_err(e, start_res.ty, end_res.ty, end.span, &sess.tycx)
                        })?;

                        let start_ty = start_res.ty.normalize(&sess.tycx);
                        let end_ty = end_res.ty.normalize(&sess.tycx);

                        if !start_ty.is_any_integer() {
                            return Err(TypeError::expected(
                                start.span,
                                start_ty.to_string(),
                                "any integer",
                            ));
                        }

                        if !end_ty.is_any_integer() {
                            return Err(TypeError::expected(
                                end.span,
                                end_ty.to_string(),
                                "any integer",
                            ));
                        }

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
                    false,
                    ast::BindingKind::Let,
                    self.span, // TODO: use iter's actual span
                )?;

                sess.bind_symbol(
                    env,
                    for_.iter_index_name,
                    ast::Visibility::Private,
                    sess.tycx.common_types.uint,
                    false,
                    ast::BindingKind::Let,
                    self.span, // TODO: use iter_index's actual span
                )?;

                sess.loop_depth += 1;
                for_.block.check(sess, env, None)?;
                sess.loop_depth -= 1;

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::ExprKind::Break { deferred } => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "break"));
                }

                for expr in deferred.iter_mut() {
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Continue { deferred } => {
                if sess.loop_depth == 0 {
                    return Err(SyntaxError::outside_of_loop(self.span, "continue"));
                }

                for expr in deferred.iter_mut() {
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::Return { expr, deferred } => {
                let function_frame = sess
                    .function_frame()
                    .ok_or(SyntaxError::outside_of_function(self.span, "return"))?;

                if let Some(expr) = expr {
                    let expected = function_frame.return_ty;
                    let res = expr.check(sess, env, Some(expected))?;
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
                    expr.check(sess, env, None)?;
                }

                Ok(Res::new(sess.tycx.common_types.never))
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_ty = sess.tycx.common_types.bool;
                let cond_res = cond.check(sess, env, Some(cond_ty))?;

                cond_res
                    .ty
                    .unify(&cond_ty, sess)
                    .map_err(|e| map_unify_err(e, cond_ty, cond_res.ty, cond.span, &sess.tycx))?;

                let then_res = then_expr.check(sess, env, expected_ty)?;

                if let Some(else_expr) = else_expr {
                    let else_res = else_expr.check(sess, env, Some(then_res.ty))?;

                    else_res.ty.unify(&then_res.ty, sess).map_err(|e| {
                        map_unify_err(e, then_res.ty, else_res.ty, else_expr.span, &sess.tycx)
                    })?;

                    Ok(Res::new(then_res.ty))
                } else {
                    Ok(Res::new(sess.tycx.common_types.unit))
                }
            }
            ast::ExprKind::Block(block) => block.check(sess, env, expected_ty),
            ast::ExprKind::Binary(binary) => binary.check(sess, env, expected_ty),
            ast::ExprKind::Unary(unary) => unary.check(sess, env, expected_ty),
            ast::ExprKind::Subscript { expr, index } => todo!(),
            ast::ExprKind::Slice { expr, low, high } => todo!(),
            ast::ExprKind::FnCall(call) => call.check(sess, env, expected_ty),
            ast::ExprKind::MemberAccess { expr, member } => {
                let res = expr.check(sess, env, None)?;
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
                        let (res, _) = sess.check_top_level_symbol(
                            CallerInfo {
                                module_id: env.module_id(),
                                span: self.span,
                            },
                            *module_id,
                            *member,
                        )?;
                        Ok(res)
                    }
                    ty => Err(TypeError::member_access_on_invalid_type(
                        expr.span,
                        ty.to_string(),
                    )),
                }
            }
            ast::ExprKind::Ident {
                symbol,
                binding_info_id,
                ..
            } => match env.find_symbol(*symbol) {
                Some(id) => {
                    // this is a local binding
                    *binding_info_id = id;
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
                            .with_message("can't capture dynamic environment yet - not implemented")
                            .with_labels(vec![Label::primary(
                                self.span.file_id,
                                self.span.range(),
                            )]));
                    }

                    Ok(sess
                        .workspace
                        .get_binding_info(id)
                        .map(|binding_info| {
                            Res::new_maybe_const(
                                binding_info.ty,
                                sess.const_bindings.get(&id).cloned(),
                            )
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
                        *symbol,
                    )?;

                    *binding_info_id = id;
                    sess.workspace.increment_binding_use(id);

                    // TODO: check visibility
                    Ok(res)
                }
            },
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                let mut field_map = UstrMap::default();

                for field in fields.iter_mut() {
                    // TODO: check field

                    if let Some(already_defined_span) = field_map.insert(field.symbol, field.span) {
                        return Err(SyntaxError::duplicate_symbol(
                            already_defined_span,
                            field.span,
                            field.symbol,
                        ));
                    }
                }

                todo!()
            }
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
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::ArrayType(_, _) => todo!(),
            ast::ExprKind::SliceType(inner, is_mutable) => {
                let res = inner.check(sess, env, None)?;
                let inner_kind = sess.extract_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Slice(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::StructType(st) => {
                let mut field_map = UstrMap::default();

                for field in st.fields.iter_mut() {
                    // TODO: check field

                    if let Some(already_defined_span) = field_map.insert(field.name, field.span) {
                        return Err(SyntaxError::duplicate_symbol(
                            already_defined_span,
                            field.span,
                            field.name,
                        ));
                    }
                }

                todo!()
            }
            ast::ExprKind::FnType(sig) => sig.check(sess, env, expected_ty),
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
                            let res = arg.expr.check(sess, env, Some(param_ty))?;

                            res.ty.unify(&param_ty, sess).map_err(|e| {
                                map_unify_err(e, param_ty, res.ty, arg.expr.span, &sess.tycx)
                            })?;

                            // TODO: coerce
                            // TODO: self.infcx
                            // TODO:     .unify_or_coerce_ty_expr(&param_ty, &mut arg.expr)?;
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

                ty.unify(&inferred_fn_ty, sess).map_err(|e| {
                    map_unify_err(e, inferred_fn_ty, ty, self.callee.span, &sess.tycx)
                })?;

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

impl Check for ast::Binary {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        let mut lhs_res = self.lhs.check(sess, env, expected_ty)?;
        let mut rhs_res = self.rhs.check(sess, env, expected_ty)?;

        // let rhs_span = rhs.expr.span;
        lhs_res
            .ty
            .unify(&rhs_res.ty, sess)
            .map_err(|e| map_unify_err(e, lhs_res.ty, rhs_res.ty, self.rhs.span, &sess.tycx))?;

        let ty_kind = lhs_res.ty.normalize(&sess.tycx);

        match &self.op {
            ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Rem
            | ast::BinaryOp::Lt
            | ast::BinaryOp::LtEq
            | ast::BinaryOp::Gt
            | ast::BinaryOp::GtEq => {
                if !ty_kind.is_number() {
                    return Err(TypeError::expected(
                        self.span,
                        ty_kind.display(&sess.tycx),
                        "a number",
                    ));
                }
            }

            ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitwiseOr
            | ast::BinaryOp::BitwiseXor
            | ast::BinaryOp::BitwiseAnd => {
                if !ty_kind.is_any_integer() {
                    return Err(TypeError::expected(
                        self.span,
                        ty_kind.display(&sess.tycx),
                        "any integer",
                    ));
                }
            }

            ast::BinaryOp::Eq | ast::BinaryOp::NEq => (),

            ast::BinaryOp::And | ast::BinaryOp::Or => {
                if !ty_kind.is_bool() {
                    return Err(TypeError::type_mismatch(
                        self.span,
                        sess.tycx.common_types.bool.display(&sess.tycx),
                        ty_kind.display(&sess.tycx),
                    ));
                }
            }
        };

        let result_ty = match &self.op {
            ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Rem
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitwiseOr
            | ast::BinaryOp::BitwiseXor
            | ast::BinaryOp::BitwiseAnd => self.lhs.ty,

            ast::BinaryOp::Eq
            | ast::BinaryOp::NEq
            | ast::BinaryOp::Lt
            | ast::BinaryOp::LtEq
            | ast::BinaryOp::Gt
            | ast::BinaryOp::GtEq
            | ast::BinaryOp::And
            | ast::BinaryOp::Or => sess.tycx.common_types.bool,
        };

        Ok(Res::new(result_ty))
    }
}

impl Check for ast::Unary {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> CheckResult {
        todo!()
    }
}

impl Check for ast::Literal {
    fn check(
        &mut self,
        sess: &mut CheckSess,
        env: &mut Env,
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
