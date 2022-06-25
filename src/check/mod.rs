mod bind;
mod builtin;
mod const_fold;
mod env;
mod top_level;

use crate::interp::interp::{Interp, InterpResult};
use crate::span::Span;
use crate::{
    ast::ty::align::AlignOf,
    infer::{
        cast::CanCast,
        coerce::{OrCoerce, OrCoerceIntoTy},
        display::{DisplayTy, OrReportErr},
        normalize::Normalize,
        substitute::substitute,
        ty_ctx::TyCtx,
        unify::{occurs, UnifyTy, UnifyTyErr},
    },
};
use crate::{
    ast::ty::size::SizeOf,
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError, TypeError,
    },
};
use crate::{
    ast::{
        self,
        pattern::{HybridPattern, Pattern, SymbolPattern, UnpackPatternKind},
        ty::{
            FunctionType, FunctionTypeKind, FunctionTypeVarargs, InferTy, PartialStructType,
            StructType, StructTypeField, StructTypeKind, Type, TypeId,
        },
        workspace::{
            BindingId, BindingInfoFlags, ModuleId, PartialBindingInfo, ScopeLevel, Workspace,
        },
        BindingKind, FunctionId,
    },
    hir::{
        self,
        const_value::{ConstArray, ConstElement, ConstFunction, ConstValue},
    },
};
use crate::{
    common::{
        builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
        target::TargetMetrics,
    },
    infer::cast::can_cast_type,
};
use env::{Env, Scope, ScopeKind};
use indexmap::IndexMap;
use std::{collections::HashMap, iter::repeat_with};
use top_level::CallerInfo;
use ustr::{ustr, Ustr, UstrMap, UstrSet};

pub type CheckData = (ast::TypedAst, hir::Cache, TyCtx);

pub fn check(workspace: &mut Workspace, module: Vec<ast::Module>) -> CheckData {
    let mut sess = CheckSess::new(workspace, &module);

    if let Err(diag) = sess.start() {
        sess.workspace.diagnostics.push(diag);
        return sess.into_data();
    }

    if sess.workspace.diagnostics.has_errors() {
        return sess.into_data();
    }

    substitute(
        &mut sess.workspace.diagnostics,
        &mut sess.tycx,
        &sess.typed_ast,
    );

    for binding in sess
        .typed_ast
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
                        binding.type_expr.as_ref().unwrap().span(),
                        "cannot be used in extern context",
                    )),
            )
        }
    }

    sess.into_data()
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
    pub modules: &'s Vec<ast::Module>,

    // The new typed ast being generated
    pub typed_ast: ast::TypedAst,
    pub cache: hir::Cache,
    pub checked_modules: HashMap<ModuleId, TypeId>,

    // Information that's relevant for the global context
    pub global_scopes: HashMap<ModuleId, Scope>,
    pub builtin_types: UstrMap<BindingId>,

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
    pub fn new(workspace: &'s mut Workspace, old_asts: &'s Vec<ast::Module>) -> Self {
        let target_metrics = workspace.build_options.target_platform.metrics();
        let interp = Interp::new(workspace.build_options.clone());

        Self {
            workspace,
            target_metrics,
            interp,
            tycx: TyCtx::default(),
            modules: old_asts,
            typed_ast: ast::TypedAst::default(),
            cache: hir::Cache::new(),
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

        for module in self.modules.iter() {
            self.check_module(module)?;
        }

        Ok(())
    }

    fn into_data(self) -> CheckData {
        (self.typed_ast, self.cache, self.tycx)
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
        let module_info = *self.workspace.module_infos.get(module_id).unwrap();
        f(self, Env::new(module_id, module_info))
    }

    pub fn function_frame(&self) -> Option<FunctionFrame> {
        self.function_frames.last().map(|&f| f)
    }

    pub fn extract_const_type(&self, node: &hir::Node) -> DiagnosticResult<TypeId> {
        match node.as_const_value() {
            Some(ConstValue::Type(t)) => Ok(*t),
            _ => Err(TypeError::expected(
                node.span(),
                node.ty().display(&self.tycx),
                "a type",
            )),
        }
    }

    pub fn extract_const_int(&self, node: &hir::Node) -> DiagnosticResult<i64> {
        match node.as_const_value() {
            Some(ConstValue::Int(v)) => Ok(*v),
            _ => Err(TypeError::expected(
                node.span(),
                node.ty().display(&self.tycx),
                "compile-time known integer",
            )),
        }
    }

    fn add_builtin_types(&mut self) {
        let mk = |sess: &mut CheckSess, symbol: &str, ty: TypeId| {
            let symbol = ustr(symbol);

            let partial_binding_info = PartialBindingInfo {
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
            };

            let id = sess
                .workspace
                .binding_infos
                .insert_with_id(partial_binding_info.into_binding_info());

            let info = sess.workspace.binding_infos.get_mut(id).unwrap();
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

    pub(super) fn is_mutable(&self, node: &hir::Node) -> bool {
        match node {
            hir::Node::MemberAccess(access) => self.is_mutable(&access.value),
            hir::Node::Id(id) => self.workspace.binding_infos.get(id.id).unwrap().is_mutable,
            _ => true,
        }
    }

    fn id_or_const(&self, id: BindingId, span: Span) -> hir::Node {
        let binding_info = self.workspace.binding_infos.get(id).unwrap();
        let ty = binding_info.ty;

        match &binding_info.const_value {
            Some(value) => hir::Node::Const(hir::Const {
                value: value.clone(),
                ty,
                span,
            }),
            None => hir::Node::Id(hir::Id { id, ty, span }),
        }
    }
}

type Result<T = hir::Node> = DiagnosticResult<T>;

pub trait Check
where
    Self: Sized,
{
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result;
}

impl Check for ast::Binding {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_ty: Option<TypeId>) -> Result {
        let ty = check_optional_type_expr(&self.type_expr, sess, env, self.pattern.span())?;

        match &self.kind {
            BindingKind::Normal => {
                let mut value_node = self.value.check(sess, env, Some(ty))?;

                value_node
                    .ty()
                    .unify(&ty, &mut sess.tycx)
                    .or_coerce_into_ty(
                        &mut value_node,
                        ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tycx,
                        ty,
                        self.type_expr.as_ref().map(|e| e.span()),
                        value_node.ty(),
                        self.value.span(),
                    )?;

                let ty_kind = ty.normalize(&sess.tycx);

                let is_type_or_module = ty_kind.is_type() || ty_kind.is_module();
                let is_any_pattern_mut = self.pattern.iter().any(|p| p.is_mutable);

                // Global immutable bindings must resolve to a const value, unless it is:
                // - of type `type` or `module`
                // - an extern binding
                if env.scope_level().is_global()
                    && !is_type_or_module
                    && !is_any_pattern_mut
                    && !matches!(
                        self.kind,
                        BindingKind::Extern(_) | BindingKind::Intrinsic(_)
                    )
                {
                    return Err(Diagnostic::error()
                        .with_message(format!("immutable top level binding must be constant"))
                        .with_label(Label::primary(self.pattern.span(), "must be constant"))
                        .with_label(Label::secondary(
                            value_node.span(),
                            "doesn't resolve to a constant value",
                        )));
                }

                // Bindings of type `type` and `module` cannot be assigned to mutable bindings
                if is_type_or_module {
                    self.pattern
                        .iter()
                        .filter(|pat| pat.is_mutable)
                        .for_each(|pat| {
                            sess.workspace.diagnostics.push(
                                Diagnostic::error()
                                    .with_message(
                                        "variable of type `type` or `module` must be immutable",
                                    )
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
                    value_node.as_const_value().cloned(),
                    &self.kind,
                    value_node.span(),
                )?;

                // If this binding matches the entry point function's requirements,
                // Tag it as the entry function
                // Requirements:
                // - Is declared in the root module
                // - It is in global scope
                // - Its name is the same as the required `entry_point_function_name`
                if let Some(ConstValue::Function(_)) = value_node.as_const_value() {
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

                Ok(hir::Node::Binding(hir::Binding {
                    ty,
                    span: self.span,
                    module_id: env.module_id(),
                    id: todo!(),
                    name: todo!(),
                    value: Box::new(value_node),
                }))
            }
            BindingKind::Intrinsic(intrinsic) => {
                let pattern = self.pattern.as_symbol_mut();
                let name = pattern.symbol;

                todo!()
                // let id = sess.typed_ast.functions.insert_with_id(ast::Function {
                //     id: FunctionId::unknown(),
                //     module_id: env.module_id(),
                //     kind: ast::FunctionKind::Intrinsic(*intrinsic),
                //     ty,
                //     span: self.span,
                // });

                // let const_value = ConstValue::Function(ConstFunction { id, name });

                // sess.bind_symbol_pattern(
                //     env,
                //     pattern,
                //     self.visibility,
                //     ty,
                //     Some(const_value.clone()),
                //     &self.kind,
                // )?;

                // self.value = Some(Box::new(ast::Ast::Const(ast::Const {
                //     value: const_value.clone(),
                //     ty: self.ty,
                //     span: pattern.span,
                // })));

                // Ok(Res::new(ty))
            }
            BindingKind::Extern(lib) => {
                if let Some(lib) = lib {
                    // Collect extern library to be linked later
                    sess.workspace.extern_libraries.insert(lib.clone());
                }

                let pattern = self.pattern.as_symbol_mut();
                let name = pattern.symbol;

                todo!()

                // let id = sess.typed_ast.functions.insert_with_id(ast::Function {
                //     id: FunctionId::unknown(),
                //     module_id: env.module_id(),
                //     kind: ast::FunctionKind::Extern {
                //         name,
                //         lib: lib.clone(),
                //     },
                //     tyty,
                //     span: self.span,
                // });

                // let const_value = ConstValue::Function(ConstFunction { id, name });

                // sess.bind_symbol_pattern(
                //     env,
                //     pattern,
                //     self.visibility,
                //     ty,
                //     Some(const_value.clone()),
                //     &self.kind,
                // )?;

                // self.value = Some(Box::new(ast::Ast::Const(ast::Const {
                //     value: const_value.clone(),
                //     ty: self.ty,
                //     span: pattern.span,
                // })));

                // Ok(Res::new_const(ty, const_value))
            }
        }
    }
}

impl Check for ast::FunctionSig {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let mut param_types: Vec<Type> = vec![];

        if !self.params.is_empty() {
            let mut defined_params = UstrMap::default();

            for param in self.params.iter() {
                let param_type =
                    check_optional_type_expr(&param.type_expr, sess, env, param.pattern.span())?;

                for pattern in param.pattern.iter() {
                    if let Some(already_defined_span) =
                        defined_params.insert(pattern.symbol, pattern.span)
                    {
                        return Err(SyntaxError::duplicate_symbol(
                            already_defined_span,
                            pattern.span,
                            pattern.symbol,
                        ));
                    }
                }

                param_types.push(param_type.as_kind());
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
                                type_expr: None,
                                ty: sess.tycx.bound(f.params[0].clone(), self.span),
                            });

                            param_types.push(f.params[0].clone());
                        }
                    }
                    _ => (),
                }
            }
        };

        let return_type = if self.return_type.is_none() && self.kind.is_extern() {
            sess.tycx.common_types.unit
        } else {
            check_optional_type_expr(&self.return_type, sess, env, self.span)?
        };

        let varargs = if let Some(varargs) = &mut self.varargs {
            let ty = if varargs.type_expr.is_some() {
                let ty = check_optional_type_expr(&varargs.type_expr, sess, env, self.span)?;
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

        let function_type = sess.tycx.bound(
            Type::Function(FunctionType {
                params: param_types,
                ret: Box::new(return_type.into()),
                varargs,
                kind: self.kind.clone(),
            }),
            self.span,
        );

        Ok(hir::Node::Const(hir::Const {
            value: ConstValue::Type(function_type),
            ty: sess
                .tycx
                .bound(function_type.as_kind().create_type(), self.span),
            span: self.span,
        }))
    }
}

impl Check for ast::Ast {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        match self {
            ast::Ast::Binding(binding) => binding.check(sess, env, None),
            ast::Ast::Assignment(assignment) => assignment.check(sess, env, expected_ty),
            ast::Ast::Cast(cast) => cast.check(sess, env, expected_ty),
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::Import(import_path) => {
                    let path_str = import_path.to_str().unwrap();

                    let module = sess
                        .modules
                        .iter()
                        .find(|m| m.module_info.file_path == path_str)
                        .unwrap_or_else(|| {
                            panic!("couldn't find ast for module with path: {}", path_str)
                        });

                    let module_type = sess.check_module(module)?;

                    Ok(hir::Node::Const(hir::Const {
                        value: ConstValue::Unit(()),
                        ty: module_type,
                        span: builtin.span,
                    }))
                }
                ast::BuiltinKind::SizeOf(expr) => {
                    let ty = check_type_expr(&expr, sess, env, expr.span())?;

                    let size = ty
                        .normalize(&sess.tycx)
                        .size_of(sess.target_metrics.word_size);

                    Ok(hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(size as _),
                        ty: sess.tycx.common_types.uint,
                        span: expr.span(),
                    }))
                }
                ast::BuiltinKind::AlignOf(expr) => {
                    let ty = check_type_expr(&expr, sess, env, expr.span())?;

                    let align = ty
                        .normalize(&sess.tycx)
                        .align_of(sess.target_metrics.word_size);

                    Ok(hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(align as _),
                        ty: sess.tycx.common_types.uint,
                        span: expr.span(),
                    }))
                }
                ast::BuiltinKind::Run(expr) => {
                    todo!("interpret the expression and return the result as a hir::Node::Const")
                    // let node = expr.check(sess, env, None)?;

                    // if sess.workspace.build_options.check_mode {
                    //     Ok(Res::new(node.ty))
                    // } else {
                    //     // TODO (Ron): unwrap interp result into a diagnostic
                    //     let interp_value = interp_expr(&expr, sess, env.module_id()).unwrap();

                    //     let ty = node.ty.normalize(&sess.tycx);

                    //     match interp_value.try_into_const_value(&mut sess.tycx, &ty, builtin.span) {
                    //         Ok(const_value) => {
                    //             *run_result = Some(const_value.clone());
                    //             Ok(Res::new_const(node.ty, const_value))
                    //         }
                    //         Err(value_str) => Err(Diagnostic::error()
                    //             .with_message(format!(
                    //                 "compile-time evaluation cannot result in `{}`",
                    //                 value_str,
                    //             ))
                    //             .with_label(Label::primary(builtin.span, "evaluated here"))),
                    //     }
                    // }
                }
                ast::BuiltinKind::Panic(expr) => {
                    todo!("replace this with `pub let {{ default_panic_handler: panic }} = import!(\"panicking\");")
                    // if let Some(expr) = expr {
                    //     expr.check(sess, env, None)?;
                    // }

                    // Ok(Res::new(sess.tycx.common_types.unit))
                }
            },
            ast::Ast::Function(function) => function.check(sess, env, expected_ty),
            ast::Ast::While(while_) => while_.check(sess, env, expected_ty),
            ast::Ast::For(for_) => {
                let iter_ty = match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        let start_node = start.check(sess, env, None)?;
                        let end_node = end.check(sess, env, None)?;

                        let anyint = sess.tycx.anyint(start.span());

                        start_node.ty.unify(&anyint, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            anyint,
                            None,
                            start_node.ty,
                            start.span(),
                        )?;

                        start_node
                            .ty
                            .unify(&end_node.ty, &mut sess.tycx)
                            .or_coerce(start, end, &mut sess.tycx, sess.target_metrics.word_size)
                            .or_report_err(
                                &sess.tycx,
                                start_node.ty,
                                Some(start.span()),
                                end_node.ty,
                                end.span(),
                            )?;

                        start_node.ty
                    }
                    ast::ForIter::Value(value) => {
                        let node = value.check(sess, env, None)?;
                        let ty = node.ty.normalize(&sess.tycx);

                        match ty.maybe_deref_once() {
                            Type::Array(inner, ..) | Type::Slice(inner, ..) => {
                                sess.tycx.bound(*inner, value.span())
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!("can't iterate over `{}`", ty))
                                    .with_label(Label::primary(value.span(), "can't iterate")));
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
                    for_.span, // TODO: use iter's actual span
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
                        for_.span, // TODO: use iter_index's actual span
                    )?;
                }

                for_.block.check(sess, env, None)?;

                sess.loop_depth -= 1;
                env.pop_scope();

                Ok(Res::new(sess.tycx.common_types.unit))
            }
            ast::Ast::Break(term) if sess.loop_depth == 0 => {
                Err(SyntaxError::outside_of_loop(term.span, "break"))
            }
            ast::Ast::Continue(term) if sess.loop_depth == 0 => {
                Err(SyntaxError::outside_of_loop(term.span, "continue"))
            }
            ast::Ast::Break(term) | ast::Ast::Continue(term) => Ok(hir::Node::Const(hir::Const {
                value: ConstValue::Unit(()),
                ty: sess.tycx.common_types.never,
                span: term.span,
            })),
            ast::Ast::Return(return_) => return_.check(sess, env, expected_ty),
            ast::Ast::If(if_) => if_.check(sess, env, expected_ty),
            ast::Ast::Block(block) => block.check(sess, env, expected_ty),
            ast::Ast::Binary(binary) => binary.check(sess, env, expected_ty),
            ast::Ast::Unary(unary) => unary.check(sess, env, expected_ty),
            ast::Ast::Subscript(sub) => {
                let uint = sess.tycx.common_types.uint;

                let mut offset_node = sub.index.check(sess, env, None)?;

                offset_node
                    .ty()
                    .unify(&uint, &mut sess.tycx)
                    .or_coerce_into_ty(
                        &mut offset_node,
                        uint,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tycx, uint, None, offset_node.ty(), sub.index.span())?;

                let node = sub.expr.check(sess, env, None)?;
                let node_type = node.ty().normalize(&sess.tycx);
                let node_type_deref = node_type.maybe_deref_once();

                let const_value = if let Some(ConstValue::Int(const_index)) =
                    offset_node.as_const_value()
                {
                    let const_index = *const_index;

                    if const_index < 0 {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "index out of array bounds - expected a positive index, found {}",
                                const_index
                            ))
                            .with_label(Label::primary(sub.index.span(), "index out of bounds")));
                    }

                    // compile-time array bounds check
                    if let Type::Array(_, size) = node_type_deref {
                        if const_index >= size as _ {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "index out of array bounds - expected 0 to {}, but found {}",
                                    size - 1,
                                    const_index
                                ))
                                .with_label(Label::primary(
                                    sub.index.span(),
                                    "index out of bounds",
                                )));
                        }
                    }

                    if let Some(ConstValue::Array(const_array)) = node.as_const_value() {
                        Some(const_array.values[const_index as _].clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                match node_type_deref {
                    Type::Array(inner, ..)
                    | Type::Slice(inner, ..)
                    | Type::MultiPointer(inner, ..) => {
                        let ty = sess.tycx.bound(*inner, sub.span);

                        if let Some(const_value) = const_value {
                            Ok(hir::Node::Const(hir::Const {
                                value: const_value,
                                ty,
                                span: sub.span,
                            }))
                        } else {
                            Ok(hir::Node::Builtin(hir::Builtin::Offset(hir::Offset {
                                ty,
                                span: sub.span,
                                value: Box::new(node),
                                offset: Box::new(offset_node),
                            })))
                        }
                    }
                    _ => Err(Diagnostic::error()
                        .with_message(format!(
                            "cannot index type `{}`",
                            node_type.display(&sess.tycx)
                        ))
                        .with_label(Label::primary(sub.expr.span(), "cannot index"))),
                }
            }
            ast::Ast::Slice(slice) => {
                let uint = sess.tycx.common_types.uint;

                let node = slice.expr.check(sess, env, None)?;
                let expr_ty = node.ty().normalize(&sess.tycx);

                let low = if let Some(low) = &mut slice.low {
                    let low = low.check(sess, env, None)?;

                    low.ty()
                        .unify(&uint, &mut sess.tycx)
                        .or_coerce_into_ty(
                            &mut low,
                            uint,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tycx, uint, None, low.ty(), low.span())?;

                    low
                } else {
                    hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(0),
                        ty: uint,
                        span: slice.span,
                    })
                };

                let high = if let Some(high) = &mut slice.high {
                    let mut high = high.check(sess, env, None)?;

                    high.ty()
                        .unify(&uint, &mut sess.tycx)
                        .or_coerce_into_ty(
                            &mut high,
                            uint,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tycx,
                            uint,
                            slice.low.as_ref().map(|e| e.span()),
                            high.ty(),
                            high.span(),
                        )?;

                    high
                } else {
                    match &expr_ty {
                        Type::Array(_, size) => hir::Node::Const(hir::Const {
                            value: ConstValue::Uint(*size as _),
                            ty: uint,
                            span: slice.span,
                        }),
                        Type::Slice(..) => hir::Node::MemberAccess(hir::MemberAccess {
                            value: Box::new(node),
                            ty: uint,
                            span: slice.span,
                            member: ustr(BUILTIN_FIELD_LEN),
                            index: 1,
                        }),
                        Type::MultiPointer(..) => {
                            return Err(Diagnostic::error()
                                .with_message(
                                    "slicing a multi-pointer requires specifying the end index",
                                )
                                .with_label(Label::primary(slice.span, "must specify end index")))
                        }
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot slice type `{}`",
                                    expr_ty.display(&sess.tycx)
                                ))
                                .with_label(Label::primary(slice.expr.span(), "cannot slice")))
                        }
                    }
                };

                let (result_ty, is_mutable) = match expr_ty {
                    Type::Array(inner, ..) => (inner, sess.is_mutable(&node)),
                    Type::Slice(inner, is_mutable) | Type::MultiPointer(inner, is_mutable) => {
                        (inner, is_mutable)
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "cannot slice type `{}`",
                                expr_ty.display(&sess.tycx)
                            ))
                            .with_label(Label::primary(slice.expr.span(), "cannot slice")))
                    }
                };

                let ty = sess
                    .tycx
                    .bound(Type::Slice(result_ty, is_mutable), slice.span);

                Ok(hir::Node::Builtin(hir::Builtin::Slice(hir::Slice {
                    ty,
                    span: slice.span,
                    value: Box::new(node),
                    low: Box::new(low),
                    high: Box::new(high),
                })))
            }
            ast::Ast::Call(call) => call.check(sess, env, expected_ty),
            ast::Ast::MemberAccess(access) => {
                let node = access.expr.check(sess, env, None)?;

                let member_tuple_index = access.member.as_str().parse::<usize>();

                // if the accessed expression's type is not resolved yet - try unifying it with a partial type
                match node.ty().normalize(&sess.tycx) {
                    Type::Var(_)
                    | Type::Infer(_, InferTy::PartialStruct(_))
                    | Type::Infer(_, InferTy::PartialTuple(_)) => {
                        // if this parsing operation succeeds, this is a tuple member access - `tup.0`
                        // otherwise, this is a struct field access - `strct.field`

                        let member_ty = sess.tycx.var(access.span);
                        let partial_ty = match member_tuple_index {
                            Ok(index) => {
                                let elements = repeat_with(|| sess.tycx.var(access.span))
                                    .take(index + 1)
                                    .map(Type::Var)
                                    .collect::<Vec<Type>>();
                                sess.tycx.partial_tuple(elements, access.span)
                            }
                            Err(_) => {
                                let fields =
                                    IndexMap::from([(access.member, Type::Var(member_ty))]);
                                sess.tycx
                                    .partial_struct(PartialStructType(fields), access.span)
                            }
                        };

                        node.ty().unify(&partial_ty, &mut sess.tycx).or_report_err(
                            &sess.tycx,
                            partial_ty,
                            None,
                            node.ty(),
                            access.expr.span(),
                        )?;
                    }
                    _ => (),
                }

                let node_type = node.ty().normalize(&sess.tycx);
                let node_type_deref = node_type.maybe_deref_once();

                let new_node = match &node_type_deref {
                    ty @ Type::Tuple(elements)
                    | ty @ Type::Infer(_, InferTy::PartialTuple(elements)) => {
                        match member_tuple_index {
                            Ok(index) => match elements.get(index) {
                                Some(field_ty) => {
                                    let ty = sess.tycx.bound(field_ty.clone(), access.span);

                                    if let Some(ConstValue::Tuple(const_elements)) =
                                        node.as_const_value()
                                    {
                                        hir::Node::Const(hir::Const {
                                            value: const_elements[index].value.clone(),
                                            ty,
                                            span: access.span,
                                        })
                                    } else {
                                        // TODO: The index here *could be wrong*.
                                        // TODO: We need to test this to make sure there aren't messing anything here
                                        hir::Node::MemberAccess(hir::MemberAccess {
                                            ty,
                                            span: access.span,
                                            value: Box::new(node),
                                            member: access.member,
                                            index: index as _,
                                        })
                                    }
                                }
                                None => {
                                    return Err(TypeError::tuple_field_out_of_bounds(
                                        access.expr.span(),
                                        &access.member,
                                        ty.display(&sess.tycx),
                                        elements.len() - 1,
                                    ))
                                }
                            },
                            Err(_) => {
                                return Err(TypeError::non_numeric_tuple_field(
                                    access.expr.span(),
                                    &access.member,
                                    ty.display(&sess.tycx),
                                ))
                            }
                        }
                    }
                    ty @ Type::Struct(st) => match st.find_field_full(access.member) {
                        Some((index, field)) => {
                            let ty = sess.tycx.bound(field.ty.clone(), access.span);

                            if let Some(ConstValue::Struct(const_fields)) = node.as_const_value() {
                                hir::Node::Const(hir::Const {
                                    value: const_fields[&field.symbol].value.clone(),
                                    ty,
                                    span: access.span,
                                })
                            } else {
                                // TODO: The index here *could be wrong*.
                                // TODO: We need to test this to make sure there aren't messing anything here
                                hir::Node::MemberAccess(hir::MemberAccess {
                                    ty,
                                    span: access.span,
                                    value: Box::new(node),
                                    member: access.member,
                                    index: index as _,
                                })
                            }
                        }
                        None => {
                            return Err(TypeError::invalid_struct_field(
                                access.expr.span(),
                                access.member,
                                ty.display(&sess.tycx),
                            ))
                        }
                    },
                    ty @ Type::Infer(_, InferTy::PartialStruct(partial_struct)) => {
                        match partial_struct.get_full(&access.member) {
                            Some((index, _, field_ty)) => {
                                let ty = sess.tycx.bound(field_ty.clone(), access.span);

                                if let Some(ConstValue::Struct(const_fields)) =
                                    node.as_const_value()
                                {
                                    hir::Node::Const(hir::Const {
                                        value: const_fields[&access.member].value.clone(),
                                        ty,
                                        span: access.span,
                                    })
                                } else {
                                    // TODO: The index here *could be wrong*.
                                    // TODO: We need to test this to make sure there aren't messing anything here
                                    hir::Node::MemberAccess(hir::MemberAccess {
                                        ty,
                                        span: access.span,
                                        value: Box::new(node),
                                        member: access.member,
                                        index: index as _,
                                    })
                                }
                            }
                            None => {
                                return Err(TypeError::invalid_struct_field(
                                    access.expr.span(),
                                    access.member,
                                    ty.display(&sess.tycx),
                                ))
                            }
                        }
                    }
                    Type::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        hir::Node::Const(hir::Const {
                            value: ConstValue::Uint(*size as _),
                            ty: sess.tycx.common_types.uint,
                            span: access.span,
                        })
                    }
                    Type::Slice(..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        let ty = sess.tycx.common_types.uint;

                        if let Some(ConstValue::Str(s)) = node.as_const_value() {
                            hir::Node::Const(hir::Const {
                                value: ConstValue::Uint(s.len() as _),
                                ty,
                                span: access.span,
                            })
                        } else {
                            hir::Node::MemberAccess(hir::MemberAccess {
                                ty,
                                span: access.span,
                                value: Box::new(node),
                                member: access.member,
                                index: 1,
                            })
                        }
                    }
                    Type::Slice(inner, is_mutable)
                        if access.member.as_str() == BUILTIN_FIELD_DATA =>
                    {
                        hir::Node::MemberAccess(hir::MemberAccess {
                            ty: sess
                                .tycx
                                .bound(Type::MultiPointer(inner.clone(), *is_mutable), access.span),
                            span: access.span,
                            value: Box::new(node),
                            member: access.member,
                            index: 0,
                        })
                    }
                    Type::Module(module_id) => {
                        let id = sess.check_top_level_symbol(
                            CallerInfo {
                                module_id: env.module_id(),
                                span: access.span,
                            },
                            *module_id,
                            access.member,
                        )?;

                        id_or_const(id, access.span)
                    }
                    ty => Err(Diagnostic::error()
                        .with_message(format!(
                            "type `{}` has no member `{}`",
                            ty.display(&sess.tycx),
                            access.member
                        ))
                        .with_label(Label::primary(access.expr.span(), ""))),
                };

                if node_type.is_pointer() {
                    Ok(hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                        ty: sess.tycx.bound(node_type_deref, new_node.span()),
                        span: access.span,
                        value: Box::new(new_node),
                    })))
                } else {
                    Ok(new_node)
                }
            }
            ast::Ast::Ident(ident) => {
                if let Some(id) = env.find_function(ident.symbol) {
                    let function = sess.typed_ast.functions.get(id).unwrap();

                    let const_value = ConstValue::Function(ConstFunction {
                        id: hir::FunctionId::from(function.id.inner()),
                        name: function.name(),
                    });

                    Ok(hir::Node::Const(hir::Const {
                        value: const_value,
                        ty: function.ty,
                        span: ident.span,
                    }))
                } else {
                    match sess.get_symbol(env, ident.symbol) {
                        Some(id) => {
                            // this is a local binding
                            ident.binding_id = id;

                            sess.workspace.add_binding_info_use(id, ident.span);

                            let binding_info = sess.workspace.binding_infos.get(id).unwrap();

                            let ty = binding_info.ty;

                            if let Some(const_value) = &binding_info.const_value {
                                Ok(hir::Node::Const(hir::Const {
                                    value: const_value.clone(),
                                    ty,
                                    span: ident.span,
                                }))
                            } else {
                                let binding_ty = ty.normalize(&sess.tycx);

                                let min_scope_level = sess
                                    .function_frame()
                                    .map_or(ScopeLevel::Global, |f| f.scope_level);

                                if !binding_ty.is_type()
                                    && !binding_ty.is_module()
                                    && !binding_info.scope_level.is_global()
                                    && binding_info.scope_level < min_scope_level
                                {
                                    return Err(Diagnostic::error()
                                        .with_message("can't capture environment - closures are not implemented yet")
                                        .with_label(Label::primary(ident.span, "can't capture")));
                                }

                                Ok(hir::Node::Id(hir::Id {
                                    id,
                                    ty,
                                    span: ident.span,
                                }))
                            }
                        }
                        None => {
                            // this is either a top level binding, a builtin binding, or it doesn't exist
                            let id = sess.check_top_level_symbol(
                                CallerInfo {
                                    module_id: env.module_id(),
                                    span: ident.span,
                                },
                                env.module_id(),
                                ident.symbol,
                            )?;

                            Ok(id_or_const(id, ident.span))
                        }
                    }
                }
            }
            ast::Ast::ArrayLiteral(lit) => match &mut lit.kind {
                ast::ArrayLiteralKind::List(elements) => {
                    let element_ty_span = elements.first().map_or(lit.span, |e| e.span());
                    let element_ty = sess.tycx.var(element_ty_span);

                    let mut element_nodes: Vec<hir::Node> = vec![];

                    for el in elements.iter() {
                        let mut node = el.check(sess, env, Some(element_ty))?;

                        node.ty()
                            .unify(&element_ty, &mut sess.tycx)
                            .or_coerce_into_ty(
                                &mut node,
                                element_ty,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(
                                &sess.tycx,
                                element_ty,
                                Some(element_ty_span),
                                node.ty(),
                                el.span(),
                            )?;

                        element_nodes.push(node);
                    }

                    let ty = sess.tycx.bound(
                        Type::Array(Box::new(element_ty.into()), element_nodes.len()),
                        lit.span,
                    );

                    let is_const_array = element_nodes.iter().all(|node| node.is_const());

                    if is_const_array {
                        let const_array = ConstValue::Array(ConstArray {
                            values: element_nodes
                                .iter()
                                .map(|node| node.as_const_value().unwrap().clone())
                                .collect(),
                            element_ty,
                        });

                        Ok(hir::Node::Const(hir::Const {
                            value: const_array,
                            ty,
                            span: lit.span,
                        }))
                    } else {
                        Ok(hir::Node::Literal(hir::Literal::Array(hir::ArrayLiteral {
                            elements: element_nodes,
                            ty,
                            span: lit.span,
                        })))
                    }
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    let len_node = len.check(sess, env, None)?;
                    let len = sess.extract_const_int(&len_node)?;

                    if len < 0 {
                        return Err(TypeError::negative_array_len(len_node.span(), len));
                    }

                    let node = expr.check(sess, env, None)?;

                    let ty = sess
                        .tycx
                        .bound(Type::Array(Box::new(node.ty().into()), len as _), lit.span);

                    if let Some(const_value) = node.as_const_value() {
                        let const_array = ConstValue::Array(ConstArray {
                            values: vec![const_value.clone(); len as usize],
                            element_ty: node.ty(),
                        });

                        Ok(hir::Node::Const(hir::Const {
                            value: const_array,
                            ty,
                            span: lit.span,
                        }))
                    } else {
                        Ok(hir::Node::Literal(hir::Literal::ArrayFill(
                            hir::ArrayFillLiteral {
                                value: Box::new(node),
                                len: len as usize,
                                ty,
                                span: lit.span,
                            },
                        )))
                    }
                }
            },
            ast::Ast::TupleLiteral(lit) => {
                // when a tuple literal is empty, it is either a unit value or unit type
                if lit.elements.is_empty() {
                    let unit_ty = sess.tycx.common_types.unit;

                    let (ty, const_value) =
                        if expected_ty.map_or(false, |ty| ty.normalize(&sess.tycx).is_type()) {
                            (
                                sess.tycx.bound(unit_ty.as_kind().create_type(), lit.span),
                                ConstValue::Type(unit_ty),
                            )
                        } else {
                            (unit_ty, ConstValue::Unit(()))
                        };

                    return Ok(hir::Node::Const(hir::Const {
                        value: const_value,
                        ty,
                        span: lit.span,
                    }));
                }

                let elements = lit
                    .elements
                    .iter()
                    .map(|el| el.check(sess, env, None))
                    .collect::<DiagnosticResult<Vec<_>>>()?;

                let is_const_tuple = elements.iter().all(|node| node.is_const());

                if is_const_tuple {
                    let const_values: Vec<&ConstValue> = elements
                        .iter()
                        .map(|node| node.as_const_value().unwrap())
                        .collect();

                    let is_tuple_type = const_values.iter().all(|v| v.is_type());

                    if is_tuple_type {
                        let element_tys: Vec<Type> = elements
                            .iter()
                            .map(|node| {
                                node.as_const_value()
                                    .unwrap()
                                    .as_type()
                                    .normalize(&sess.tycx)
                                    .clone()
                            })
                            .collect();

                        let tuple_type = Type::Tuple(element_tys);

                        let const_value =
                            ConstValue::Type(sess.tycx.bound(tuple_type.clone(), lit.span));

                        Ok(hir::Node::Const(hir::Const {
                            value: const_value,
                            ty: sess.tycx.bound(tuple_type.create_type(), lit.span),
                            span: lit.span,
                        }))
                    } else {
                        let element_tys: Vec<TypeId> =
                            lit.elements.iter().map(|e| e.ty()).collect();

                        let element_ty_kinds: Vec<Type> =
                            element_tys.iter().map(|ty| ty.as_kind()).collect();

                        let kind = Type::Tuple(element_ty_kinds.clone());

                        let ty = sess.tycx.bound(kind.clone(), lit.span);

                        let const_value = ConstValue::Tuple(
                            const_values
                                .iter()
                                .cloned()
                                .cloned()
                                .zip(element_tys)
                                .map(|(value, ty)| ConstElement { value, ty })
                                .collect(),
                        );

                        Ok(hir::Node::Const(hir::Const {
                            value: const_value.clone(),
                            ty,
                            span: lit.span,
                        }))
                    }
                } else {
                    let element_tys: Vec<Type> =
                        lit.elements.iter().map(|e| e.ty().as_kind()).collect();

                    let kind = Type::Tuple(element_tys);

                    let ty = sess.tycx.bound(kind.clone(), lit.span);

                    Ok(hir::Node::Literal(hir::Literal::Tuple(hir::TupleLiteral {
                        elements,
                        ty,
                        span: lit.span,
                    })))
                }
            }
            ast::Ast::StructLiteral(lit) => match &mut lit.type_expr {
                Some(type_expr) => {
                    let node = type_expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                    let ty = sess.extract_const_type(&node)?;

                    let kind = ty.normalize(&sess.tycx);

                    match kind {
                        Type::Struct(struct_ty) => check_named_struct_literal(
                            sess,
                            env,
                            struct_ty,
                            &mut lit.fields,
                            lit.span,
                        ),
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "type `{}` does not support struct initialization syntax",
                                    ty
                                ))
                                .with_label(Label::primary(type_expr.span(), "not a struct type")))
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
                                lit.span,
                            ),
                            _ => {
                                check_anonymous_struct_literal(sess, env, &mut lit.fields, lit.span)
                            }
                        }
                    }
                    None => check_anonymous_struct_literal(sess, env, &mut lit.fields, lit.span),
                },
            },
            ast::Ast::Literal(lit) => {
                let const_value: ConstValue = lit.kind.into();

                let ty = match &lit.kind {
                    ast::LiteralKind::Nil => sess.tycx.var(lit.span),
                    ast::LiteralKind::Bool(_) => sess.tycx.common_types.bool,
                    ast::LiteralKind::Int(_) => sess.tycx.anyint(lit.span),
                    ast::LiteralKind::Float(_) => sess.tycx.anyfloat(lit.span),
                    ast::LiteralKind::Str(_) => sess.tycx.common_types.str,
                    ast::LiteralKind::Char(_) => sess.tycx.common_types.u8,
                };

                Ok(hir::Node::Const(hir::Const {
                    ty,
                    span: lit.span,
                    value: const_value,
                }))
            }
            ast::Ast::PointerType(ast::ExprAndMut {
                inner,
                is_mutable,
                span,
                ..
            }) => {
                let node = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_kind = sess.extract_const_type(&node)?;
                let kind = Type::Pointer(Box::new(inner_kind.into()), *is_mutable);

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tycx.bound(kind.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tycx.bound(kind, *span)),
                }))
            }
            ast::Ast::MultiPointerType(ast::ExprAndMut {
                inner,
                is_mutable,
                span,
                ..
            }) => {
                let node = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_kind = sess.extract_const_type(&node)?;
                let kind = Type::MultiPointer(Box::new(inner_kind.into()), *is_mutable);

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tycx.bound(kind.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tycx.bound(kind, *span)),
                }))
            }
            ast::Ast::ArrayType(ast::ArrayType {
                inner, size, span, ..
            }) => {
                let inner_node = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_ty = sess.extract_const_type(&inner_node)?;

                let size_node = size.check(sess, env, None)?;

                let size_value = sess.extract_const_int(&size_node)?;

                if size_value < 0 {
                    return Err(TypeError::negative_array_len(size.span(), size_value));
                }

                let array_ty = Type::Array(Box::new(inner_ty.into()), size_value as usize);

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tycx.bound(array_ty.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tycx.bound(array_ty, *span)),
                }))
            }
            ast::Ast::SliceType(ast::ExprAndMut {
                inner,
                is_mutable,
                span,
                ..
            }) => {
                let node = inner.check(sess, env, Some(sess.tycx.common_types.anytype))?;
                let inner_kind = sess.extract_const_type(&node)?;
                let kind = Type::Slice(Box::new(inner_kind.into()), *is_mutable);

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tycx.bound(kind.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tycx.bound(kind, *span)),
                }))
            }
            ast::Ast::StructType(st) => {
                st.name = if st.name.is_empty() {
                    get_anonymous_struct_name(st.span)
                } else {
                    st.name
                };

                // the struct's main type variable
                let struct_ty_var = sess.tycx.bound(
                    Type::Struct(StructType::opaque(st.name, st.binding_id, st.kind)),
                    st.span,
                );

                // the struct's main type variable, in its `type` variation
                let struct_ty_type_var = sess
                    .tycx
                    .bound(struct_ty_var.as_kind().create_type(), st.span);

                sess.self_types.push(struct_ty_var);

                env.push_scope(ScopeKind::Block);

                st.binding_id = sess.bind_symbol(
                    env,
                    st.name,
                    ast::Visibility::Private,
                    struct_ty_type_var,
                    Some(ConstValue::Type(struct_ty_var)),
                    false,
                    ast::BindingKind::Normal,
                    st.span,
                )?;

                let mut field_map = UstrMap::<Span>::default();
                let mut struct_ty_fields = vec![];

                for field in st.fields.iter_mut() {
                    let node = field
                        .ty
                        .check(sess, env, Some(sess.tycx.common_types.anytype))?;
                    let ty = sess.extract_const_type(&node)?;

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
                    binding_id: st.binding_id,
                    kind: st.kind,
                    fields: struct_ty_fields,
                });

                if occurs(struct_ty_var, &struct_ty, &sess.tycx) {
                    Err(UnifyTyErr::Occurs.into_diagnostic(
                        &sess.tycx,
                        struct_ty,
                        None,
                        struct_ty_var,
                        st.span,
                    ))
                } else {
                    Ok(hir::Node::Const(hir::Const {
                        ty: sess.tycx.bound(struct_ty.clone().create_type(), st.span),
                        span: st.span,
                        value: ConstValue::Type(sess.tycx.bound(struct_ty, st.span)),
                    }))
                }
            }
            ast::Ast::FunctionType(sig) => {
                let node = sig.check(sess, env, expected_ty)?;

                let ty = sess.tycx.bound(node.ty().as_kind().create_type(), sig.span);

                Ok(hir::Node::Const(hir::Const {
                    ty,
                    span: sig.span,
                    value: ConstValue::Type(node.ty()),
                }))
            }
            ast::Ast::SelfType(expr) => match sess.self_types.last() {
                Some(&ty) => {
                    let ty = sess.tycx.bound(ty.as_kind().create_type(), expr.span);

                    Ok(hir::Node::Const(hir::Const {
                        ty,
                        span: expr.span,
                        value: ConstValue::Type(ty),
                    }))
                }
                None => Err(Diagnostic::error()
                    .with_message("`Self` is only available within struct types")
                    .with_label(Label::primary(expr.span, "`Self` is invalid here"))),
            },
            ast::Ast::Placeholder(expr) => {
                let ty = sess.tycx.var(expr.span);

                Ok(hir::Node::Const(hir::Const {
                    ty,
                    span: expr.span,
                    value: ConstValue::Type(ty),
                }))
            }
            ast::Ast::Const(_) => panic!(),
            ast::Ast::Error(expr) => Ok(hir::Node::noop(sess.tycx.var(expr.span), expr.span)),
        }
    }
}

impl Check for ast::While {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let bool_type = sess.tycx.common_types.bool;

        let mut condition_node = self.condition.check(sess, env, Some(bool_type))?;

        condition_node
            .ty()
            .unify(&bool_type, &mut sess.tycx)
            .or_coerce_into_ty(
                &mut condition_node,
                bool_type,
                &mut sess.tycx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tycx,
                bool_type,
                None,
                condition_node.ty(),
                self.condition.span(),
            )?;

        env.push_scope(ScopeKind::Loop);
        sess.loop_depth += 1;

        let block_node = self.block.check(sess, env, None)?;

        sess.loop_depth -= 1;
        env.pop_scope();

        let while_node_type = match condition_node.as_const_value() {
            Some(ConstValue::Bool(true)) => sess.tycx.common_types.never,
            _ => sess.tycx.common_types.unit,
        };

        Ok(hir::Node::Control(hir::Control::While(hir::While {
            condition: Box::new(condition_node),
            body: Box::new(block_node),
            ty: while_node_type,
            span: self.span,
        })))
    }
}

impl Check for ast::FunctionExpr {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let name = self.sig.name;

        let sig_node = self.sig.check(sess, env, expected_ty)?;

        let sig_ty = sig_node.ty();
        let function_type = sess
            .extract_const_type(&sig_node)?
            .normalize(&sess.tycx)
            .into_fn();

        let return_type = sess.tycx.bound(
            function_type.ret.as_ref().clone(),
            self.sig
                .return_type
                .as_ref()
                .map_or(self.sig.span, |e| e.span()),
        );

        let return_ty_span = self
            .sig
            .return_type
            .as_ref()
            .map_or(self.sig.span, |e| e.span());

        env.push_scope(ScopeKind::Function);

        for (param, param_ty) in self.sig.params.iter_mut().zip(function_type.params.iter()) {
            let ty = sess.tycx.bound(
                param_ty.clone(),
                param
                    .type_expr
                    .as_ref()
                    .map_or(param.pattern.span(), |e| e.span()),
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

        let function_id = sess.cache.functions.insert_with_id(hir::Function {
            id: hir::FunctionId::unknown(),
            module_id: env.module_id(),
            name,
            kind: hir::FunctionKind::Orphan { body: None },
            ty: sig_ty,
            span: self.span,
        });

        env.insert_function(name, function_id);

        let mut body_node = sess.with_function_frame(
            FunctionFrame {
                return_ty: return_type,
                return_ty_span,
                scope_level: env.scope_level(),
            },
            |sess| self.body.check(sess, env, None),
        )?;

        let mut unify_node = body_node.ty().unify(&return_type, &mut sess.tycx);

        if let Some(last_statement) = body_node.as_sequence_mut().unwrap().statements.last_mut() {
            unify_node = unify_node.or_coerce_into_ty(
                last_statement,
                return_type,
                &mut sess.tycx,
                sess.target_metrics.word_size,
            );
        }

        unify_node.or_report_err(
            &sess.tycx,
            return_type,
            Some(return_ty_span),
            body_node.ty(),
            self.body.span,
        )?;

        env.pop_scope();

        sess.cache
            .functions
            .get_mut(function_id)
            .unwrap()
            .set_body(self.body.clone());

        Ok(hir::Node::Const(hir::Const {
            value: ConstValue::Function(ConstFunction {
                id: function_id,
                name,
            }),
            ty: sig_ty,
            span: self.span,
        }))
    }
}

impl Check for ast::Assignment {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let lhs_node = self.lhs.check(sess, env, None)?;
        let mut rhs_node = self.rhs.check(sess, env, Some(lhs_node.ty()))?;

        rhs_node
            .ty()
            .unify(&lhs_node.ty(), &mut sess.tycx)
            .or_coerce_into_ty(
                &mut rhs_node,
                lhs_node.ty(),
                &mut sess.tycx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tycx,
                lhs_node.ty(),
                Some(lhs_node.span()),
                rhs_node.ty(),
                rhs_node.span(),
            )?;

        Ok(hir::Node::Assignment(hir::Assignment {
            ty: sess.tycx.common_types.unit,
            span: self.span,
            lhs: Box::new(lhs_node),
            rhs: Box::new(rhs_node),
        }))
    }
}

impl Check for ast::Return {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let function_frame = sess
            .function_frame()
            .ok_or(SyntaxError::outside_of_function(self.span, "return"))?;

        let value = if let Some(expr) = &self.expr {
            let expected = function_frame.return_ty;
            let mut node = expr.check(sess, env, Some(expected))?;

            node.ty()
                .unify(&expected, &mut sess.tycx)
                .or_coerce_into_ty(
                    &mut node,
                    expected,
                    &mut sess.tycx,
                    sess.target_metrics.word_size,
                )
                .or_report_err(
                    &sess.tycx,
                    expected,
                    Some(function_frame.return_ty_span),
                    node.ty(),
                    expr.span(),
                )?;

            node
        } else {
            let unit_type = sess.tycx.common_types.unit;

            function_frame
                .return_ty
                .unify(&unit_type, &mut sess.tycx)
                .or_report_err(
                    &sess.tycx,
                    unit_type,
                    None,
                    function_frame.return_ty,
                    self.span,
                )?;

            hir::Node::Const(hir::Const {
                value: ConstValue::Unit(()),
                ty: unit_type,
                span: self.span,
            })
        };

        Ok(hir::Node::Control(hir::Control::Return(hir::Return {
            value: Box::new(value),
            ty: sess.tycx.common_types.never,
            span: self.span,
        })))
    }
}
impl Check for ast::If {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let unit_type = sess.tycx.common_types.unit;
        let bool_type = sess.tycx.common_types.bool;

        let mut condition_node = self.condition.check(sess, env, Some(bool_type))?;

        condition_node
            .ty()
            .unify(&bool_type, &mut sess.tycx)
            .or_coerce_into_ty(
                &mut condition_node,
                bool_type,
                &mut sess.tycx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tycx,
                bool_type,
                None,
                condition_node.ty(),
                self.condition.span(),
            )?;

        // if the condition is compile-time known, only check the resulting branch
        match condition_node.as_const_value() {
            Some(ConstValue::Bool(true)) => self.then.check(sess, env, expected_ty),
            Some(ConstValue::Bool(false)) => {
                if let Some(otherwise) = &mut self.otherwise {
                    otherwise.check(sess, env, expected_ty)
                } else {
                    Ok(hir::Node::Const(hir::Const {
                        value: ConstValue::Unit(()),
                        ty: unit_type,
                        span: self.span,
                    }))
                }
            }
            _ => {
                let mut then_node = self.then.check(sess, env, expected_ty)?;

                let if_node = if let Some(otherwise) = &mut self.otherwise {
                    let mut otherwise_node = otherwise.check(sess, env, Some(then_node.ty()))?;

                    otherwise_node
                        .ty()
                        .unify(&then_node.ty(), &mut sess.tycx)
                        .or_coerce(
                            &mut then_node,
                            &mut otherwise_node,
                            &mut sess.tycx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tycx,
                            then_node.ty(),
                            Some(self.then.span()),
                            otherwise_node.ty(),
                            otherwise.span(),
                        )?;

                    hir::If {
                        ty: then_node.ty(),
                        span: self.span,
                        condition: Box::new(condition_node),
                        then: Box::new(then_node),
                        otherwise: Some(Box::new(otherwise_node)),
                    }
                } else {
                    hir::If {
                        ty: unit_type,
                        span: self.span,
                        condition: Box::new(condition_node),
                        then: Box::new(then_node),
                        otherwise: None,
                    }
                };

                Ok(hir::Node::Control(hir::Control::If(if_node)))
            }
        }
    }
}

impl Check for ast::Binary {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_ty: Option<TypeId>) -> Result {
        let mut lhs_node = self.lhs.check(sess, env, None)?;
        let mut rhs_node = self.rhs.check(sess, env, Some(lhs_node.ty()))?;

        let expected_ty = match &self.op {
            ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Rem
            | ast::BinaryOp::Lt
            | ast::BinaryOp::Le
            | ast::BinaryOp::Gt
            | ast::BinaryOp::Ge
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitOr
            | ast::BinaryOp::BitXor
            | ast::BinaryOp::BitAnd => sess.tycx.anyint(self.span),

            ast::BinaryOp::Eq | ast::BinaryOp::Ne | ast::BinaryOp::And | ast::BinaryOp::Or => {
                sess.tycx.common_types.bool
            }
        };

        lhs_node
            .ty()
            .unify(&expected_ty, &mut sess.tycx)
            .or_report_err(
                &sess.tycx,
                expected_ty,
                None,
                lhs_node.ty(),
                self.lhs.span(),
            )?;

        rhs_node
            .ty()
            .unify(&lhs_node.ty(), &mut sess.tycx)
            .or_coerce(
                &mut lhs_node,
                &mut rhs_node,
                &mut sess.tycx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tycx,
                lhs_node.ty(),
                None,
                rhs_node.ty(),
                self.rhs.span(),
            )?;

        let ty = match &self.op {
            ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Rem
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitOr
            | ast::BinaryOp::BitXor
            | ast::BinaryOp::BitAnd => self.lhs.ty(),

            ast::BinaryOp::Eq
            | ast::BinaryOp::Ne
            | ast::BinaryOp::Lt
            | ast::BinaryOp::Le
            | ast::BinaryOp::Gt
            | ast::BinaryOp::Ge
            | ast::BinaryOp::And
            | ast::BinaryOp::Or => sess.tycx.common_types.bool,
        };

        let span = self.span;

        match (lhs_node.as_const_value(), rhs_node.as_const_value()) {
            (Some(lhs), Some(rhs)) => {
                let const_value = const_fold::binary(lhs, rhs, self.op, self.span)?;

                Ok(hir::Node::Const(hir::Const {
                    value: const_value,
                    ty,
                    span,
                }))
            }
            _ => {
                let binary = hir::Binary {
                    lhs: Box::new(lhs_node),
                    rhs: Box::new(rhs_node),
                    ty,
                    span,
                };

                let builtin = match self.op {
                    ast::BinaryOp::Add => hir::Builtin::Add(binary),
                    ast::BinaryOp::Sub => hir::Builtin::Sub(binary),
                    ast::BinaryOp::Mul => hir::Builtin::Mul(binary),
                    ast::BinaryOp::Div => hir::Builtin::Div(binary),
                    ast::BinaryOp::Rem => hir::Builtin::Rem(binary),
                    ast::BinaryOp::Eq => hir::Builtin::Eq(binary),
                    ast::BinaryOp::Ne => hir::Builtin::Ne(binary),
                    ast::BinaryOp::Lt => hir::Builtin::Lt(binary),
                    ast::BinaryOp::Le => hir::Builtin::Le(binary),
                    ast::BinaryOp::Gt => hir::Builtin::Gt(binary),
                    ast::BinaryOp::Ge => hir::Builtin::Ge(binary),
                    ast::BinaryOp::And => hir::Builtin::And(binary),
                    ast::BinaryOp::Or => hir::Builtin::Or(binary),
                    ast::BinaryOp::Shl => hir::Builtin::Shl(binary),
                    ast::BinaryOp::Shr => hir::Builtin::Shr(binary),
                    ast::BinaryOp::BitAnd => hir::Builtin::BitAnd(binary),
                    ast::BinaryOp::BitOr => hir::Builtin::BitOr(binary),
                    ast::BinaryOp::BitXor => hir::Builtin::BitXor(binary),
                };

                Ok(hir::Node::Builtin(builtin))
            }
        }
    }
}
impl Check for ast::Unary {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_ty: Option<TypeId>) -> Result {
        let node = self.value.check(sess, env, None)?;

        match self.op {
            ast::UnaryOp::Ref(is_mutable) => {
                let ty = sess.tycx.bound(
                    Type::Pointer(Box::new(node.ty().into()), is_mutable),
                    self.span,
                );

                Ok(hir::Node::Builtin(hir::Builtin::Ref(hir::Unary {
                    ty,
                    span: self.span,
                    value: Box::new(node),
                })))
            }
            ast::UnaryOp::Deref => {
                let pointee_ty = sess.tycx.var(self.span);

                let ptr_ty = sess
                    .tycx
                    .bound(Type::Pointer(Box::new(pointee_ty.into()), false), self.span);

                node.ty().unify(&ptr_ty, &mut sess.tycx).or_report_err(
                    &sess.tycx,
                    ptr_ty,
                    None,
                    node.ty(),
                    self.value.span(),
                )?;

                Ok(hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                    ty: pointee_ty,
                    span: self.span,
                    value: Box::new(node),
                })))
            }
            ast::UnaryOp::Not => {
                let anyint = sess.tycx.anyint(self.span);
                let bool = sess.tycx.common_types.bool;

                node.ty()
                    .unify(&bool, &mut sess.tycx)
                    .or_else(|_| node.ty().unify(&anyint, &mut sess.tycx))
                    .or_report_err(&sess.tycx, bool, None, node.ty(), self.value.span())?;

                if let Some(const_value) = node.as_const_value() {
                    Ok(hir::Node::Const(hir::Const {
                        value: const_value.not(),
                        ty: node.ty(),
                        span: self.span,
                    }))
                } else {
                    Ok(hir::Node::Builtin(hir::Builtin::Not(hir::Unary {
                        value: Box::new(node),
                        ty: node.ty(),
                        span: self.span,
                    })))
                }
            }
            ast::UnaryOp::Neg => {
                let anyint = sess.tycx.anyint(self.span);

                node.ty().unify(&anyint, &mut sess.tycx).or_report_err(
                    &sess.tycx,
                    anyint,
                    None,
                    node.ty(),
                    self.value.span(),
                )?;

                if let Some(const_value) = node.as_const_value() {
                    Ok(hir::Node::Const(hir::Const {
                        value: const_value.neg(),
                        ty: node.ty(),
                        span: self.span,
                    }))
                } else {
                    Ok(hir::Node::Builtin(hir::Builtin::Neg(hir::Unary {
                        value: Box::new(node),
                        ty: node.ty(),
                        span: self.span,
                    })))
                }
            }
            ast::UnaryOp::Plus => {
                let anyint = sess.tycx.anyint(self.span);

                node.ty().unify(&anyint, &mut sess.tycx).or_report_err(
                    &sess.tycx,
                    anyint,
                    None,
                    node.ty(),
                    self.value.span(),
                )?;

                Ok(node)
            }
        }
    }
}
impl Check for ast::Call {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_ty: Option<TypeId>) -> Result {
        let callee = self.callee.check(sess, env, None)?;

        match callee.ty().normalize(&sess.tycx) {
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

                let mut args = vec![];

                for (index, arg) in self.args.iter().enumerate() {
                    if let Some(param) = fn_ty.params.get(index) {
                        let param_ty = sess.tycx.bound(param.clone(), self.span);
                        let mut node = arg.check(sess, env, Some(param_ty))?;

                        node.ty()
                            .unify(&param_ty, &mut sess.tycx)
                            .or_coerce_into_ty(
                                &mut node,
                                param_ty,
                                &mut sess.tycx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(&sess.tycx, param_ty, None, node.ty(), arg.span())?;

                        args.push(node);
                    } else {
                        // this is a variadic argument, meaning that the argument's
                        // index is greater than the function's param length
                        let node = arg.check(sess, env, None)?;
                        args.push(node);
                    }
                }

                Ok(hir::Node::Call(hir::Call {
                    ty: sess.tycx.bound(fn_ty.ret.as_ref().clone(), self.span),
                    span: self.span,
                    callee: Box::new(callee),
                    args,
                }))
            }
            ty => {
                let args = self
                    .args
                    .iter()
                    .map(|arg| arg.check(sess, env, None))
                    .collect::<DiagnosticResult<Vec<_>>>()?;

                let return_ty = sess.tycx.var(self.span);

                let inferred_fn_ty = Type::Function(FunctionType {
                    params: self.args.iter().map(|arg| arg.ty().into()).collect(),
                    ret: Box::new(return_ty.into()),
                    varargs: None,
                    kind: FunctionTypeKind::Orphan,
                });

                ty.unify(&inferred_fn_ty, &mut sess.tycx).or_report_err(
                    &sess.tycx,
                    inferred_fn_ty,
                    None,
                    ty,
                    self.callee.span(),
                )?;

                Ok(hir::Node::Call(hir::Call {
                    ty: return_ty,
                    span: self.span,
                    callee: Box::new(callee),
                    args,
                }))
            }
        }
    }
}

impl Check for ast::Cast {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let node = self.expr.check(sess, env, None)?;

        let target_ty = if let Some(ty_expr) = &mut self.target {
            let node = ty_expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
            sess.extract_const_type(&node)?
        } else {
            match expected_ty {
                Some(t) => t,
                None => {
                    return Err(Diagnostic::error()
                        .with_message("can't infer the type cast's target type")
                        .with_label(Label::primary(self.expr.span(), "can't infer")))
                }
            }
        };

        let from = node.ty().normalize(&sess.tycx);
        let to = target_ty.normalize(&sess.tycx);

        if can_cast_type(&from, &to) {
            Ok(hir::Node::Cast(hir::Cast {
                value: Box::new(node),
                ty: target_ty,
                span: self.span,
            }))
        } else {
            Err(Diagnostic::error()
                .with_message(format!("cannot cast from `{}` to `{}`", from, to))
                .with_label(Label::primary(
                    self.span,
                    format!("invalid cast to `{}`", to),
                )))
        }
    }
}

impl Check for ast::Block {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_ty: Option<TypeId>) -> Result {
        let unit_node = hir::Node::Const(hir::Const {
            value: ConstValue::Unit(()),
            ty: sess.tycx.common_types.unit,
            span: self.span,
        });

        if self.statements.is_empty() {
            Ok(unit_node)
        } else {
            env.push_scope(ScopeKind::Block);

            let last_index = self.statements.len() - 1;
            let statements = self
                .statements
                .iter()
                .enumerate()
                .map(|(i, expr)| {
                    expr.check(sess, env, if i == last_index { expected_ty } else { None })
                })
                .collect::<DiagnosticResult<Vec<_>>>()?;

            env.pop_scope();

            let last_statement_ty = statements.last().unwrap().ty();

            if self.yields {
                Ok(hir::Node::Sequence(hir::Sequence {
                    ty: last_statement_ty,
                    span: self.span,
                    statements,
                }))
            } else {
                let ty = if last_statement_ty.normalize(&sess.tycx).is_never() {
                    sess.tycx.common_types.never
                } else {
                    sess.tycx.common_types.unit
                };

                Ok(hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty,
                    span: self.span,
                }))
            }
        }
    }
}

#[inline]
fn check_named_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    struct_ty: StructType,
    fields: &mut Vec<ast::StructLiteralField>,
    span: Span,
) -> Result {
    let mut field_set = UstrSet::default();
    let mut uninit_fields = UstrSet::from_iter(struct_ty.fields.iter().map(|f| f.symbol));

    let mut field_nodes: Vec<hir::StructLiteralField> = vec![];

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
                let mut node = field.expr.check(sess, env, Some(expected_ty))?;

                node.ty()
                    .unify(&expected_ty, &mut sess.tycx)
                    .or_coerce_into_ty(
                        &mut node,
                        expected_ty,
                        &mut sess.tycx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tycx,
                        expected_ty,
                        Some(ty_field.span),
                        node.ty(),
                        field.expr.span(),
                    )?;

                field_nodes.push(hir::StructLiteralField {
                    ty: node.ty(),
                    span,
                    name: field.symbol,
                    value: Box::new(node),
                });
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

    Ok(make_struct_literal_node(sess, field_nodes, struct_ty, span))
}

#[inline]
fn check_anonymous_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    fields: &mut Vec<ast::StructLiteralField>,
    span: Span,
) -> Result {
    let mut field_set = UstrSet::default();

    let name = get_anonymous_struct_name(span);

    let mut field_nodes: Vec<hir::StructLiteralField> = vec![];

    let mut struct_ty = StructType {
        name,
        binding_id: BindingId::unknown(),
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

        let node = field.expr.check(sess, env, None)?;

        struct_ty.fields.push(StructTypeField {
            symbol: field.symbol,
            ty: node.ty().into(),
            span: field.span,
        });

        field_nodes.push(hir::StructLiteralField {
            ty: node.ty(),
            span,
            name: field.symbol,
            value: Box::new(node),
        });
    }

    Ok(make_struct_literal_node(sess, field_nodes, struct_ty, span))
}

fn make_struct_literal_node(
    sess: &mut CheckSess,
    field_nodes: Vec<hir::StructLiteralField>,
    struct_ty: StructType,
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
            ty: sess.tycx.bound(Type::Struct(struct_ty), span),
            span,
        })
    } else {
        hir::Node::Literal(hir::Literal::Struct(hir::StructLiteral {
            ty: sess.tycx.bound(Type::Struct(struct_ty), span),
            span,
            fields: field_nodes,
        }))
    }
}

fn get_anonymous_struct_name(span: Span) -> Ustr {
    ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
}

fn interp_expr(expr: &ast::Ast, sess: &mut CheckSess, module_id: ModuleId) -> InterpResult {
    let mut interp_sess = sess
        .interp
        .create_session(sess.workspace, &sess.tycx, &sess.typed_ast);
    interp_sess.eval(expr, module_id)
}

pub(super) fn check_optional_type_expr<'s>(
    type_expr: &Option<Box<ast::Ast>>,
    sess: &mut CheckSess<'s>,
    env: &mut Env,
    span: Span,
) -> DiagnosticResult<TypeId> {
    if let Some(type_expr) = type_expr {
        check_type_expr(type_expr, sess, env, span)
    } else {
        Ok(sess.tycx.var(span))
    }
}

pub(super) fn check_type_expr<'s>(
    type_expr: &ast::Ast,
    sess: &mut CheckSess<'s>,
    env: &mut Env,
    span: Span,
) -> DiagnosticResult<TypeId> {
    let node = type_expr.check(sess, env, Some(sess.tycx.common_types.anytype))?;
    sess.extract_const_type(&node)
}
