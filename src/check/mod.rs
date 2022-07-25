mod attrs;
mod builtin;
mod const_fold;
mod env;
mod lvalue_access;
mod pattern;
mod top_level;

use self::pattern::get_qualified_name;
use crate::{
    ast::{self, pattern::Pattern},
    common::{
        builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
        target::TargetMetrics,
    },
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError, TypeError,
    },
    hir::{
        self,
        attrs::AttrKind,
        const_value::{ConstArray, ConstElement, ConstExternVariable, ConstFunction, ConstValue},
    },
    infer::{
        cast::{can_cast_type, try_cast_const_value},
        coerce::{OrCoerce, OrCoerceIntoTy},
        display::{DisplayTy, OrReportErr},
        normalize::Normalize,
        substitute::substitute,
        type_ctx::TypeCtx,
        unify::{occurs, UnifyType, UnifyTypeErr},
    },
    interp::interp::Interp,
    span::Span,
    types::{
        align_of::AlignOf, is_sized::IsSized, size_of::SizeOf, FunctionType, FunctionTypeKind,
        FunctionTypeParam, FunctionTypeVarargs, InferType, PartialStructType, StructType,
        StructTypeField, StructTypeKind, Type, TypeId,
    },
    workspace::{
        BindingId, BindingInfo, BindingInfoFlags, BindingInfoKind, ModuleId, PartialBindingInfo,
        ScopeLevel, Workspace,
    },
};
use env::{Env, Scope, ScopeKind};
use indexmap::IndexMap;
use std::{
    collections::{HashMap, HashSet},
    iter::repeat_with,
};
use top_level::CallerInfo;
use ustr::{ustr, Ustr, UstrMap, UstrSet};

pub type CheckData = (hir::Cache, TypeCtx);

pub fn check(workspace: &mut Workspace, module: Vec<ast::Module>) -> CheckData {
    let mut sess = CheckSess::new(workspace, &module);

    if let Err(diag) = sess.start() {
        sess.workspace.diagnostics.push(diag);
    }

    if sess.workspace.diagnostics.has_errors() {
        return sess.into_data();
    }

    substitute(&mut sess.workspace.diagnostics, &mut sess.tcx, &sess.cache);

    check_entry_point_function(&mut sess);

    sess.into_data()
}

fn check_entry_point_function(sess: &mut CheckSess) {
    // Check that an entry point function exists, if needed
    if sess.workspace.build_options.need_entry_point_function() {
        if let Some(binding_info) = sess.workspace.entry_point_function() {
            let ty = binding_info.ty.normalize(&sess.tcx).into_function();

            // if this is the main function, check its type matches a fn() -> [unit | never]
            if !(ty.return_type.is_unit() || ty.return_type.is_never())
                || !ty.params.is_empty()
                || ty.varargs.is_some()
            {
                sess.workspace.diagnostics.push(
                    Diagnostic::error()
                        .with_message(format!(
                            "entry point function `main` has type `{}`, expected `fn() -> ()`",
                            ty
                        ))
                        .with_label(Label::primary(
                            binding_info.span,
                            "invalid type of entry point function",
                        )),
                );
            }
        } else {
            sess.workspace.diagnostics.push(
                Diagnostic::error()
                    .with_message("entry point function `main` is not defined")
                    .with_label(Label::primary(
                        Span::initial(sess.workspace.get_root_module_info().file_id),
                        "",
                    ))
                    .with_note("define function `let main = fn() {}` in your entry file"),
            );
        }
    }
}

#[derive(Debug)]
pub(super) struct QueuedModule {
    pub(super) module_type: TypeId,
    pub(super) all_complete: bool,
    pub(super) complete_bindings: HashSet<usize>, // Binding index -> Completion status
}

pub(super) struct CheckSess<'s> {
    pub workspace: &'s mut Workspace,
    pub target_metrics: TargetMetrics,

    pub interp: Interp,

    pub tcx: TypeCtx,

    // The ast's being processed
    pub modules: &'s Vec<ast::Module>,

    pub cache: hir::Cache,
    pub queued_modules: HashMap<ModuleId, QueuedModule>,

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

    pub unique_name_indices: UstrMap<usize>,

    pub in_lvalue_context: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionFrame {
    return_type: TypeId,
    return_type_span: Span,
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
            tcx: TypeCtx::default(),
            modules: old_asts,
            cache: hir::Cache::new(),
            queued_modules: HashMap::new(),
            global_scopes: HashMap::new(),
            builtin_types: UstrMap::default(),
            function_frames: vec![],
            self_types: vec![],
            loop_depth: 0,
            unique_name_indices: UstrMap::default(),
            in_lvalue_context: false,
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
        (self.cache, self.tcx)
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
                node.ty().display(&self.tcx),
                "a type",
            )),
        }
    }

    pub fn extract_const_int(&self, node: &hir::Node) -> DiagnosticResult<i64> {
        match node.as_const_value() {
            Some(ConstValue::Int(v)) => Ok(*v),
            _ => Err(TypeError::expected(
                node.span(),
                node.ty().display(&self.tcx),
                "compile-time known integer",
            )),
        }
    }

    fn add_builtin_types(&mut self) {
        let mk = |sess: &mut CheckSess, name: &str, ty: TypeId| {
            let name = ustr(name);

            let partial_binding_info = PartialBindingInfo {
                module_id: Default::default(),
                name,
                visibility: ast::Visibility::Public,
                ty: sess
                    .tcx
                    .bound_maybe_spanned(ty.as_kind().create_type(), None),
                const_value: Some(ConstValue::Type(ty)),
                is_mutable: false,
                kind: BindingInfoKind::Orphan,
                scope_level: ScopeLevel::Global,
                qualified_name: name,
                span: Span::unknown(),
                flags: BindingInfoFlags::BUILTIN_TYPE,
            };

            let id = sess
                .workspace
                .binding_infos
                .insert_with_id(partial_binding_info.into_binding_info());

            sess.builtin_types.insert(name, id);
        };

        mk(self, builtin::SYM_UNIT, self.tcx.common_types.unit);
        mk(self, builtin::SYM_BOOL, self.tcx.common_types.bool);

        mk(self, builtin::SYM_I8, self.tcx.common_types.i8);
        mk(self, builtin::SYM_I16, self.tcx.common_types.i16);
        mk(self, builtin::SYM_I32, self.tcx.common_types.i32);
        mk(self, builtin::SYM_I64, self.tcx.common_types.i64);
        mk(self, builtin::SYM_INT, self.tcx.common_types.int);

        mk(self, builtin::SYM_U8, self.tcx.common_types.u8);
        mk(self, builtin::SYM_U16, self.tcx.common_types.u16);
        mk(self, builtin::SYM_U32, self.tcx.common_types.u32);
        mk(self, builtin::SYM_U64, self.tcx.common_types.u64);
        mk(self, builtin::SYM_UINT, self.tcx.common_types.uint);

        mk(self, builtin::SYM_F16, self.tcx.common_types.f16);
        mk(self, builtin::SYM_F32, self.tcx.common_types.f32);
        mk(self, builtin::SYM_F64, self.tcx.common_types.f64);
        mk(self, builtin::SYM_FLOAT, self.tcx.common_types.float);

        mk(self, builtin::SYM_NEVER, self.tcx.common_types.never);

        mk(self, builtin::SYM_STR, self.tcx.common_types.str);
    }

    pub(super) fn is_lvalue(&self, node: &hir::Node) -> bool {
        match node {
            hir::Node::MemberAccess(_)
            | hir::Node::Id(_)
            | hir::Node::Builtin(hir::Builtin::Deref(_))
            | hir::Node::Builtin(hir::Builtin::Offset(_)) => true,
            _ => false,
        }
    }

    pub(super) fn id_or_const_by_id(&self, id: BindingId, span: Span) -> hir::Node {
        self.id_or_const(self.workspace.binding_infos.get(id).unwrap(), span)
    }

    pub(super) fn id_or_const(&self, binding_info: &BindingInfo, span: Span) -> hir::Node {
        match &binding_info.const_value {
            Some(value) if !self.in_lvalue_context => hir::Node::Const(hir::Const {
                value: value.clone(),
                ty: binding_info.ty,
                span,
            }),
            _ => hir::Node::Id(hir::Id {
                id: binding_info.id,
                ty: binding_info.ty,
                span,
            }),
        }
    }

    fn generate_name(&mut self, prefix: impl Into<Ustr>) -> Ustr {
        let prefix = prefix.into();
        let entry = self.unique_name_indices.entry(prefix).or_insert(0);
        let name = ustr(&format!("{}@{}", prefix, *entry));
        *entry += 1;
        name
    }

    pub(super) fn eval(
        &mut self,
        node: &hir::Node,
        module_id: ModuleId,
        eval_span: Span,
    ) -> Result<ConstValue, Diagnostic> {
        if let Some(const_value) = node.as_const_value() {
            Ok(const_value.clone())
        } else {
            let ty = node.ty().normalize(&self.tcx);

            let eval_result = self
                .interp
                .create_session(self.workspace, &self.tcx, &self.cache)
                .eval(node, module_id);

            match eval_result {
                Ok(value) => match value.try_into_const_value(&mut self.tcx, &ty, eval_span) {
                    Ok(const_value) => Ok(const_value),
                    Err(value_str) => Err(Diagnostic::error()
                        .with_message(format!(
                            "compile-time evaluation cannot result in `{}`",
                            value_str,
                        ))
                        .with_label(Label::primary(eval_span, "evaluated here"))),
                },
                Err(diagnostics) => {
                    let (last, tail) = diagnostics.split_last().unwrap();

                    for diag in tail {
                        self.workspace.diagnostics.push(diag.clone());
                    }

                    Err(last.clone())
                }
            }
        }
    }
}

type CheckResult<T = hir::Node> = DiagnosticResult<T>;

pub(super) trait Check
where
    Self: Sized,
{
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult;
}

impl Check for ast::Binding {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let attrs = sess.check_attrs(&self.attrs, env)?;

        match &self.kind {
            ast::BindingKind::Orphan {
                pattern,
                type_expr,
                value,
                is_static,
            } => {
                let ty = check_optional_type_expr(type_expr, sess, env, pattern.span())?;

                let mut value_node = value.check(sess, env, Some(ty))?;

                value_node
                    .ty()
                    .unify(&ty, &mut sess.tcx)
                    .or_coerce_into_ty(
                        &mut value_node,
                        ty,
                        &mut sess.tcx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tcx,
                        ty,
                        type_expr.as_ref().map(|e| e.span()),
                        value_node.ty(),
                        value.span(),
                    )?;

                let binding_type = ty.normalize(&sess.tcx);

                if let Some(ConstValue::Type(_)) = value_node.as_const_value() {
                    return Err(Diagnostic::error()
                        .with_message("expected a value, got a type")
                        .with_label(Label::primary(value_node.span(), "expected a value"))
                        .with_note(
                            "if you intended to bind a type, change the `let` keyword to `type`",
                        ));
                }

                let value_is_module = binding_type.is_module();

                // Global immutable bindings must resolve to a const value, unless it is:
                // - of type `type` or `module`
                // - an extern binding
                if !is_static
                    && env.scope_level().is_global()
                    && !value_node.is_const()
                    && !value_is_module
                    && !pattern.iter().any(|p| p.is_mutable)
                {
                    return Err(Diagnostic::error()
                        .with_message(format!("immutable, top level variable must be constant"))
                        .with_label(Label::primary(pattern.span(), "must be constant"))
                        .with_label(Label::secondary(
                            value_node.span(),
                            "doesn't resolve to a constant value",
                        )));
                }

                // Bindings of type `type` and `module` cannot be assigned to mutable bindings
                if value_is_module {
                    pattern
                        .iter()
                        .filter(|pattern| pattern.is_mutable)
                        .for_each(|pattern| {
                            sess.workspace.diagnostics.push(
                                Diagnostic::error()
                                    .with_message("variable of type `{module}` must be immutable")
                                    .with_label(Label::primary(pattern.span, "variable is mutable"))
                                    .with_note("try removing the `mut` from the declaration"),
                            );
                        });
                } else if binding_type.is_unsized() {
                    sess.workspace
                        .diagnostics
                        .push(TypeError::binding_is_unsized(
                            &pattern.to_string(),
                            binding_type.display(&sess.tcx),
                            pattern.span(),
                        ))
                }

                let is_function =
                    matches!(value_node.as_const_value(), Some(ConstValue::Function(_)));

                let value_span = value_node.span();
                let (_, bound_node) = sess.bind_pattern(
                    env,
                    &pattern,
                    self.visibility,
                    ty,
                    Some(value_node),
                    if *is_static {
                        BindingInfoKind::Static
                    } else {
                        BindingInfoKind::Orphan
                    },
                    value_span,
                    if type_expr.is_some() {
                        BindingInfoFlags::IS_USER_DEFINED
                    } else {
                        BindingInfoFlags::IS_USER_DEFINED | BindingInfoFlags::TYPE_WAS_INFERRED
                    },
                )?;

                // If this binding matches the entry point function's requirements,
                // Tag it as the entry function
                // Requirements:
                // - Is declared in the root module
                // - It is in global scope
                // - Its name is the same as the required `entry_point_function_name`
                if is_function {
                    if let hir::Node::Binding(binding) = &bound_node {
                        if sess.workspace.build_options.need_entry_point_function()
                            && self.module_id == sess.workspace.root_module_id
                            && env.scope_level().is_global()
                            && binding.name
                                == sess
                                    .workspace
                                    .build_options
                                    .entry_point_function_name()
                                    .unwrap()
                        {
                            if let Some(_) = sess.workspace.entry_point_function_id {
                                // TODO: When attributes are implemented:
                                // TODO: Emit error that we can't have two entry point functions
                            } else {
                                sess.workspace.entry_point_function_id = Some(binding.id);
                            }
                        }
                    }
                }

                Ok(bound_node)
            }
            ast::BindingKind::ExternFunction {
                name: ast::NameAndSpan { name, span },
                lib,
                sig,
            } => {
                let (name, span) = (*name, *span);

                for param in sig.params.iter() {
                    if param.type_expr.is_none() {
                        return Err(Diagnostic::error()
                            .with_message("extern function must specify all of its parameter types")
                            .with_label(Label::primary(
                                param.pattern.span(),
                                "parameter type not specified",
                            )));
                    }
                }

                let function_type_node =
                    sig.check(sess, env, Some(sess.tcx.common_types.anytype))?;

                let ty = function_type_node
                    .into_const_value()
                    .unwrap()
                    .into_type()
                    .unwrap();

                let (qualified_name, function_kind, binding_kind) = match attrs
                    .get(AttrKind::Intrinsic)
                {
                    Some(_) => match hir::Intrinsic::try_from(name.as_str()) {
                        Ok(intrinsic) => {
                            if lib.is_some() {
                                return Err(Diagnostic::error()
                                    .with_message(
                                        "intrinsic function cannot have a library defined",
                                    )
                                    .with_label(Label::primary(span, "cannot define a library")));
                            }

                            (
                                get_qualified_name(env.scope_name(), name),
                                hir::FunctionKind::Intrinsic(intrinsic),
                                BindingInfoKind::Intrinsic,
                            )
                        }
                        Err(_) => {
                            return Err(Diagnostic::error()
                                .with_message(format!("unknown intrinsic function `{}`", name))
                                .with_label(Label::primary(span, "unknown intrinsic function")))
                        }
                    },
                    None => (
                        name,
                        hir::FunctionKind::Extern { lib: lib.clone() },
                        BindingInfoKind::ExternFunction,
                    ),
                };

                let function_id = sess.cache.functions.insert_with_id(hir::Function {
                    module_id: env.module_id(),
                    id: hir::FunctionId::unknown(),
                    name,
                    qualified_name,
                    kind: function_kind,
                    ty,
                    span: self.span,
                });

                let function_value = hir::Node::Const(hir::Const {
                    value: ConstValue::Function(ConstFunction {
                        id: function_id,
                        name: qualified_name,
                    }),
                    ty,
                    span,
                });

                sess.bind_name(
                    env,
                    name,
                    self.visibility,
                    ty,
                    Some(function_value),
                    false,
                    binding_kind,
                    span,
                    BindingInfoFlags::IS_USER_DEFINED,
                )
                .map(|(_, node)| node)
            }
            ast::BindingKind::ExternVariable {
                name: ast::NameAndSpan { name, span },
                lib,
                is_mutable,
                type_expr,
            } => {
                let (name, span) = (*name, *span);

                let ty = check_type_expr(type_expr, sess, env)?;

                let value = hir::Node::Const(hir::Const {
                    value: ConstValue::ExternVariable(ConstExternVariable {
                        name,
                        lib: lib.clone(),
                        ty,
                    }),
                    ty,
                    span,
                });

                sess.bind_name(
                    env,
                    name,
                    self.visibility,
                    ty,
                    Some(value),
                    *is_mutable,
                    BindingInfoKind::ExternVariable,
                    span,
                    BindingInfoFlags::IS_USER_DEFINED,
                )
                .map(|(_, node)| node)
            }
            ast::BindingKind::Type {
                name: ast::NameAndSpan { name, span },
                type_expr,
            } => {
                let (name, span) = (*name, *span);

                let type_node = type_expr.check(sess, env, Some(sess.tcx.common_types.anytype))?;

                match type_node.as_const_value() {
                    Some(ConstValue::Type(_)) => sess
                        .bind_name(
                            env,
                            name,
                            self.visibility,
                            type_node.ty(),
                            Some(type_node),
                            false,
                            BindingInfoKind::Type,
                            span,
                            BindingInfoFlags::IS_USER_DEFINED,
                        )
                        .map(|(_, node)| node),
                    _ => Err(Diagnostic::error()
                        .with_message(format!(
                            "expected a type, got a value of type `{}`",
                            type_node.ty().display(&sess.tcx)
                        ))
                        .with_label(Label::primary(type_expr.span(), "expected a type"))
                        .with_note(
                            "if you intended to bind a value, change the `type` keyword to `let`",
                        )),
                }
            }
        }
    }
}

impl Check for ast::FunctionSig {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        let mut param_types: Vec<FunctionTypeParam> = vec![];

        if !self.params.is_empty() {
            let mut defined_params = UstrMap::default();

            for param in self.params.iter() {
                let param_type =
                    check_optional_type_expr(&param.type_expr, sess, env, param.pattern.span())?;

                for pattern in param.pattern.iter() {
                    let name = pattern.name;

                    if let Some(already_defined_span) = defined_params.insert(name, pattern.span) {
                        return Err(SyntaxError::duplicate_binding(
                            already_defined_span,
                            pattern.span,
                            name,
                        ));
                    }
                }

                param_types.push(FunctionTypeParam {
                    name: match &param.pattern {
                        Pattern::Name(p) => p.name,
                        Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => ustr("_"),
                        Pattern::Hybrid(p) => p.name_pattern.name,
                    },
                    ty: param_type.as_kind(),
                });
            }
        } else {
            // if the function signature has no parameters, and the
            // parent type is a function with 1 parameter, add an implicit `it` parameter
            if let Some(ty) = expected_type {
                match ty.normalize(&sess.tcx) {
                    Type::Function(f) => {
                        if f.params.len() == 1 {
                            param_types.push(FunctionTypeParam {
                                name: ustr("it"),
                                ty: f.params[0].ty.clone(),
                            });
                        }
                    }
                    _ => (),
                }
            }
        };

        let return_type = if self.return_type.is_none() && self.kind.is_extern() {
            sess.tcx.common_types.unit
        } else {
            check_optional_type_expr(&self.return_type, sess, env, self.span)?
        };

        let varargs = if let Some(varargs) = &self.varargs {
            let ty = if varargs.type_expr.is_some() {
                let ty = check_optional_type_expr(&varargs.type_expr, sess, env, self.span)?;
                Some(ty.as_kind())
            } else {
                None
            };

            if ty.is_none() && !self.kind.is_extern() {
                return Err(Diagnostic::error()
                    .with_message("variadic parameter must be typed")
                    .with_label(Label::primary(varargs.span, "missing a type annotation")));
            }

            Some(Box::new(FunctionTypeVarargs {
                name: varargs.name.name,
                ty,
            }))
        } else {
            None
        };

        let function_type = sess.tcx.bound(
            Type::Function(FunctionType {
                params: param_types,
                return_type: Box::new(return_type.into()),
                varargs,
                kind: self.kind.clone(),
            }),
            self.span,
        );

        Ok(hir::Node::Const(hir::Const {
            value: ConstValue::Type(function_type),
            ty: sess
                .tcx
                .bound(function_type.as_kind().create_type(), self.span),
            span: self.span,
        }))
    }
}

impl Check for ast::Ast {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        match self {
            ast::Ast::Binding(binding) => binding.check(sess, env, None),
            ast::Ast::Assignment(assignment) => assignment.check(sess, env, expected_type),
            ast::Ast::Cast(cast) => cast.check(sess, env, expected_type),
            ast::Ast::Import(import) => {
                let import_path = import.path.to_str().unwrap();

                let module = sess
                    .modules
                    .iter()
                    .find(|m| m.info.file_path == import_path)
                    .unwrap_or_else(|| {
                        panic!("couldn't find ast for module with path: {}", import_path)
                    });

                let module_type = sess.check_module(module)?;

                Ok(hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty: module_type,
                    span: import.span,
                }))
            }
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::SizeOf(expr) => {
                    let ty = check_type_expr(&expr, sess, env)?;
                    let ty = ty.normalize(&sess.tcx);

                    if ty.is_unsized() {
                        Err(TypeError::type_is_unsized(
                            ty.display(&sess.tcx),
                            expr.span(),
                        ))
                    } else {
                        let size = ty.size_of(sess.target_metrics.word_size);

                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Uint(size as _),
                            ty: sess.tcx.common_types.uint,
                            span: expr.span(),
                        }))
                    }
                }
                ast::BuiltinKind::AlignOf(expr) => {
                    let ty = check_type_expr(&expr, sess, env)?;
                    let ty = ty.normalize(&sess.tcx);

                    if ty.is_unsized() {
                        Err(TypeError::type_is_unsized(
                            ty.display(&sess.tcx),
                            expr.span(),
                        ))
                    } else {
                        let align = ty.align_of(sess.target_metrics.word_size);

                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Uint(align as _),
                            ty: sess.tcx.common_types.uint,
                            span: expr.span(),
                        }))
                    }
                }
            },
            ast::Ast::Const(const_) => const_.check(sess, env, expected_type),
            ast::Ast::Function(function) => function.check(sess, env, expected_type),
            ast::Ast::While(while_) => while_.check(sess, env, expected_type),
            ast::Ast::For(for_) => for_.check(sess, env, expected_type),
            ast::Ast::Break(term) => {
                if sess.loop_depth > 0 {
                    Ok(hir::Node::Control(hir::Control::Break(hir::Empty {
                        ty: sess.tcx.common_types.never,
                        span: term.span,
                    })))
                } else {
                    Err(SyntaxError::outside_of_loop(term.span, "break"))
                }
            }
            ast::Ast::Continue(term) => {
                if sess.loop_depth > 0 {
                    Ok(hir::Node::Control(hir::Control::Continue(hir::Empty {
                        ty: sess.tcx.common_types.never,
                        span: term.span,
                    })))
                } else {
                    Err(SyntaxError::outside_of_loop(term.span, "continue"))
                }
            }
            ast::Ast::Return(return_) => return_.check(sess, env, expected_type),
            ast::Ast::If(if_) => if_.check(sess, env, expected_type),
            ast::Ast::Block(block) => block.check(sess, env, expected_type),
            ast::Ast::Binary(binary) => binary.check(sess, env, expected_type),
            ast::Ast::Unary(unary) => unary.check(sess, env, expected_type),
            ast::Ast::Subscript(sub) => {
                let uint = sess.tcx.common_types.uint;

                let mut offset_node = sub.index.check(sess, env, None)?;

                offset_node
                    .ty()
                    .unify(&uint, &mut sess.tcx)
                    .or_coerce_into_ty(
                        &mut offset_node,
                        uint,
                        &mut sess.tcx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(&sess.tcx, uint, None, offset_node.ty(), sub.index.span())?;

                let node = sub.expr.check(sess, env, None)?;
                let node_type = node.ty().normalize(&sess.tcx);

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
                    if let Type::Array(_, size) = node_type.maybe_deref_once() {
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
                        Some(const_array.values[const_index as usize].clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                let inner = match &node_type {
                    Type::Array(inner, _) => inner.as_ref(),
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Array(inner, _) | Type::Slice(inner) => inner.as_ref(),
                        inner => inner,
                    },
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "cannot index type `{}`",
                                node_type.display(&sess.tcx)
                            ))
                            .with_label(Label::primary(sub.expr.span(), "cannot index")))
                    }
                };

                let ty = sess.tcx.bound(inner.clone(), sub.span);

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
                        index: Box::new(offset_node),
                    })))
                }
            }
            ast::Ast::Slice(slice) => {
                let uint = sess.tcx.common_types.uint;

                let node = slice.expr.check(sess, env, None)?;
                let node_type = node.ty().normalize(&sess.tcx);

                let low_node = if let Some(low) = &slice.low {
                    let mut low_node = low.check(sess, env, None)?;

                    low_node
                        .ty()
                        .unify(&uint, &mut sess.tcx)
                        .or_coerce_into_ty(
                            &mut low_node,
                            uint,
                            &mut sess.tcx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(&sess.tcx, uint, None, low_node.ty(), low_node.span())?;

                    low_node
                } else {
                    hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(0),
                        ty: uint,
                        span: slice.span,
                    })
                };

                let high_node = if let Some(high) = &slice.high {
                    let mut high_node = high.check(sess, env, None)?;

                    high_node
                        .ty()
                        .unify(&uint, &mut sess.tcx)
                        .or_coerce_into_ty(
                            &mut high_node,
                            uint,
                            &mut sess.tcx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tcx,
                            uint,
                            slice.low.as_ref().map(|e| e.span()),
                            high_node.ty(),
                            high_node.span(),
                        )?;

                    high_node
                } else {
                    match &node_type {
                        Type::Array(_, size) => hir::Node::Const(hir::Const {
                            value: ConstValue::Uint(*size as _),
                            ty: uint,
                            span: slice.span,
                        }),
                        Type::Pointer(inner, _) => match inner.as_ref() {
                            Type::Slice(..) => hir::Node::MemberAccess(hir::MemberAccess {
                                value: Box::new(node.clone()),
                                ty: uint,
                                span: slice.span,
                                member_name: ustr(BUILTIN_FIELD_LEN),
                                member_index: 1,
                            }),
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(
                                        "slicing a pointer requires specifying the end index",
                                    )
                                    .with_label(Label::primary(
                                        slice.span,
                                        "must specify end index",
                                    )))
                            }
                        },
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot slice type `{}`",
                                    node_type.display(&sess.tcx)
                                ))
                                .with_label(Label::primary(slice.expr.span(), "cannot slice")))
                        }
                    }
                };

                let inner_type = match node_type {
                    Type::Array(inner, ..) => inner,
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(inner) => inner.clone(),
                        _ => {
                            if slice.high.is_none() {
                                return Err(Diagnostic::error()
                                    .with_message("pointer has an unknown length, ending index must be specified")
                                    .with_label(Label::primary(
                                        slice.expr.span(),
                                        "pointer has an unknown length",
                                    )));
                            }

                            inner
                        }
                    },
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "cannot slice type `{}`",
                                node_type.display(&sess.tcx)
                            ))
                            .with_label(Label::primary(slice.expr.span(), "cannot slice")))
                    }
                };

                let ty = sess.tcx.bound(Type::Slice(inner_type), slice.span);

                Ok(hir::Node::Builtin(hir::Builtin::Slice(hir::Slice {
                    ty,
                    span: slice.span,
                    value: Box::new(node),
                    low: Box::new(low_node),
                    high: Box::new(high_node),
                })))
            }
            ast::Ast::Call(call) => call.check(sess, env, expected_type),
            ast::Ast::MemberAccess(access) => {
                let node = access.expr.check(sess, env, None)?;

                let member_tuple_index = access.member.as_str().parse::<usize>();

                // if the accessed expression's type is not resolved yet - try unifying it with a partial type
                match node.ty().normalize(&sess.tcx) {
                    Type::Var(_)
                    | Type::Infer(_, InferType::PartialStruct(_))
                    | Type::Infer(_, InferType::PartialTuple(_)) => {
                        // if this parsing operation succeeds, this is a tuple member access - `tup.0`
                        // otherwise, this is a struct field access - `strct.field`

                        let member_ty = sess.tcx.var(access.span);
                        let partial_ty = match member_tuple_index {
                            Ok(index) => {
                                let elements = repeat_with(|| sess.tcx.var(access.span))
                                    .take(index + 1)
                                    .map(Type::Var)
                                    .collect::<Vec<Type>>();
                                sess.tcx.partial_tuple(elements, access.span)
                            }
                            Err(_) => {
                                let fields =
                                    IndexMap::from([(access.member, Type::Var(member_ty))]);
                                sess.tcx
                                    .partial_struct(PartialStructType(fields), access.span)
                            }
                        };

                        node.ty().unify(&partial_ty, &mut sess.tcx).or_report_err(
                            &sess.tcx,
                            partial_ty,
                            None,
                            node.ty(),
                            access.expr.span(),
                        )?;
                    }
                    _ => (),
                }

                let node_type = node.ty().normalize(&sess.tcx);

                match &node_type {
                    Type::Pointer(inner, is_mutable) => match inner.as_ref() {
                        Type::Slice(inner) => {
                            if access.member.as_str() == BUILTIN_FIELD_LEN {
                                let ty = sess.tcx.common_types.uint;

                                if let Some(ConstValue::Str(s)) = node.as_const_value() {
                                    return Ok(hir::Node::Const(hir::Const {
                                        value: ConstValue::Uint(s.len() as _),
                                        ty,
                                        span: access.span,
                                    }));
                                } else {
                                    return Ok(hir::Node::MemberAccess(hir::MemberAccess {
                                        ty,
                                        span: access.span,
                                        value: Box::new(node),
                                        member_name: access.member,
                                        member_index: 1,
                                    }));
                                }
                            } else if access.member.as_str() == BUILTIN_FIELD_DATA {
                                return Ok(hir::Node::MemberAccess(hir::MemberAccess {
                                    value: Box::new(node),
                                    member_name: access.member,
                                    member_index: 0,
                                    ty: sess.tcx.bound(
                                        Type::Pointer(inner.clone(), *is_mutable),
                                        access.span,
                                    ),
                                    span: access.span,
                                }));
                            }
                        }
                        _ => (),
                    },
                    Type::Module(module_id) => {
                        let id = sess.check_top_level_binding(
                            CallerInfo {
                                module_id: env.module_id(),
                                span: access.span,
                            },
                            *module_id,
                            access.member,
                        )?;

                        return Ok(sess.id_or_const_by_id(id, access.span));
                    }
                    _ => (),
                }

                // Note (Ron): If the accessed value is a pointer, we auto dereference it.
                let node = if node_type.is_pointer() {
                    hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                        ty: sess
                            .tcx
                            .bound(node_type.maybe_deref_once().clone(), node.span()),
                        span: node.span(),
                        value: Box::new(node),
                    }))
                } else {
                    node
                };

                match &node_type.maybe_deref_once() {
                    ty @ Type::Tuple(elements)
                    | ty @ Type::Infer(_, InferType::PartialTuple(elements)) => {
                        match member_tuple_index {
                            Ok(index) => match elements.get(index) {
                                Some(field_ty) => {
                                    let ty = sess.tcx.bound(field_ty.clone(), access.span);

                                    if let Some(ConstValue::Tuple(const_elements)) =
                                        node.as_const_value()
                                    {
                                        Ok(hir::Node::Const(hir::Const {
                                            value: const_elements[index].value.clone(),
                                            ty,
                                            span: access.span,
                                        }))
                                    } else {
                                        // TODO: The index here *could be wrong*.
                                        // TODO: We need to test this to make sure there aren't messing anything here
                                        Ok(hir::Node::MemberAccess(hir::MemberAccess {
                                            ty,
                                            span: access.span,
                                            value: Box::new(node),
                                            member_name: access.member,
                                            member_index: index as _,
                                        }))
                                    }
                                }
                                None => Err(TypeError::tuple_field_out_of_bounds(
                                    access.expr.span(),
                                    &access.member,
                                    ty.display(&sess.tcx),
                                    elements.len() - 1,
                                )),
                            },
                            Err(_) => Err(TypeError::non_numeric_tuple_field(
                                access.expr.span(),
                                &access.member,
                                ty.display(&sess.tcx),
                            )),
                        }
                    }
                    ty @ Type::Struct(st) => match st.find_field_full(access.member) {
                        Some((index, field)) => {
                            let ty = sess.tcx.bound(field.ty.clone(), access.span);

                            if let Some(ConstValue::Struct(const_fields)) = node.as_const_value() {
                                Ok(hir::Node::Const(hir::Const {
                                    value: const_fields[&field.name].value.clone(),
                                    ty,
                                    span: access.span,
                                }))
                            } else {
                                // TODO: The index here *could be wrong*.
                                // TODO: We need to test this to make sure there aren't messing anything here
                                Ok(hir::Node::MemberAccess(hir::MemberAccess {
                                    ty,
                                    span: access.span,
                                    value: Box::new(node),
                                    member_name: access.member,
                                    member_index: index as _,
                                }))
                            }
                        }
                        None => Err(TypeError::invalid_struct_field(
                            access.expr.span(),
                            access.member,
                            ty.display(&sess.tcx),
                        )),
                    },
                    ty @ Type::Infer(_, InferType::PartialStruct(partial_struct)) => {
                        match partial_struct.get_full(&access.member) {
                            Some((index, _, field_ty)) => {
                                let ty = sess.tcx.bound(field_ty.clone(), access.span);

                                if let Some(ConstValue::Struct(const_fields)) =
                                    node.as_const_value()
                                {
                                    Ok(hir::Node::Const(hir::Const {
                                        value: const_fields[&access.member].value.clone(),
                                        ty,
                                        span: access.span,
                                    }))
                                } else {
                                    // TODO: The index here *could be wrong*.
                                    // TODO: We need to test this to make sure there aren't messing anything here
                                    Ok(hir::Node::MemberAccess(hir::MemberAccess {
                                        ty,
                                        span: access.span,
                                        value: Box::new(node),
                                        member_name: access.member,
                                        member_index: index as _,
                                    }))
                                }
                            }
                            None => Err(TypeError::invalid_struct_field(
                                access.expr.span(),
                                access.member,
                                ty.display(&sess.tcx),
                            )),
                        }
                    }
                    Type::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Uint(*size as _),
                            ty: sess.tcx.common_types.uint,
                            span: access.span,
                        }))
                    }
                    ty => Err(Diagnostic::error()
                        .with_message(format!(
                            "type `{}` has no member `{}`",
                            ty.display(&sess.tcx),
                            access.member
                        ))
                        .with_label(Label::primary(access.expr.span(), ""))),
                }
            }
            ast::Ast::Ident(ident) => {
                if let Some(id) = env.find_function(ident.name) {
                    let function = sess.cache.functions.get(id).unwrap();

                    Ok(hir::Node::Const(hir::Const {
                        value: ConstValue::Function(ConstFunction {
                            id: hir::FunctionId::from(function.id.inner()),
                            name: function.qualified_name,
                        }),
                        ty: function.ty,
                        span: ident.span,
                    }))
                } else {
                    match sess.get_binding_id(env, ident.name) {
                        Some(id) => {
                            // this is a local binding

                            sess.workspace.add_binding_info_use(id, ident.span);

                            let binding_info = sess.workspace.binding_infos.get(id).unwrap();

                            if binding_info.const_value.is_none() {
                                let binding_ty = binding_info.ty.normalize(&sess.tcx);

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
                            }

                            Ok(sess.id_or_const_by_id(id, ident.span))
                        }
                        None => {
                            // this is either a top level binding, a builtin binding, or it doesn't exist
                            let id = sess.check_top_level_binding(
                                CallerInfo {
                                    module_id: env.module_id(),
                                    span: ident.span,
                                },
                                env.module_id(),
                                ident.name,
                            )?;

                            Ok(sess.id_or_const_by_id(id, ident.span))
                        }
                    }
                }
            }
            ast::Ast::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(elements) => {
                    let element_ty_span = elements.first().map_or(lit.span, |e| e.span());
                    let element_ty = sess.tcx.var(element_ty_span);

                    let mut element_nodes: Vec<hir::Node> = vec![];

                    for el in elements.iter() {
                        let mut node = el.check(sess, env, Some(element_ty))?;

                        node.ty()
                            .unify(&element_ty, &mut sess.tcx)
                            .or_coerce_into_ty(
                                &mut node,
                                element_ty,
                                &mut sess.tcx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(
                                &sess.tcx,
                                element_ty,
                                Some(element_ty_span),
                                node.ty(),
                                el.span(),
                            )?;

                        element_nodes.push(node);
                    }

                    let ty = sess.tcx.bound(
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
                        .tcx
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
            ast::Ast::TupleLiteral(lit) => lit.check(sess, env, expected_type),
            ast::Ast::StructLiteral(lit) => match &lit.type_expr {
                Some(type_expr) => {
                    let node = type_expr.check(sess, env, Some(sess.tcx.common_types.anytype))?;
                    let ty = sess.extract_const_type(&node)?;

                    let kind = ty.normalize(&sess.tcx);

                    match kind {
                        Type::Struct(struct_ty) => {
                            check_named_struct_literal(sess, env, struct_ty, &lit.fields, lit.span)
                        }
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "type `{}` does not support struct initialization syntax",
                                    ty.display(&sess.tcx)
                                ))
                                .with_label(Label::primary(type_expr.span(), "not a struct type")))
                        }
                    }
                }
                None => match expected_type {
                    Some(ty) => {
                        let kind = ty.normalize(&sess.tcx);
                        match kind {
                            Type::Struct(struct_ty) => check_named_struct_literal(
                                sess,
                                env,
                                struct_ty,
                                &lit.fields,
                                lit.span,
                            ),
                            _ => check_anonymous_struct_literal(sess, env, &lit.fields, lit.span),
                        }
                    }
                    None => check_anonymous_struct_literal(sess, env, &lit.fields, lit.span),
                },
            },
            ast::Ast::Literal(lit) => {
                let const_value: ConstValue = lit.kind.into();

                let ty = match &lit.kind {
                    ast::LiteralKind::Nil => sess.tcx.var(lit.span),
                    ast::LiteralKind::Bool(_) => sess.tcx.common_types.bool,
                    ast::LiteralKind::Int(_) => sess.tcx.anyint(lit.span),
                    ast::LiteralKind::Float(_) => sess.tcx.anyfloat(lit.span),
                    ast::LiteralKind::Str(_) => sess.tcx.common_types.str,
                    ast::LiteralKind::Char(_) => sess.tcx.common_types.u8,
                };

                Ok(hir::Node::Const(hir::Const {
                    ty,
                    span: lit.span,
                    value: const_value,
                }))
            }
            ast::Ast::PointerType(ast::PointerType {
                inner,
                is_mutable,
                span,
                ..
            }) => {
                let inner_type = check_type_expr(inner, sess, env)?;
                let ptr_type = Type::Pointer(Box::new(inner_type.into()), *is_mutable);

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tcx.bound(ptr_type.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tcx.bound(ptr_type, *span)),
                }))
            }
            ast::Ast::ArrayType(ast::ArrayType {
                inner, size, span, ..
            }) => {
                let inner_type = check_type_expr(inner, sess, env)?;

                let size_node = size.check(sess, env, None)?;
                let size_value = sess.extract_const_int(&size_node)?;

                if size_value < 0 {
                    return Err(TypeError::negative_array_len(size.span(), size_value));
                }

                let inner_type_norm = inner_type.normalize(&sess.tcx);
                if inner_type_norm.is_unsized() {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "the size of type `{}` cannot be known at compile-time",
                            inner_type_norm.display(&sess.tcx)
                        ))
                        .with_label(Label::primary(
                            inner.span(),
                            "doesn't have a size known at compile-time",
                        ))
                        .with_note("array element's size must be known at compile-time"));
                }

                let array_type = Type::Array(Box::new(inner_type.into()), size_value as usize);

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tcx.bound(array_type.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tcx.bound(array_type, *span)),
                }))
            }
            ast::Ast::SliceType(ast::SliceType { inner, span, .. }) => {
                let inner_type = check_type_expr(inner, sess, env)?;

                let inner_type_norm = inner_type.normalize(&sess.tcx);
                if inner_type_norm.is_unsized() {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "the size of type `{}` cannot be known at compile-time",
                            inner_type_norm.display(&sess.tcx)
                        ))
                        .with_label(Label::primary(
                            inner.span(),
                            "doesn't have a size known at compile-time",
                        ))
                        .with_note("slice element's size must be known at compile-time"));
                }

                let slice_type = Type::Slice(Box::new(inner_type.into()));

                Ok(hir::Node::Const(hir::Const {
                    ty: sess.tcx.bound(slice_type.clone().create_type(), *span),
                    span: *span,
                    value: ConstValue::Type(sess.tcx.bound(slice_type, *span)),
                }))
            }
            ast::Ast::StructType(struct_type) => struct_type.check(sess, env, expected_type),
            ast::Ast::FunctionType(sig) => {
                let node = sig.check(sess, env, Some(sess.tcx.common_types.anytype))?;
                let function_type = node.ty().normalize(&sess.tcx).into_type().into_function();

                for (i, param) in function_type.params.iter().enumerate() {
                    let ty = param.ty.normalize(&sess.tcx);

                    if ty.is_unsized() {
                        let pattern = &sig.params[i].pattern;

                        sess.workspace
                            .diagnostics
                            .push(TypeError::binding_is_unsized(
                                &pattern.to_string(),
                                ty.display(&sess.tcx),
                                pattern.span(),
                            ))
                    }
                }

                let return_type_norm = function_type.return_type.normalize(&sess.tcx);
                if return_type_norm.is_unsized() {
                    sess.workspace.diagnostics.push(TypeError::type_is_unsized(
                        return_type_norm.display(&sess.tcx),
                        sig.return_type
                            .as_ref()
                            .map(|t| t.span())
                            .unwrap_or(sig.span),
                    ))
                }

                if let Some(varargs) = &function_type.varargs {
                    if let Some(varargs_type) = &varargs.ty {
                        let varargs_type = varargs_type.normalize(&sess.tcx);

                        if varargs_type.is_unsized() {
                            sess.workspace.diagnostics.push(TypeError::type_is_unsized(
                                varargs_type.display(&sess.tcx),
                                sig.varargs.as_ref().unwrap().span,
                            ))
                        }
                    }
                }

                for param in sig.params.iter() {
                    match &param.pattern {
                        Pattern::Name(_) => (),
                        Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) | Pattern::Hybrid(_) => {
                            return Err(Diagnostic::error()
                                .with_message("expected an indentifier or _")
                                .with_label(Label::primary(
                                    param.pattern.span(),
                                    "expected an identifier or _",
                                ))
                                .with_note(
                                    "binding patterns are not allowed in function type parameters",
                                ))
                        }
                    }
                }

                Ok(node)
            }
            ast::Ast::SelfType(expr) => match sess.self_types.last() {
                Some(&ty) => {
                    let ty = sess.tcx.bound(ty.as_kind().create_type(), expr.span);

                    Ok(hir::Node::Const(hir::Const {
                        ty,
                        span: expr.span,
                        value: ConstValue::Type(ty),
                    }))
                }
                None => Err(Diagnostic::error()
                    .with_message("Self is only available within struct types")
                    .with_label(Label::primary(expr.span, "invalid Self"))),
            },
            ast::Ast::Placeholder(expr) => {
                let ty = sess.tcx.var(expr.span);

                Ok(hir::Node::Const(hir::Const {
                    ty,
                    span: expr.span,
                    value: ConstValue::Type(ty),
                }))
            }
            ast::Ast::Error(expr) => Ok(hir::Node::noop(sess.tcx.var(expr.span), expr.span)),
        }
    }
}

impl Check for ast::While {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let bool_type = sess.tcx.common_types.bool;

        let mut condition_node = self.condition.check(sess, env, Some(bool_type))?;

        condition_node
            .ty()
            .unify(&bool_type, &mut sess.tcx)
            .or_coerce_into_ty(
                &mut condition_node,
                bool_type,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tcx,
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
            Some(ConstValue::Bool(true)) => sess.tcx.common_types.never,
            _ => sess.tcx.common_types.unit,
        };

        Ok(hir::Node::Control(hir::Control::While(hir::While {
            condition: Box::new(condition_node),
            body: Box::new(block_node),
            ty: while_node_type,
            span: self.span,
        })))
    }
}

impl Check for ast::For {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let index_type = sess.tcx.common_types.uint;
        let bool_type = sess.tcx.common_types.bool;
        let unit_type = sess.tcx.common_types.unit;

        match &self.iterator {
            ast::ForIter::Range(start, end) => {
                // Lowers the for loop into the following loop:
                //
                // -- Ast --
                // for x, i in 0..10 {
                //     printf("%d %d\n", i, x);
                // }
                //
                // -- Hir --
                // {
                //     let mut i = 0;
                //     let mut x = 0;
                //     while i <= 10 {
                //         printf("%d %d\n", i, x);
                //         i += 1;
                //         x += 1;
                //     }
                // }

                let mut start_node = start.check(sess, env, None)?;
                let mut end_node = end.check(sess, env, None)?;

                let anyint = sess.tcx.anyint(start.span());

                start_node
                    .ty()
                    .unify(&anyint, &mut sess.tcx)
                    .or_report_err(&sess.tcx, anyint, None, start_node.ty(), start_node.span())?;

                start_node
                    .ty()
                    .unify(&end_node.ty(), &mut sess.tcx)
                    .or_coerce(
                        &mut start_node,
                        &mut end_node,
                        &mut sess.tcx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tcx,
                        start_node.ty(),
                        Some(start.span()),
                        end_node.ty(),
                        end.span(),
                    )?;

                env.push_scope(ScopeKind::Loop);
                sess.loop_depth += 1;

                let mut statements: Vec<hir::Node> = vec![];

                // let mut index = 0
                let index_binding =
                    self.index_binding
                        .clone()
                        .unwrap_or_else(|| ast::NameAndSpan {
                            name: sess.generate_name("index"),
                            span: self.span,
                        });

                let (index_id, index_binding) = sess.bind_name(
                    env,
                    index_binding.name,
                    ast::Visibility::Private,
                    index_type,
                    Some(hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(0),
                        ty: index_type,
                        span: index_binding.span,
                    })),
                    false,
                    BindingInfoKind::Orphan,
                    index_binding.span,
                    if self.index_binding.is_some() {
                        BindingInfoFlags::IS_USER_DEFINED
                            | BindingInfoFlags::TYPE_WAS_INFERRED
                            | BindingInfoFlags::NO_CONST_FOLD
                    } else {
                        BindingInfoFlags::NO_CONST_FOLD
                    },
                )?;

                statements.push(index_binding);

                let iter_type = start_node.ty();

                // let mut iter = start
                let (iter_id, iter_binding) = sess.bind_name(
                    env,
                    self.iter_binding.name,
                    ast::Visibility::Private,
                    iter_type,
                    Some(start_node),
                    false,
                    BindingInfoKind::Orphan,
                    self.iter_binding.span,
                    BindingInfoFlags::IS_USER_DEFINED
                        | BindingInfoFlags::TYPE_WAS_INFERRED
                        | BindingInfoFlags::NO_CONST_FOLD,
                )?;

                statements.push(iter_binding);

                let index_id_node = hir::Node::Id(hir::Id {
                    id: index_id,
                    ty: index_type,
                    span: self.span,
                });

                let iter_id_node = hir::Node::Id(hir::Id {
                    id: iter_id,
                    ty: iter_type,
                    span: self.span,
                });

                // iter <= end
                let condition = hir::Node::Builtin(hir::Builtin::Le(hir::Binary {
                    ty: bool_type,
                    span: self.span,
                    lhs: Box::new(iter_id_node.clone()),
                    rhs: Box::new(end_node),
                }));

                // loop block { ... }
                let mut block_node = self.block.check(sess, env, None)?.into_sequence().unwrap();

                // index += 1
                block_node
                    .statements
                    .push(hir::Node::Assignment(hir::Assignment {
                        lhs: Box::new(index_id_node.clone()),
                        rhs: Box::new(hir::Node::Builtin(hir::Builtin::Add(hir::Binary {
                            ty: index_type,
                            span: self.span,
                            lhs: Box::new(index_id_node),
                            rhs: Box::new(hir::Node::Const(hir::Const {
                                value: ConstValue::Uint(1),
                                ty: index_type,
                                span: self.span,
                            })),
                        }))),
                        ty: unit_type,
                        span: self.span,
                    }));

                // iter += 1
                block_node
                    .statements
                    .push(hir::Node::Assignment(hir::Assignment {
                        lhs: Box::new(iter_id_node.clone()),
                        rhs: Box::new(hir::Node::Builtin(hir::Builtin::Add(hir::Binary {
                            ty: iter_type,
                            span: self.span,
                            lhs: Box::new(iter_id_node),
                            rhs: Box::new(hir::Node::Const(hir::Const {
                                value: ConstValue::Uint(1),
                                ty: iter_type,
                                span: self.span,
                            })),
                        }))),
                        ty: unit_type,
                        span: self.span,
                    }));

                sess.loop_depth -= 1;
                env.pop_scope();

                statements.push(hir::Node::Control(hir::Control::While(hir::While {
                    condition: Box::new(condition),
                    body: Box::new(hir::Node::Sequence(block_node)),
                    ty: unit_type,
                    span: self.span,
                })));

                Ok(hir::Node::Sequence(hir::Sequence {
                    statements,
                    ty: unit_type,
                    span: self.span,
                    is_block: false,
                }))
            }
            ast::ForIter::Value(value) => {
                // Lowers the for loop into the following loop:
                //
                // -- Ast --
                // for x, i in value {
                //     printf("%d %d\n", i, x);
                // }

                // -- Hir --
                // {
                //     let mut i = 0;
                //     while i < value.len() {
                //         let x = value[i];
                //         printf("%d %d\n", i, x);
                //         i += 1;
                //     }
                // }

                let value_node = value.check(sess, env, None)?;
                let (value_type, value_span) = (value_node.ty(), value_node.span());

                let value_node_type = value_node.ty().normalize(&sess.tcx);

                let inner = match &value_node_type {
                    Type::Array(inner, _) => inner.clone(),
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(inner) | Type::Array(inner, _) => inner.clone(),
                        _ => {
                            // TODO: duplicate error
                            return Err(Diagnostic::error()
                                .with_message(format!("can't iterate over `{}`", value_node_type))
                                .with_label(Label::primary(value.span(), "can't iterate")));
                        }
                    },
                    _ => {
                        // TODO: duplicate error
                        return Err(Diagnostic::error()
                            .with_message(format!("can't iterate over `{}`", value_node_type))
                            .with_label(Label::primary(value.span(), "can't iterate")));
                    }
                };

                env.push_scope(ScopeKind::Loop);
                sess.loop_depth += 1;

                let mut statements: Vec<hir::Node> = vec![];

                // bind the value to a local variable, so we don't have to evaluate the expression every loop
                let value_name = sess.generate_name("iterator");
                let (value_id, value_binding) = sess.bind_name(
                    env,
                    value_name,
                    ast::Visibility::Private,
                    value_type,
                    Some(value_node),
                    false,
                    BindingInfoKind::Orphan,
                    value_span,
                    BindingInfoFlags::empty(),
                )?;

                statements.push(value_binding);

                let value_id_node = hir::Node::Id(hir::Id {
                    id: value_id,
                    ty: value_type,
                    span: value_span,
                });

                // let mut index = 0
                let index_binding =
                    self.index_binding
                        .clone()
                        .unwrap_or_else(|| ast::NameAndSpan {
                            name: sess.generate_name("index"),
                            span: self.span,
                        });

                let (index_id, index_binding) = sess.bind_name(
                    env,
                    index_binding.name,
                    ast::Visibility::Private,
                    index_type,
                    Some(hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(0),
                        ty: index_type,
                        span: index_binding.span,
                    })),
                    false,
                    BindingInfoKind::Orphan,
                    index_binding.span,
                    if self.index_binding.is_some() {
                        BindingInfoFlags::IS_USER_DEFINED
                            | BindingInfoFlags::TYPE_WAS_INFERRED
                            | BindingInfoFlags::NO_CONST_FOLD
                    } else {
                        BindingInfoFlags::NO_CONST_FOLD
                    },
                )?;

                statements.push(index_binding);

                let index_id_node = hir::Node::Id(hir::Id {
                    id: index_id,
                    ty: index_type,
                    span: self.span,
                });

                // index <= value.len
                let value_len_node = match &value_node_type {
                    Type::Array(_, size) => hir::Node::Const(hir::Const {
                        value: ConstValue::Uint(*size as _),
                        ty: index_type,
                        span: self.span,
                    }),
                    // This must be a Pointer(Slice)
                    _ => hir::Node::MemberAccess(hir::MemberAccess {
                        value: Box::new(value_id_node.clone()),
                        member_name: ustr("len"),
                        member_index: 1,
                        ty: index_type,
                        span: self.span,
                    }),
                };

                let condition = hir::Node::Builtin(hir::Builtin::Lt(hir::Binary {
                    ty: bool_type,
                    span: self.span,
                    lhs: Box::new(index_id_node.clone()),
                    rhs: Box::new(value_len_node),
                }));

                // bind before block is checked: let iter = value[index]
                let iter_type = sess
                    .tcx
                    .bound(inner.as_ref().clone(), self.iter_binding.span);

                let (_, iter_binding) = sess.bind_name(
                    env,
                    self.iter_binding.name,
                    ast::Visibility::Private,
                    iter_type,
                    Some(hir::Node::Builtin(hir::Builtin::Offset(hir::Offset {
                        value: Box::new(value_id_node),
                        index: Box::new(index_id_node.clone()),
                        ty: iter_type,
                        span: self.span,
                    }))),
                    false,
                    BindingInfoKind::Orphan,
                    self.iter_binding.span,
                    BindingInfoFlags::IS_USER_DEFINED
                        | BindingInfoFlags::TYPE_WAS_INFERRED
                        | BindingInfoFlags::NO_CONST_FOLD,
                )?;

                // loop block { ... }
                let mut block_node = self.block.check(sess, env, None)?.into_sequence().unwrap();

                // let iter = value[index]
                block_node.statements.insert(0, iter_binding);

                // index += 1
                block_node
                    .statements
                    .push(hir::Node::Assignment(hir::Assignment {
                        lhs: Box::new(index_id_node.clone()),
                        rhs: Box::new(hir::Node::Builtin(hir::Builtin::Add(hir::Binary {
                            ty: index_type,
                            span: self.span,
                            lhs: Box::new(index_id_node),
                            rhs: Box::new(hir::Node::Const(hir::Const {
                                value: ConstValue::Uint(1),
                                ty: index_type,
                                span: self.span,
                            })),
                        }))),
                        ty: unit_type,
                        span: self.span,
                    }));

                sess.loop_depth -= 1;
                env.pop_scope();

                statements.push(hir::Node::Control(hir::Control::While(hir::While {
                    condition: Box::new(condition),
                    body: Box::new(hir::Node::Sequence(block_node)),
                    ty: unit_type,
                    span: self.span,
                })));

                Ok(hir::Node::Sequence(hir::Sequence {
                    statements,
                    ty: unit_type,
                    span: self.span,
                    is_block: false,
                }))
            }
        }
    }
}

impl Check for ast::Const {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        // Note (Ron 02/07/2022):
        // The inner expression of `const` isn't allowed to capture its outer environment yet.
        // TODO: Running arbitrary should code require these preconditions to be met:
        //       1. All types are at least partially inferred
        //       2. All types in all memory locations are sized
        let node = sess.with_env(env.module_id(), |sess, mut env| {
            self.expr.check(sess, &mut env, None)
        })?;

        if sess.workspace.build_options.check_mode {
            Ok(node)
        } else {
            let value = sess.eval(&node, env.module_id(), self.span)?;

            Ok(hir::Node::Const(hir::Const {
                value,
                ty: node.ty(),
                span: self.span,
            }))
        }
    }
}

impl Check for ast::Function {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        let name = self.sig.name;
        let qualified_name = get_qualified_name(env.scope_name(), name);

        let sig_node = self.sig.check(sess, env, expected_type)?;

        let sig_type = sess.extract_const_type(&sig_node)?;
        let function_type = sig_type.normalize(&sess.tcx).into_function();

        let return_type = sess.tcx.bound(
            function_type.return_type.as_ref().clone(),
            self.sig
                .return_type
                .as_ref()
                .map_or(self.sig.span, |e| e.span()),
        );

        let return_type_span = self
            .sig
            .return_type
            .as_ref()
            .map_or(self.sig.span, |e| e.span());

        env.push_scope(ScopeKind::Function);

        let mut params: Vec<hir::FunctionParam> = vec![];
        let mut param_bind_statements: Vec<hir::Node> = vec![];

        for (index, param_type) in function_type.params.iter().enumerate() {
            match self.sig.params.get(index) {
                Some(param) => {
                    let ty = sess.tcx.bound(
                        param_type.ty.clone(),
                        param
                            .type_expr
                            .as_ref()
                            .map_or(param.pattern.span(), |e| e.span()),
                    );

                    let (bound_id, bound_node) = sess.bind_pattern(
                        env,
                        &param.pattern,
                        ast::Visibility::Private,
                        ty,
                        None,
                        BindingInfoKind::Orphan,
                        param.pattern.span(),
                        if param.type_expr.is_some() {
                            BindingInfoFlags::IS_USER_DEFINED
                        } else {
                            BindingInfoFlags::IS_USER_DEFINED | BindingInfoFlags::TYPE_WAS_INFERRED
                        },
                    )?;

                    params.push(hir::FunctionParam {
                        id: bound_id,
                        ty,
                        span: param.pattern.span(),
                    });

                    // If this is a single statement, we ignore it,
                    // As it doesn't include any destructuring statements.
                    match bound_node.into_sequence() {
                        Ok(sequence) => param_bind_statements.extend(sequence.statements),
                        Err(_) => (),
                    }
                }
                None => {
                    // This parameter was inserted implicitly
                    let span = self.sig.span;
                    let ty = sess.tcx.bound(param_type.ty.clone(), span);

                    let (bound_id, bound_node) = sess.bind_name(
                        env,
                        param_type.name,
                        ast::Visibility::Private,
                        ty,
                        None,
                        false,
                        BindingInfoKind::Orphan,
                        span,
                        BindingInfoFlags::IS_IMPLICIT_IT_FN_PARAMETER,
                    )?;

                    params.push(hir::FunctionParam {
                        id: bound_id,
                        ty,
                        span,
                    });

                    // If this is a single statement, we ignore it,
                    // As it doesn't include any destructuring statements.
                    match bound_node.into_sequence() {
                        Ok(sequence) => param_bind_statements.extend(sequence.statements),
                        Err(_) => (),
                    }
                }
            }
        }

        if let Some(varargs) = &function_type.varargs {
            if varargs.ty.is_none() {
                return Err(Diagnostic::error()
                    .with_message("untyped variadic parameters are only valid in extern functions")
                    .with_label(Label::primary(
                        self.sig.varargs.as_ref().unwrap().span,
                        "variadic parameter must be typed",
                    )));
            }
        }

        let function_id = sess.cache.functions.insert_with_id(hir::Function {
            id: hir::FunctionId::unknown(),
            module_id: env.module_id(),
            name,
            qualified_name,
            kind: hir::FunctionKind::Orphan {
                params: params.clone(),
                inferred_return_type_span: if self.sig.return_type.is_some() {
                    None
                } else {
                    Some(self.sig.span)
                },
                body: None,
            },
            ty: sig_type,
            span: self.span,
        });

        env.insert_function(name, function_id);

        let mut body_node = sess
            .with_function_frame(
                FunctionFrame {
                    return_type,
                    return_type_span,
                    scope_level: env.scope_level(),
                },
                |sess| self.body.check(sess, env, Some(return_type)),
            )?
            .into_sequence()
            .unwrap();

        param_bind_statements.append(&mut body_node.statements);
        body_node.statements = param_bind_statements;

        let mut unify_node = body_node.ty.unify(&return_type, &mut sess.tcx);

        if let Some(last_statement) = body_node.statements.last_mut() {
            unify_node = unify_node.or_coerce_into_ty(
                last_statement,
                return_type,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            );
        }

        unify_node.or_report_err(
            &sess.tcx,
            return_type,
            Some(return_type_span),
            body_node.ty,
            self.body.span,
        )?;

        env.pop_scope();

        // Check that all parameter types are sized
        for param in params.iter() {
            let ty = param.ty.normalize(&sess.tcx);

            if ty.is_unsized() {
                let binding_info = sess.workspace.binding_infos.get(param.id).unwrap();

                sess.workspace
                    .diagnostics
                    .push(TypeError::binding_is_unsized(
                        &binding_info.name,
                        ty.display(&sess.tcx),
                        binding_info.span,
                    ))
            }
        }

        // Check that the return type is sized
        let return_type_norm = return_type.normalize(&sess.tcx);
        if return_type_norm.is_unsized() {
            sess.workspace.diagnostics.push(TypeError::type_is_unsized(
                return_type_norm.display(&sess.tcx),
                return_type_span,
            ))
        }

        // Check that the variadic argument is sized
        if let Some(varargs) = &function_type.varargs {
            if let Some(varargs_type) = &varargs.ty {
                let varargs_type = varargs_type.normalize(&sess.tcx);

                if varargs_type.is_unsized() {
                    sess.workspace.diagnostics.push(TypeError::type_is_unsized(
                        varargs_type.display(&sess.tcx),
                        self.sig.varargs.as_ref().unwrap().span,
                    ))
                }
            }
        }

        sess.cache
            .functions
            .get_mut(function_id)
            .unwrap()
            .set_body(body_node);

        Ok(hir::Node::Const(hir::Const {
            value: ConstValue::Function(ConstFunction {
                id: function_id,
                name,
            }),
            ty: sig_type,
            span: self.span,
        }))
    }
}

impl Check for ast::Assignment {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        sess.in_lvalue_context = true;
        let lhs_node = self.lhs.check(sess, env, None)?;
        sess.in_lvalue_context = false;

        let mut rhs_node = self.rhs.check(sess, env, Some(lhs_node.ty()))?;

        rhs_node
            .ty()
            .unify(&lhs_node.ty(), &mut sess.tcx)
            .or_coerce_into_ty(
                &mut rhs_node,
                lhs_node.ty(),
                &mut sess.tcx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tcx,
                lhs_node.ty(),
                Some(lhs_node.span()),
                rhs_node.ty(),
                rhs_node.span(),
            )?;

        sess.check_mutable_lvalue_access(&lhs_node)?;

        Ok(hir::Node::Assignment(hir::Assignment {
            ty: sess.tcx.common_types.unit,
            span: self.span,
            lhs: Box::new(lhs_node),
            rhs: Box::new(rhs_node),
        }))
    }
}

impl Check for ast::Return {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let function_frame = sess
            .function_frame()
            .ok_or(SyntaxError::outside_of_function(self.span, "return"))?;

        let value = if let Some(expr) = &self.expr {
            let return_type = function_frame.return_type;
            let mut node = expr.check(sess, env, Some(return_type))?;

            node.ty()
                .unify(&return_type, &mut sess.tcx)
                .or_coerce_into_ty(
                    &mut node,
                    return_type,
                    &mut sess.tcx,
                    sess.target_metrics.word_size,
                )
                .or_report_err(
                    &sess.tcx,
                    return_type,
                    Some(function_frame.return_type_span),
                    node.ty(),
                    expr.span(),
                )?;

            node
        } else {
            let unit_type = sess.tcx.common_types.unit;

            function_frame
                .return_type
                .unify(&unit_type, &mut sess.tcx)
                .or_report_err(
                    &sess.tcx,
                    unit_type,
                    None,
                    function_frame.return_type,
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
            ty: sess.tcx.common_types.never,
            span: self.span,
        })))
    }
}
impl Check for ast::If {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        let unit_type = sess.tcx.common_types.unit;
        let bool_type = sess.tcx.common_types.bool;

        let mut condition_node = self.condition.check(sess, env, Some(bool_type))?;

        condition_node
            .ty()
            .unify(&bool_type, &mut sess.tcx)
            .or_coerce_into_ty(
                &mut condition_node,
                bool_type,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tcx,
                bool_type,
                None,
                condition_node.ty(),
                self.condition.span(),
            )?;

        // if the condition is compile-time known, only check the resulting branch
        match condition_node.as_const_value() {
            Some(ConstValue::Bool(true)) => self.then.check(sess, env, expected_type),
            Some(ConstValue::Bool(false)) => {
                if let Some(otherwise) = &self.otherwise {
                    otherwise.check(sess, env, expected_type)
                } else {
                    Ok(hir::Node::Const(hir::Const {
                        value: ConstValue::Unit(()),
                        ty: unit_type,
                        span: self.span,
                    }))
                }
            }
            _ => {
                let mut then_node = self.then.check(sess, env, expected_type)?;

                let if_node = if let Some(otherwise) = &self.otherwise {
                    let mut otherwise_node = otherwise.check(sess, env, Some(then_node.ty()))?;

                    otherwise_node
                        .ty()
                        .unify(&then_node.ty(), &mut sess.tcx)
                        .or_coerce(
                            &mut then_node,
                            &mut otherwise_node,
                            &mut sess.tcx,
                            sess.target_metrics.word_size,
                        )
                        .or_report_err(
                            &sess.tcx,
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
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let mut lhs_node = self.lhs.check(sess, env, None)?;

        // Apply short circuiting to && and ||
        match (&self.op, lhs_node.as_const_value()) {
            (ast::BinaryOp::And, Some(ConstValue::Bool(false))) => {
                let bool_type = sess.tcx.common_types.bool;

                lhs_node
                    .ty()
                    .unify(&bool_type, &mut sess.tcx)
                    .or_report_err(&sess.tcx, bool_type, None, lhs_node.ty(), lhs_node.span())?;

                return Ok(hir::Node::Const(hir::Const {
                    value: ConstValue::Bool(false),
                    ty: bool_type,
                    span: self.span,
                }));
            }
            (ast::BinaryOp::Or, Some(ConstValue::Bool(true))) => {
                let bool_type = sess.tcx.common_types.bool;

                lhs_node
                    .ty()
                    .unify(&bool_type, &mut sess.tcx)
                    .or_report_err(&sess.tcx, bool_type, None, lhs_node.ty(), lhs_node.span())?;

                return Ok(hir::Node::Const(hir::Const {
                    value: ConstValue::Bool(true),
                    ty: bool_type,
                    span: self.span,
                }));
            }
            _ => (),
        }

        let mut rhs_node = self.rhs.check(sess, env, Some(lhs_node.ty()))?;

        let lhs_node_type = lhs_node.ty().normalize(&sess.tcx);

        let expected_rhs_type = match lhs_node_type {
            Type::Pointer(..) => match &self.op {
                ast::BinaryOp::Add | ast::BinaryOp::Sub => sess.tcx.common_types.int,
                ast::BinaryOp::Eq
                | ast::BinaryOp::Ne
                | ast::BinaryOp::Lt
                | ast::BinaryOp::Le
                | ast::BinaryOp::Gt
                | ast::BinaryOp::Ge => lhs_node.ty(),
                op => {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "invalid operator `{}` used in pointer arithmetic",
                            op.to_string()
                        ))
                        .with_label(Label::primary(self.span, "invalid in pointer arithmetic"))
                        .with_label(Label::secondary(
                            lhs_node.span(),
                            format!("because this is of type {}", lhs_node_type),
                        )))
                }
            },
            _ => match &self.op {
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
                | ast::BinaryOp::BitAnd => {
                    let anyint_type = sess.tcx.anyint(lhs_node.span());

                    lhs_node
                        .ty()
                        .unify(&anyint_type, &mut sess.tcx)
                        .or_report_err(
                            &sess.tcx,
                            anyint_type,
                            None,
                            lhs_node.ty(),
                            lhs_node.span(),
                        )?;

                    anyint_type
                }
                ast::BinaryOp::And | ast::BinaryOp::Or => {
                    let bool_type = sess.tcx.common_types.bool;

                    lhs_node
                        .ty()
                        .unify(&bool_type, &mut sess.tcx)
                        .or_report_err(
                            &sess.tcx,
                            bool_type,
                            None,
                            lhs_node.ty(),
                            lhs_node.span(),
                        )?;

                    bool_type
                }
                _ => lhs_node.ty(),
            },
        };

        rhs_node
            .ty()
            .unify(&expected_rhs_type, &mut sess.tcx)
            .or_coerce(
                &mut lhs_node,
                &mut rhs_node,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            )
            .or_report_err(
                &sess.tcx,
                expected_rhs_type,
                None,
                rhs_node.ty(),
                self.rhs.span(),
            )?;

        let result_type = match lhs_node_type {
            Type::Pointer(..) => match &self.op {
                ast::BinaryOp::Add | ast::BinaryOp::Sub => lhs_node.ty(),
                ast::BinaryOp::Eq
                | ast::BinaryOp::Ne
                | ast::BinaryOp::Lt
                | ast::BinaryOp::Le
                | ast::BinaryOp::Gt
                | ast::BinaryOp::Ge => sess.tcx.common_types.bool,
                _ => panic!(),
            },
            _ => match &self.op {
                ast::BinaryOp::Add
                | ast::BinaryOp::Sub
                | ast::BinaryOp::Mul
                | ast::BinaryOp::Div
                | ast::BinaryOp::Rem
                | ast::BinaryOp::Shl
                | ast::BinaryOp::Shr
                | ast::BinaryOp::BitOr
                | ast::BinaryOp::BitXor
                | ast::BinaryOp::BitAnd => lhs_node.ty(),

                ast::BinaryOp::Eq
                | ast::BinaryOp::Ne
                | ast::BinaryOp::Lt
                | ast::BinaryOp::Le
                | ast::BinaryOp::Gt
                | ast::BinaryOp::Ge
                | ast::BinaryOp::And
                | ast::BinaryOp::Or => sess.tcx.common_types.bool,
            },
        };

        match (lhs_node.as_const_value(), rhs_node.as_const_value()) {
            (Some(lhs), Some(rhs)) => {
                let const_value = const_fold::binary(lhs, rhs, self.op, self.span, &sess.tcx)?;

                Ok(hir::Node::Const(hir::Const {
                    value: const_value,
                    ty: result_type,
                    span: self.span,
                }))
            }
            _ => {
                let binary = hir::Binary {
                    lhs: Box::new(lhs_node),
                    rhs: Box::new(rhs_node),
                    ty: result_type,
                    span: self.span,
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
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        match self.op {
            ast::UnaryOp::Ref(is_mutable) => {
                sess.in_lvalue_context = true;
                let node = self.value.check(sess, env, None)?;
                sess.in_lvalue_context = false;

                let node_type = node.ty();
                let ptr_type = sess.tcx.bound(
                    Type::Pointer(Box::new(node_type.as_kind()), is_mutable),
                    self.span,
                );

                if sess.is_lvalue(&node) || node_type.normalize(&sess.tcx).is_unsized() {
                    Ok(hir::Node::Builtin(hir::Builtin::Ref(hir::Ref {
                        value: Box::new(node),
                        is_mutable,
                        ty: ptr_type,
                        span: self.span,
                    })))
                } else {
                    let rvalue_name = sess.generate_name("rval");

                    let (id, bound_node) = sess.bind_name(
                        env,
                        rvalue_name,
                        ast::Visibility::Private,
                        node_type,
                        Some(node),
                        is_mutable,
                        BindingInfoKind::Orphan,
                        self.span,
                        BindingInfoFlags::NO_CONST_FOLD,
                    )?;

                    let id_node = hir::Node::Id(hir::Id {
                        id,
                        ty: node_type,
                        span: self.span,
                    });

                    let ref_node = hir::Node::Builtin(hir::Builtin::Ref(hir::Ref {
                        value: Box::new(id_node),
                        is_mutable,
                        ty: ptr_type,
                        span: self.span,
                    }));

                    Ok(hir::Node::Sequence(hir::Sequence {
                        statements: vec![bound_node, ref_node],
                        ty: ptr_type,
                        span: self.span,
                        is_block: false,
                    }))
                }
            }
            ast::UnaryOp::Deref => {
                let node = self.value.check(sess, env, None)?;
                let node_type = node.ty();

                let pointee_ty = sess.tcx.var(self.span);

                let ptr_ty = sess
                    .tcx
                    .bound(Type::Pointer(Box::new(pointee_ty.into()), false), self.span);

                node_type.unify(&ptr_ty, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    ptr_ty,
                    None,
                    node_type,
                    self.value.span(),
                )?;

                Ok(hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                    ty: pointee_ty,
                    span: self.span,
                    value: Box::new(node),
                })))
            }
            ast::UnaryOp::Not => {
                let node = self.value.check(sess, env, None)?;
                let node_type = node.ty();

                let anyint = sess.tcx.anyint(self.span);
                let bool = sess.tcx.common_types.bool;

                node_type
                    .unify(&bool, &mut sess.tcx)
                    .or_else(|_| node_type.unify(&anyint, &mut sess.tcx))
                    .or_report_err(&sess.tcx, bool, None, node_type, self.value.span())?;

                if let Some(const_value) = node.as_const_value() {
                    Ok(hir::Node::Const(hir::Const {
                        value: const_value.not(),
                        ty: node_type,
                        span: self.span,
                    }))
                } else {
                    Ok(hir::Node::Builtin(hir::Builtin::Not(hir::Unary {
                        value: Box::new(node),
                        ty: node_type,
                        span: self.span,
                    })))
                }
            }
            ast::UnaryOp::Neg => {
                let node = self.value.check(sess, env, None)?;
                let node_type = node.ty();

                let anyint = sess.tcx.anyint(self.span);

                node_type.unify(&anyint, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    anyint,
                    None,
                    node_type,
                    self.value.span(),
                )?;

                if let Some(const_value) = node.as_const_value() {
                    Ok(hir::Node::Const(hir::Const {
                        value: const_value.neg(),
                        ty: node_type,
                        span: self.span,
                    }))
                } else {
                    Ok(hir::Node::Builtin(hir::Builtin::Neg(hir::Unary {
                        value: Box::new(node),
                        ty: node_type,
                        span: self.span,
                    })))
                }
            }
            ast::UnaryOp::Plus => {
                let node = self.value.check(sess, env, None)?;
                let node_type = node.ty();

                let anyint = sess.tcx.anyint(self.span);

                node_type.unify(&anyint, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    anyint,
                    None,
                    node_type,
                    self.value.span(),
                )?;

                Ok(node)
            }
        }
    }
}
impl Check for ast::Call {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let callee = self.callee.check(sess, env, None)?;

        let call_node = match callee.ty().normalize(&sess.tcx) {
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
                        let param_ty = sess.tcx.bound(param.ty.clone(), self.span);
                        let mut node = arg.check(sess, env, Some(param_ty))?;

                        node.ty()
                            .unify(&param_ty, &mut sess.tcx)
                            .or_coerce_into_ty(
                                &mut node,
                                param_ty,
                                &mut sess.tcx,
                                sess.target_metrics.word_size,
                            )
                            .or_report_err(&sess.tcx, param_ty, None, node.ty(), arg.span())?;

                        args.push(node);
                    } else {
                        // this is a variadic argument, meaning that the argument's
                        // index is greater than the function's param length
                        let node = arg.check(sess, env, None)?;
                        args.push(node);
                    }
                }

                hir::Call {
                    ty: sess
                        .tcx
                        .bound(fn_ty.return_type.as_ref().clone(), self.span),
                    span: self.span,
                    callee: Box::new(callee),
                    args,
                }
            }
            ty => {
                let args = self
                    .args
                    .iter()
                    .map(|arg| arg.check(sess, env, None))
                    .collect::<DiagnosticResult<Vec<_>>>()?;

                let return_ty = sess.tcx.var(self.span);

                let inferred_fn_ty = Type::Function(FunctionType {
                    params: args
                        .iter()
                        .map(|arg| FunctionTypeParam {
                            name: ustr(""),
                            ty: arg.ty().into(),
                        })
                        .collect(),
                    return_type: Box::new(return_ty.into()),
                    varargs: None,
                    kind: FunctionTypeKind::Orphan,
                });

                ty.unify(&inferred_fn_ty, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    inferred_fn_ty,
                    None,
                    ty,
                    self.callee.span(),
                )?;

                hir::Call {
                    ty: return_ty,
                    span: self.span,
                    callee: Box::new(callee),
                    args,
                }
            }
        };

        for arg in call_node.args.iter() {
            let arg_type = arg.ty().normalize(&sess.tcx);
            match arg_type {
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

        Ok(hir::Node::Call(call_node))
    }
}

impl Check for ast::Cast {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        let node = self.expr.check(sess, env, None)?;

        let target_type = if let Some(type_expr) = &self.target {
            check_type_expr(type_expr, sess, env)?
        } else {
            match expected_type {
                Some(t) => t,
                None => {
                    return Err(Diagnostic::error()
                        .with_message("can't infer the type cast's target type")
                        .with_label(Label::primary(self.expr.span(), "can't infer")))
                }
            }
        };

        let from = node.ty().normalize(&sess.tcx);
        let to = target_type.normalize(&sess.tcx);

        if can_cast_type(&from, &to) {
            if let Some(const_value) = node.as_const_value() {
                if let Some(const_value) = try_cast_const_value(const_value, &to) {
                    return Ok(hir::Node::Const(hir::Const {
                        value: const_value,
                        ty: target_type,
                        span: self.span,
                    }));
                }
            }

            Ok(hir::Node::Cast(hir::Cast {
                value: Box::new(node),
                ty: target_type,
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
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        let unit_type = sess.tcx.common_types.unit;

        if self.statements.is_empty() {
            Ok(hir::Node::Sequence(hir::Sequence {
                statements: vec![hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty: unit_type,
                    span: self.span,
                })],
                ty: unit_type,
                span: self.span,
                is_block: true,
            }))
        } else {
            let mut statements: Vec<hir::Node> = vec![];

            env.push_scope(ScopeKind::Block);

            let last_index = self.statements.len() - 1;
            for (i, expr) in self.statements.iter().enumerate() {
                let expected_type = if i == last_index {
                    expected_type
                } else {
                    Some(unit_type)
                };

                let node = expr.check(sess, env, expected_type)?;

                statements.push(node);
            }

            env.pop_scope();

            let last_statement = statements.last().unwrap();

            let yield_type = if self.yields {
                last_statement.ty()
            } else if last_statement.ty().normalize(&sess.tcx).is_never() {
                sess.tcx.common_types.never
            } else {
                unit_type
            };

            if !self.yields {
                statements.push(hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty: yield_type,
                    span: last_statement.span(),
                }));
            }

            Ok(hir::Node::Sequence(hir::Sequence {
                statements,
                ty: yield_type,
                span: self.span,
                is_block: true,
            }))
        }
    }
}

impl Check for ast::TupleLiteral {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        expected_type: Option<TypeId>,
    ) -> CheckResult {
        // when a tuple literal is empty, it is either a unit value or unit type
        if self.elements.is_empty() {
            let unit_ty = sess.tcx.common_types.unit;

            let (ty, const_value) =
                if expected_type.map_or(false, |ty| ty.normalize(&sess.tcx).is_type()) {
                    (
                        sess.tcx.bound(unit_ty.as_kind().create_type(), self.span),
                        ConstValue::Type(unit_ty),
                    )
                } else {
                    (unit_ty, ConstValue::Unit(()))
                };

            return Ok(hir::Node::Const(hir::Const {
                value: const_value,
                ty,
                span: self.span,
            }));
        }

        let elements = self
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

            let is_tuple_type = const_values.iter().all(|v| v.as_type().is_some());

            if is_tuple_type {
                let element_types: Vec<Type> = elements
                    .iter()
                    .map(|node| {
                        node.as_const_value()
                            .unwrap()
                            .as_type()
                            .unwrap()
                            .normalize(&sess.tcx)
                            .clone()
                    })
                    .collect();

                for (i, elem_type) in element_types.iter().enumerate() {
                    if elem_type.is_unsized() {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "the size of type `{}` cannot be known at compile-time",
                                elem_type.display(&sess.tcx)
                            ))
                            .with_label(Label::primary(
                                elements[i].span(),
                                "doesn't have a size known at compile-time",
                            ))
                            .with_note("tuple element sizes must be known at compile-time"));
                    }
                }

                let tuple_type = Type::Tuple(element_types);
                let const_value = ConstValue::Type(sess.tcx.bound(tuple_type.clone(), self.span));

                Ok(hir::Node::Const(hir::Const {
                    value: const_value,
                    ty: sess.tcx.bound(tuple_type.create_type(), self.span),
                    span: self.span,
                }))
            } else {
                let element_tys: Vec<TypeId> = elements.iter().map(|el| el.ty()).collect();

                let element_type_norms: Vec<Type> =
                    element_tys.iter().map(|ty| ty.as_kind()).collect();

                let ty = sess.tcx.bound(Type::Tuple(element_type_norms), self.span);

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
                    value: const_value,
                    ty,
                    span: self.span,
                }))
            }
        } else {
            let element_tys: Vec<Type> = elements.iter().map(|e| e.ty().as_kind()).collect();

            let kind = Type::Tuple(element_tys);

            let ty = sess.tcx.bound(kind.clone(), self.span);

            Ok(hir::Node::Literal(hir::Literal::Tuple(hir::TupleLiteral {
                elements,
                ty,
                span: self.span,
            })))
        }
    }
}
impl Check for ast::StructType {
    fn check(
        &self,
        sess: &mut CheckSess,
        env: &mut Env,
        _expected_type: Option<TypeId>,
    ) -> CheckResult {
        let name = if self.name.is_empty() {
            get_anonymous_struct_name(self.span)
        } else {
            self.name
        };

        // the struct's main type variable
        let struct_type_var = sess.tcx.bound(
            Type::Struct(StructType::empty(name, BindingId::unknown(), self.kind)),
            self.span,
        );

        // the struct's main type variable, in its `type` variation
        let struct_type_type_var = sess
            .tcx
            .bound(struct_type_var.as_kind().create_type(), self.span);

        sess.self_types.push(struct_type_var);

        env.push_scope(ScopeKind::Block);

        let (binding_id, _) = sess.bind_name(
            env,
            name,
            ast::Visibility::Private,
            struct_type_type_var,
            Some(hir::Node::Const(hir::Const {
                value: ConstValue::Type(struct_type_var),
                ty: sess.tcx.common_types.anytype,
                span: self.span,
            })),
            false,
            BindingInfoKind::Orphan,
            self.span,
            BindingInfoFlags::empty(),
        )?;

        sess.tcx.bind_ty(
            struct_type_var,
            Type::Struct(StructType::empty(name, binding_id, self.kind)),
        );

        let mut field_map = UstrMap::<Span>::default();
        let mut struct_type_fields = vec![];

        for field in self.fields.iter() {
            let node = field
                .ty
                .check(sess, env, Some(sess.tcx.common_types.anytype))?;
            let ty = sess.extract_const_type(&node)?;

            if let Some(defined_span) = field_map.insert(field.name, field.span) {
                return Err(SyntaxError::duplicate_struct_field(
                    defined_span,
                    field.span,
                    field.name.to_string(),
                ));
            }

            struct_type_fields.push(StructTypeField {
                name: field.name,
                ty: ty.into(),
                span: field.span,
            });
        }

        env.pop_scope();

        sess.self_types.pop();

        for field in struct_type_fields.iter() {
            let field_type = field.ty.normalize(&sess.tcx);
            if field_type.is_unsized() {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "the size of `{}`s type `{}` cannot be known at compile-time",
                        field.name,
                        field_type.display(&sess.tcx)
                    ))
                    .with_label(Label::primary(
                        field.span,
                        "doesn't have a size known at compile-time",
                    ))
                    .with_note("struct field sizes must be known at compile-time"));
            }
        }

        let struct_type = Type::Struct(StructType {
            name,
            binding_id,
            kind: self.kind,
            fields: struct_type_fields,
        });

        if occurs(struct_type_var, &struct_type, &sess.tcx) {
            Err(UnifyTypeErr::Occurs.into_diagnostic(
                &sess.tcx,
                struct_type,
                None,
                struct_type_var,
                self.span,
            ))
        } else {
            Ok(hir::Node::Const(hir::Const {
                ty: sess.tcx.bound(struct_type.clone().create_type(), self.span),
                span: self.span,
                value: ConstValue::Type(sess.tcx.bound(struct_type, self.span)),
            }))
        }
    }
}

#[inline]
fn check_named_struct_literal(
    sess: &mut CheckSess,
    env: &mut Env,
    struct_ty: StructType,
    fields: &Vec<ast::StructLiteralField>,
    span: Span,
) -> CheckResult {
    let mut field_set = UstrSet::default();
    let mut uninit_fields = UstrSet::from_iter(struct_ty.fields.iter().map(|f| f.name));

    let mut field_nodes: Vec<hir::StructLiteralField> = vec![];

    for field in fields.iter() {
        if !field_set.insert(field.name) {
            return Err(SyntaxError::struct_field_specified_more_than_once(
                field.span,
                field.name.to_string(),
            ));
        }

        match struct_ty.find_field(field.name) {
            Some(ty_field) => {
                uninit_fields.remove(&field.name);

                let expected_type = sess.tcx.bound(ty_field.ty.clone(), ty_field.span);
                let mut node = field.expr.check(sess, env, Some(expected_type))?;

                node.ty()
                    .unify(&expected_type, &mut sess.tcx)
                    .or_coerce_into_ty(
                        &mut node,
                        expected_type,
                        &mut sess.tcx,
                        sess.target_metrics.word_size,
                    )
                    .or_report_err(
                        &sess.tcx,
                        expected_type,
                        Some(ty_field.span),
                        node.ty(),
                        field.expr.span(),
                    )?;

                field_nodes.push(hir::StructLiteralField {
                    ty: node.ty(),
                    span,
                    name: field.name,
                    value: Box::new(node),
                });
            }
            None => {
                return Err(TypeError::invalid_struct_field(
                    field.span,
                    field.name,
                    Type::Struct(struct_ty).display(&sess.tcx),
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
    fields: &Vec<ast::StructLiteralField>,
    span: Span,
) -> CheckResult {
    let mut field_set = UstrSet::default();

    let name = get_anonymous_struct_name(span);

    let mut field_nodes: Vec<hir::StructLiteralField> = vec![];

    let mut struct_ty = StructType {
        name,
        binding_id: BindingId::unknown(),
        kind: StructTypeKind::Struct,
        fields: vec![],
    };

    for field in fields.iter() {
        if !field_set.insert(field.name) {
            return Err(SyntaxError::struct_field_specified_more_than_once(
                field.span,
                field.name.to_string(),
            ));
        }

        let node = field.expr.check(sess, env, None)?;

        struct_ty.fields.push(StructTypeField {
            name: field.name,
            ty: node.ty().into(),
            span: field.span,
        });

        field_nodes.push(hir::StructLiteralField {
            ty: node.ty(),
            span,
            name: field.name,
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
            ty: sess.tcx.bound(Type::Struct(struct_ty), span),
            span,
        })
    } else {
        hir::Node::Literal(hir::Literal::Struct(hir::StructLiteral {
            ty: sess.tcx.bound(Type::Struct(struct_ty), span),
            span,
            fields: field_nodes,
        }))
    }
}

fn get_anonymous_struct_name(span: Span) -> Ustr {
    ustr(&format!("struct:{}:{}", span.start.line, span.start.column))
}

pub(super) fn check_optional_type_expr<'s>(
    type_expr: &Option<Box<ast::Ast>>,
    sess: &mut CheckSess<'s>,
    env: &mut Env,
    span: Span,
) -> DiagnosticResult<TypeId> {
    if let Some(type_expr) = type_expr {
        check_type_expr(type_expr, sess, env)
    } else {
        Ok(sess.tcx.var(span))
    }
}

pub(super) fn check_type_expr<'s>(
    type_expr: &ast::Ast,
    sess: &mut CheckSess<'s>,
    env: &mut Env,
) -> DiagnosticResult<TypeId> {
    let node = type_expr.check(sess, env, Some(sess.tcx.common_types.anytype))?;
    sess.extract_const_type(&node)
}
