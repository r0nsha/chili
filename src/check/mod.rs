mod attrs;
mod call;
mod const_fold;
mod entry;
mod env;
mod intrinsics;
mod lvalue_access;
mod pat;
mod top_level;

use self::pat::get_qualified_name;
use crate::{
    ast::{self, pat::Pat},
    common::target::TargetMetrics,
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
        display::{DisplayType, OrReportErr},
        misc::IsConcrete,
        normalize::Normalize,
        substitute::substitute_cache,
        type_ctx::TypeCtx,
        unify::{occurs, UnifyType, UnifyTypeErr},
    },
    interp::interp::Interp,
    span::Span,
    sym,
    types::{
        align_of::AlignOf, is_sized::IsSized, size_of::SizeOf, FunctionType, FunctionTypeParam, FunctionTypeVarargs,
        StructType, StructTypeField, Type, TypeId,
    },
    workspace::{
        BindingId, BindingInfo, BindingInfoFlags, BindingInfoKind, LibraryId, ModuleId, ScopeLevel, Workspace,
    },
};
use env::{Env, Scope, ScopeKind};
use indexmap::indexmap;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};
use top_level::CallerInfo;
use ustr::{ustr, Ustr, UstrMap};

pub type CheckData = (hir::Cache, TypeCtx);

pub fn check(workspace: &mut Workspace, module: Vec<ast::Module>) -> CheckData {
    let mut sess = CheckSess::new(workspace, &module);

    if let Err(diag) = sess.start() {
        sess.workspace.diagnostics.push(diag);
    }

    if sess.workspace.diagnostics.has_errors() {
        return sess.into_data();
    }

    sess.check_entry_point_function_exists();

    sess.into_data()
}

#[derive(Debug)]
pub(super) struct QueuedModule {
    pub(super) module_type: TypeId,
    pub(super) all_complete: bool,
    pub(super) queued_bindings: HashSet<usize>, // Binding indices
    pub(super) queued_comptime: HashSet<usize>, // Comptime indices
}

pub(super) struct CheckSess<'s> {
    pub workspace: &'s mut Workspace,
    pub target_metrics: TargetMetrics,

    pub interp: Interp,

    pub tcx: TypeCtx,

    // The module's being processed
    pub modules: &'s [ast::Module],

    pub cache: hir::Cache,
    pub queued_modules: HashMap<ModuleId, QueuedModule>,

    // Information that's relevant for the global context
    pub global_scopes: HashMap<ModuleId, Scope>,

    // Stack of function frames, each ast::Function creates its own frame
    pub function_frames: Vec<FunctionFrame>,

    // Stack of `Self` types
    pub self_types: Vec<TypeId>,

    // The current loop (while/for) depth
    // 0 meaning we are not in a loop, > 1 means we are in a loop
    pub loop_depth: usize,

    pub unique_name_indices: UstrMap<usize>,

    pub in_lvalue_context: bool,

    // A stack of encountered items. Used to detect global bindings that refer themselves
    pub encountered_items: HashSet<(ModuleId, usize)>,
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
            function_frames: vec![],
            self_types: vec![],
            loop_depth: 0,
            unique_name_indices: UstrMap::default(),
            in_lvalue_context: false,
            encountered_items: HashSet::new(),
        }
    }

    pub fn start(&mut self) -> CheckResult<()> {
        self.set_libraries_root_module_id();
        self.check_all_libraries()?;
        self.perform_final_substitution()?;
        Ok(())
    }

    fn set_libraries_root_module_id(&mut self) {
        self.workspace.libraries.iter_mut().for_each(|(_, library)| {
            let root_file = library.root_file.to_str().unwrap();

            library.root_module_id = self
                .workspace
                .module_infos
                .iter()
                .position(|(_, module)| module.file_path == root_file)
                .map(ModuleId::from)
                .unwrap();
        });
    }

    fn check_all_libraries(&mut self) -> CheckResult<()> {
        self.workspace
            .libraries
            .ids()
            .into_iter()
            .try_for_each(|library_id| self.check_library(library_id))
    }

    fn check_library(&mut self, library_id: LibraryId) -> CheckResult<()> {
        self.modules
            .iter()
            .filter(|module| module.info.library_id == library_id)
            .try_for_each(|module| self.check_module(module).map(|_| ()))
    }

    fn perform_final_substitution(&mut self) -> CheckResult<()> {
        substitute_cache(&self.cache, &mut self.tcx).map_err(|mut diagnostics| {
            let last = diagnostics.pop().unwrap();
            self.workspace.diagnostics.extend(diagnostics);
            last
        })
    }

    fn into_data(self) -> CheckData {
        (self.cache, self.tcx)
    }

    pub fn with_function_frame<T, F: FnMut(&mut Self) -> T>(&mut self, frame: FunctionFrame, mut f: F) -> T {
        self.function_frames.push(frame);
        let result = f(self);
        self.function_frames.pop();
        result
    }

    pub fn with_env<T, F: FnMut(&mut Self, Env) -> T>(&mut self, module_id: ModuleId, mut f: F) -> T {
        let module_info = *self.workspace.module_infos.get(module_id).unwrap();
        f(self, Env::new(module_id, module_info))
    }

    pub fn function_frame(&self) -> Option<FunctionFrame> {
        self.function_frames.last().copied()
    }

    pub fn require_const_type(&self, node: &hir::Node) -> DiagnosticResult<TypeId> {
        match node.as_const_value() {
            Some(ConstValue::Type(t)) => Ok(*t),
            _ => Err(TypeError::expected(node.span(), node.ty().display(&self.tcx), "a type")),
        }
    }

    pub fn require_const_int(&self, node: &hir::Node) -> DiagnosticResult<i128> {
        match node.as_const_value() {
            Some(ConstValue::Int(v)) => Ok(*v),
            _ => Err(TypeError::expected(
                node.span(),
                node.ty().display(&self.tcx),
                "compile-time known integer",
            )),
        }
    }

    pub(super) fn is_lvalue(&self, node: &hir::Node) -> bool {
        matches!(
            node,
            hir::Node::MemberAccess(_)
                | hir::Node::Id(_)
                | hir::Node::Builtin(hir::Builtin::Deref(_))
                | hir::Node::Builtin(hir::Builtin::Offset(_))
        )
    }

    pub(super) fn id_or_const_by_id(&self, id: BindingId, span: Span) -> hir::Node {
        self.id_or_const(self.workspace.binding_infos.get(id).unwrap(), span)
    }

    pub(super) fn id_or_const(&self, binding_info: &BindingInfo, span: Span) -> hir::Node {
        match &binding_info.const_value {
            Some(value @ ConstValue::Type(_)) => hir::Node::Const(hir::Const {
                value: value.clone(),
                ty: binding_info.ty,
                span,
            }),
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

    pub(super) fn eval(&mut self, node: &hir::Node, module_id: ModuleId, eval_span: Span) -> CheckResult<ConstValue> {
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
                        .with_message(format!("compile-time evaluation cannot result in `{}`", value_str))
                        .with_label(Label::primary(eval_span, "evaluated here"))),
                },
                Err(mut diagnostics) => {
                    let last = diagnostics.pop().unwrap();
                    self.workspace.diagnostics.extend(diagnostics);
                    Err(last)
                }
            }
        }
    }

    pub(super) fn get_type_by_name(
        &mut self,
        module_name: &str,
        scope_level: ScopeLevel,
        name: &str,
    ) -> DiagnosticResult<TypeId> {
        let module_id = self
            .workspace
            .module_infos
            .iter()
            .find(|(_, m)| m.qualified_name == module_name)
            .map(|(id, _)| ModuleId::from(id))
            .unwrap_or_else(|| panic!("couldn't find module '{}'", module_name));

        self.check_module_by_id(module_id)?;

        let ty = self
            .workspace
            .binding_infos
            .iter()
            .find(|(_, b)| b.module_id == module_id && b.scope_level == scope_level && b.name == name)
            .map(|(_, b)| *b.const_value.as_ref().unwrap().as_type().unwrap())
            .unwrap_or_else(|| panic!("couldn't find '{}' in module '{}'", name, module_name));

        Ok(ty)
    }

    pub(super) fn location_type(&mut self) -> DiagnosticResult<TypeId> {
        self.get_type_by_name("std.intrinsics", ScopeLevel::Global, "Location")
    }

    pub(super) fn build_location_value(&mut self, env: &Env, span: Span) -> DiagnosticResult<ConstValue> {
        let location_type = self.location_type()?.normalize(&self.tcx).into_struct();

        let file_field = location_type.field("file").unwrap();
        let line_field = location_type.field("line").unwrap();
        let column_field = location_type.field("column").unwrap();

        Ok(ConstValue::Struct(indexmap! {
            file_field.name => ConstElement {
                value: ConstValue::Str(env.module_info().file_path),
                ty: self.tcx.common_types.str_pointer
            },
            line_field.name => ConstElement {
                value: ConstValue::Int(span.start.line as _),
                ty: self.tcx.common_types.u32
            },
            column_field.name => ConstElement {
                value: ConstValue::Int(span.start.column as _),
                ty: self.tcx.common_types.u32
            }
        }))
    }

    pub(super) fn get_track_caller_location_param_id(&self, env: &Env, span: Span) -> DiagnosticResult<BindingId> {
        let invalid_scope_err = || {
            Diagnostic::error()
                .with_message(format!(
                    "`{}` can only be used in a function annotated with @{}",
                    hir::INTRINSIC_NAME_CALLER_LOCATION,
                    hir::attrs::ATTR_NAME_TRACK_CALLER,
                ))
                .with_label(Label::primary(span, "cannot be used within the current scope"))
        };

        match env.find_binding(ustr(sym::TRACK_CALLER_LOCATION_PARAM)) {
            Some(id) => match self.function_frame() {
                Some(frame) => {
                    let binding_info = self.workspace.binding_infos.get(id).unwrap();

                    if env.scope_level() >= frame.scope_level && binding_info.scope_level == frame.scope_level {
                        Ok(id)
                    } else {
                        Err(invalid_scope_err())
                    }
                }
                None => Err(invalid_scope_err()),
            },
            None => Err(invalid_scope_err()),
        }
    }

    pub(super) fn build_rvalue_ref(
        &mut self,
        env: &mut Env,
        value: hir::Node,
        is_mutable: bool,
        span: Span,
    ) -> DiagnosticResult<(hir::Node, hir::Node)> {
        let ty = value.ty();
        let ptr_type = self.tcx.bound(Type::Pointer(Box::new(ty.as_kind()), is_mutable), span);

        let (value, bound_node) = match value {
            hir::Node::Id(_) => (value.clone(), value),
            _ => {
                let rvalue_name = self.generate_name("rvalue");

                let (id, bound_node) = self.bind_name(
                    env,
                    rvalue_name,
                    ast::Vis::Private,
                    ty,
                    Some(value),
                    is_mutable,
                    BindingInfoKind::LetConst,
                    span,
                    BindingInfoFlags::NO_CONST_FOLD,
                )?;

                (hir::Node::Id(hir::Id { id, ty, span }), bound_node)
            }
        };

        let ref_node = hir::Node::Builtin(hir::Builtin::Ref(hir::Ref {
            value: Box::new(value),
            is_mutable,
            ty: ptr_type,
            span,
        }));

        Ok((bound_node, ref_node))
    }

    pub(crate) fn array_literal_or_const(
        &mut self,
        elements: Vec<hir::Node>,
        element_type: Type,
        span: Span,
    ) -> hir::Node {
        let element_type_ty = self.tcx.bound(element_type.clone(), span);

        let array_type = self
            .tcx
            .bound(Type::Array(Box::new(element_type), elements.len()), span);

        if elements.iter().all(|a| a.is_const()) {
            hir::Node::Const(hir::Const {
                value: ConstValue::Array(ConstArray {
                    values: elements
                        .into_iter()
                        .map(|elem| elem.into_const_value().unwrap())
                        .collect(),
                    element_type: element_type_ty,
                }),
                ty: array_type,
                span,
            })
        } else {
            hir::Node::Literal(hir::Literal::Array(hir::ArrayLiteral {
                elements,
                ty: array_type,
                span,
            }))
        }
    }

    pub(crate) fn get_len_node(tcx: &TypeCtx, node: &hir::Node) -> Option<hir::Node> {
        match node.ty().normalize(tcx) {
            Type::Array(_, size) => Some(hir::Node::Const(hir::Const {
                value: ConstValue::Int(size as _),
                ty: tcx.common_types.uint,
                span: node.span(),
            })),
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(_) | Type::Str(_) => Some(hir::Node::MemberAccess(hir::MemberAccess {
                    value: Box::new(node.clone()),
                    member_name: ustr(sym::BUILTIN_FIELD_LEN),
                    member_index: 1,
                    ty: tcx.common_types.uint,
                    span: node.span(),
                })),
                _ => None,
            },
            _ => None,
        }
    }
}

type CheckResult<T = hir::Node> = DiagnosticResult<T>;

pub(super) trait Check
where
    Self: Sized,
{
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult;
}

impl Check for ast::Binding {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        let attrs = sess.check_attrs(&self.attrs, env)?;

        sess.check_attrs_are_assigned_to_valid_binding(&attrs, self)?;

        match &self.kind {
            ast::BindingKind::Let { pat, type_expr, value } => {
                let ty = check_optional_type_expr(type_expr, sess, env, pat.span())?;

                let mut value_node = value.check(sess, env, Some(ty))?;

                value_node
                    .ty()
                    .unify(&ty, &mut sess.tcx)
                    .or_coerce_into_ty(&mut value_node, &ty, &mut sess.tcx, sess.target_metrics.word_size)
                    .or_report_err(
                        &sess.tcx,
                        &ty,
                        type_expr.as_ref().map(|e| e.span()),
                        &value_node.ty(),
                        value.span(),
                    )?;

                let binding_type = ty.normalize(&sess.tcx);

                if let Some(ConstValue::Type(_)) = value_node.as_const_value() {
                    return Err(Diagnostic::error()
                        .with_message("expected a value, got a type")
                        .with_label(Label::primary(value_node.span(), "expected a value"))
                        .with_note("if you intended to bind a type, change the `let` keyword to `type`"));
                }

                let value_is_module = binding_type.is_module();

                let is_static =
                    env.scope_level().is_global() && !value_node.is_const() || pat.iter().any(|p| p.is_mutable);

                // Bindings of type `{module}` cannot be assigned to mutable bindings
                if value_is_module {
                    pat.iter().filter(|pat| pat.is_mutable).for_each(|pat| {
                        sess.workspace.diagnostics.push(
                            Diagnostic::error()
                                .with_message("variable of type `{module}` must be immutable")
                                .with_label(Label::primary(pat.span, "variable is mutable"))
                                .with_note("try removing the `mut` from the declaration"),
                        );
                    });
                } else if binding_type.is_unsized() {
                    sess.workspace.diagnostics.push(TypeError::binding_is_unsized(
                        &pat.to_string(),
                        binding_type.display(&sess.tcx),
                        pat.span(),
                    ))
                }

                let value_span = value_node.span();
                let (_, bound_node) = sess.bind_pat(
                    env,
                    pat,
                    self.vis,
                    ty,
                    Some(value_node),
                    if is_static {
                        BindingInfoKind::LetStatic
                    } else {
                        BindingInfoKind::LetConst
                    },
                    value_span,
                    if type_expr.is_some() {
                        BindingInfoFlags::IS_USER_DEFINED
                    } else {
                        BindingInfoFlags::IS_USER_DEFINED | BindingInfoFlags::TYPE_WAS_INFERRED
                    },
                )?;

                Ok(bound_node)
            }
            ast::BindingKind::Function {
                name: ast::NameAndSpan { name, span },
                sig,
                body,
            } => {
                let (name, span) = (*name, *span);

                check_function_sig_has_type_annotations(sess, sig)?;

                let node = check_function(
                    sess,
                    env,
                    sig,
                    body,
                    span,
                    None,
                    if attrs.has(AttrKind::TrackCaller) {
                        TrackCaller::Yes
                    } else {
                        TrackCaller::No
                    },
                )?;

                // If this function binding matches the entry point function's requirements, Tag it as the entry function
                // Requirements:
                // - Is declared in the root module
                // - It is in global scope
                // - Its name is "main"
                if sess.workspace.build_options.need_entry_point_function()
                    && env.module_id() == sess.workspace.root_module_id
                    && env.scope_level().is_global()
                    && name == "main"
                {
                    if let Some(ConstValue::Function(f)) = node.as_const_value() {
                        let function = sess.cache.functions.get(f.id).unwrap();

                        if let hir::FunctionKind::Orphan { .. } = &function.kind {
                            sess.cache.entry_point_function_id = Some(function.id);
                        }
                    } else {
                        unreachable!()
                    }
                }

                sess.bind_name(
                    env,
                    name,
                    self.vis,
                    node.ty(),
                    Some(node),
                    false,
                    BindingInfoKind::Function,
                    span,
                    BindingInfoFlags::IS_USER_DEFINED,
                )
                .map(|(_, node)| node)
            }
            ast::BindingKind::ExternFunction {
                name: ast::NameAndSpan { name, span },
                sig,
            } => {
                let (name, span) = (*name, *span);

                let lib = sess.maybe_get_extern_lib_attr(env, &attrs, AttrKind::Lib)?;
                let dylib = sess.maybe_get_extern_lib_attr(env, &attrs, AttrKind::Dylib)?;
                let link_name = if let Some(attr) = attrs.get(AttrKind::LinkName) {
                    *attr.value.as_str().unwrap()
                } else {
                    name
                };

                let sig_node = sig.check(sess, env, Some(sess.tcx.common_types.anytype))?;

                let (qualified_name, function_kind, binding_kind) = if attrs.has(AttrKind::Intrinsic) {
                    match hir::Intrinsic::try_from(name.as_str()) {
                        Ok(intrinsic) => {
                            if lib.is_some() || dylib.is_some() || attrs.has(AttrKind::LinkName) {
                                return Err(Diagnostic::error()
                                    .with_message("intrinsic function cannot have a library defined")
                                    .with_label(Label::primary(span, "cannot define a library")));
                            }

                            match intrinsic {
                                hir::Intrinsic::StartWorkspace
                                | hir::Intrinsic::Location
                                | hir::Intrinsic::CallerLocation
                                | hir::Intrinsic::CompilerError
                                | hir::Intrinsic::CompilerWarning => (
                                    get_qualified_name(env.scope_name(), name),
                                    hir::FunctionKind::Intrinsic(intrinsic),
                                    BindingInfoKind::Intrinsic(intrinsic),
                                ),
                                hir::Intrinsic::Os | hir::Intrinsic::Arch => {
                                    return Err(Diagnostic::error()
                                        .with_message(format!("intrinsic name `{}` is reserved for a variable", name))
                                        .with_label(Label::primary(span, "intrinsic is a variable")))
                                }
                            }
                        }
                        Err(_) => {
                            return Err(Diagnostic::error()
                                .with_message(format!("unknown intrinsic function `{}`", name))
                                .with_label(Label::primary(span, "unknown intrinsic function")))
                        }
                    }
                } else {
                    (
                        name,
                        hir::FunctionKind::Extern {
                            lib: lib.clone(),
                            dylib: dylib.or(lib),
                            link_name,
                        },
                        BindingInfoKind::ExternFunction,
                    )
                };

                let ty = sig_node.into_const_value().unwrap().into_type().unwrap();

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
                    self.vis,
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
                is_mutable,
                type_expr,
            } => {
                let (name, span) = (*name, *span);

                let ty = check_type_expr(type_expr, sess, env)?;

                let lib = sess.maybe_get_extern_lib_attr(env, &attrs, AttrKind::Lib)?;
                let dylib = sess.maybe_get_extern_lib_attr(env, &attrs, AttrKind::Dylib)?;
                let link_name = if let Some(attr) = attrs.get(AttrKind::LinkName) {
                    *attr.value.as_str().unwrap()
                } else {
                    name
                };

                let (value, binding_kind) = if attrs.has(AttrKind::Intrinsic) {
                    match hir::Intrinsic::try_from(name.as_str()) {
                        Ok(intrinsic) => {
                            if lib.is_some() || dylib.is_some() || attrs.has(AttrKind::LinkName) {
                                return Err(Diagnostic::error()
                                    .with_message("intrinsic variable cannot have a library defined")
                                    .with_label(Label::primary(span, "cannot define a library")));
                            }

                            let (value, value_ty) = match intrinsic {
                                hir::Intrinsic::Os => {
                                    (ConstValue::from_os(sess.target_metrics.os), sess.tcx.common_types.uint)
                                }
                                hir::Intrinsic::Arch => (
                                    ConstValue::from_arch(sess.target_metrics.arch),
                                    sess.tcx.common_types.uint,
                                ),
                                hir::Intrinsic::StartWorkspace
                                | hir::Intrinsic::Location
                                | hir::Intrinsic::CallerLocation
                                | hir::Intrinsic::CompilerError
                                | hir::Intrinsic::CompilerWarning => {
                                    return Err(Diagnostic::error()
                                        .with_message(format!("intrinsic name `{}` is reserved for a function", name))
                                        .with_label(Label::primary(span, "intrinsic is a function")));
                                }
                            };

                            ty.unify(&value_ty, &mut sess.tcx).or_report_err(
                                &sess.tcx,
                                &value_ty,
                                Some(span),
                                &ty,
                                type_expr.span(),
                            )?;

                            (
                                hir::Node::Const(hir::Const {
                                    value,
                                    ty: value_ty,
                                    span,
                                }),
                                BindingInfoKind::Intrinsic(intrinsic),
                            )
                        }
                        Err(_) => {
                            return Err(Diagnostic::error()
                                .with_message(format!("unknown intrinsic variable `{}`", name))
                                .with_label(Label::primary(span, "unknown intrinsic function")))
                        }
                    }
                } else {
                    (
                        hir::Node::Const(hir::Const {
                            value: ConstValue::ExternVariable(ConstExternVariable {
                                name: link_name,
                                lib: lib.clone(),
                                dylib: dylib.or(lib),
                                ty,
                            }),
                            ty,
                            span,
                        }),
                        BindingInfoKind::ExternVariable,
                    )
                };

                if let Err(faulty_ty) = ty.is_concrete(&sess.tcx) {
                    return Err(Diagnostic::error()
                        .with_message(format!("extern variable `{}` isn't fully typed", name))
                        .with_label(Label::primary(span, format!("`{}` defined here", name)))
                        .maybe_with_label(
                            sess.tcx
                                .ty_span(faulty_ty)
                                .map(|span| Label::secondary(span, "incomplete type found here")),
                        ));
                }

                sess.bind_name(
                    env,
                    name,
                    self.vis,
                    ty,
                    Some(value),
                    *is_mutable,
                    binding_kind,
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
                            self.vis,
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
                        .with_note("if you intended to bind a value, change the `type` keyword to `let`")),
                }
            }
        }
    }
}

impl Check for ast::FunctionSig {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        check_function_sig_has_type_annotations(sess, self)?;

        let node = check_function_sig(
            sess,
            env,
            self,
            expected_type,
            Some(sess.tcx.common_types.unit),
            TrackCaller::No,
        )?;

        let function_type = node.ty().normalize(&sess.tcx).into_type().into_function();

        check_function_param_types(
            sess,
            self,
            &function_type.params,
            function_type.varargs.as_ref().map(|v| v.as_ref()),
        )?;

        check_function_return_type(
            sess,
            &function_type.return_type,
            self.return_type.as_ref().map(|t| t.span()).unwrap_or(self.span),
        )?;

        Ok(node)
    }
}

impl Check for ast::Ast {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        match self {
            ast::Ast::Binding(binding) => binding.check(sess, env, None),
            ast::Ast::Cast(cast) => cast.check(sess, env, expected_type),
            ast::Ast::Import(import) => {
                let import_path = import.path.to_str().unwrap();

                let module = sess
                    .modules
                    .iter()
                    .find(|m| m.info.file_path == import_path)
                    .unwrap_or_else(|| panic!("couldn't find ast for module with path: {}", import_path));

                sess.check_module(module)?;

                Ok(sess.module_node(module.id, import.span))
            }
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::SizeOf(expr) => {
                    let ty = check_type_expr(expr, sess, env)?;
                    let ty = ty.normalize(&sess.tcx);

                    if ty.is_unsized() {
                        Err(TypeError::type_is_unsized(ty.display(&sess.tcx), expr.span()))
                    } else {
                        let size = ty.size_of(sess.target_metrics.word_size);

                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Int(size as _),
                            ty: sess.tcx.common_types.uint,
                            span: expr.span(),
                        }))
                    }
                }
                ast::BuiltinKind::AlignOf(expr) => {
                    let ty = check_type_expr(expr, sess, env)?;
                    let ty = ty.normalize(&sess.tcx);

                    if ty.is_unsized() {
                        Err(TypeError::type_is_unsized(ty.display(&sess.tcx), expr.span()))
                    } else {
                        let align = ty.align_of(sess.target_metrics.word_size);

                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Int(align as _),
                            ty: sess.tcx.common_types.uint,
                            span: expr.span(),
                        }))
                    }
                }
            },
            ast::Ast::Comptime(const_) => const_.check(sess, env, expected_type),
            ast::Ast::Function(function) => function.check(sess, env, expected_type),
            ast::Ast::Loop(loop_) => loop_.check(sess, env, expected_type),
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
                    .or_coerce_into_ty(&mut offset_node, &uint, &mut sess.tcx, sess.target_metrics.word_size)
                    .or_report_err(&sess.tcx, &uint, None, &offset_node.ty(), sub.index.span())?;

                let node = sub.expr.check(sess, env, None)?;
                let node_type = node.ty().normalize(&sess.tcx);

                let const_value = if let Some(ConstValue::Int(const_index)) = offset_node.as_const_value() {
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
                                .with_label(Label::primary(sub.index.span(), "index out of bounds")));
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
                    Type::Array(inner, _) => inner.as_ref().clone(),
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Array(inner, _) | Type::Slice(inner) | Type::Str(inner) => inner.as_ref().clone(),
                        inner => inner.clone(),
                    },
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!("cannot index type `{}`", node_type.display(&sess.tcx)))
                            .with_label(Label::primary(sub.expr.span(), "cannot index")))
                    }
                };

                let ty = sess.tcx.bound(inner, sub.span);

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
                        .or_coerce_into_ty(&mut low_node, &uint, &mut sess.tcx, sess.target_metrics.word_size)
                        .or_report_err(&sess.tcx, &uint, None, &low_node.ty(), low_node.span())?;

                    low_node
                } else {
                    hir::Node::Const(hir::Const {
                        value: ConstValue::Int(0),
                        ty: uint,
                        span: slice.span,
                    })
                };

                let high_node = if let Some(high) = &slice.high {
                    let mut high_node = high.check(sess, env, None)?;

                    high_node
                        .ty()
                        .unify(&uint, &mut sess.tcx)
                        .or_coerce_into_ty(&mut high_node, &uint, &mut sess.tcx, sess.target_metrics.word_size)
                        .or_report_err(
                            &sess.tcx,
                            &uint,
                            slice.low.as_ref().map(|e| e.span()),
                            &high_node.ty(),
                            high_node.span(),
                        )?;

                    high_node
                } else {
                    match CheckSess::get_len_node(&sess.tcx, &node) {
                        Some(len_node) => len_node,
                        None => match &node_type {
                            Type::Pointer(_, _) => {
                                return Err(Diagnostic::error()
                                    .with_message("slicing a pointer requires specifying the end index")
                                    .with_label(Label::primary(slice.span, "must specify end index")))
                            }
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!("cannot slice type `{}`", node_type.display(&sess.tcx)))
                                    .with_label(Label::primary(slice.expr.span(), "cannot slice")))
                            }
                        },
                    }
                };

                let result_type = match &node_type {
                    Type::Array(inner, ..) => Type::Slice(inner.clone()),
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(inner) => Type::Slice(inner.clone()),
                        Type::Str(inner) => Type::Str(inner.clone()),
                        _ => {
                            if slice.high.is_none() {
                                return Err(Diagnostic::error()
                                    .with_message("pointer has an unknown length, ending index must be specified")
                                    .with_label(Label::primary(slice.expr.span(), "pointer has an unknown length")));
                            }

                            Type::Slice(inner.clone())
                        }
                    },
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!("cannot slice type `{}`", node_type.display(&sess.tcx)))
                            .with_label(Label::primary(slice.expr.span(), "cannot slice")))
                    }
                };

                let ty = sess.tcx.bound(result_type, slice.span);

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

                let node_type = node.ty().normalize(&sess.tcx);

                match &node_type {
                    Type::Pointer(inner, is_mutable) => match inner.as_ref() {
                        Type::Slice(inner) | Type::Str(inner) => {
                            if access.member.as_str() == sym::BUILTIN_FIELD_LEN {
                                let ty = sess.tcx.common_types.uint;

                                if let Some(ConstValue::Str(s)) = node.as_const_value() {
                                    return Ok(hir::Node::Const(hir::Const {
                                        value: ConstValue::Int(s.len() as _),
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
                            } else if access.member.as_str() == sym::BUILTIN_FIELD_PTR {
                                return Ok(hir::Node::MemberAccess(hir::MemberAccess {
                                    value: Box::new(node),
                                    member_name: access.member,
                                    member_index: 0,
                                    ty: sess.tcx.bound(Type::Pointer(inner.clone(), *is_mutable), access.span),
                                    span: access.span,
                                }));
                            }
                        }
                        _ => (),
                    },
                    Type::Module(module_id) => {
                        return sess.check_top_level_name(
                            access.member,
                            *module_id,
                            CallerInfo {
                                module_id: env.module_id(),
                                span: access.member_span,
                            },
                            true,
                        )
                    }
                    _ => (),
                }

                // Note (Ron): If the accessed value is a pointer, we auto dereference it.
                let node = if node_type.is_pointer() {
                    hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                        ty: sess.tcx.bound(node_type.maybe_deref_once(), node.span()),
                        span: node.span(),
                        value: Box::new(node),
                    }))
                } else {
                    node
                };

                match &node_type.maybe_deref_once() {
                    ty @ Type::Tuple(elements) => {
                        match access.member.as_str().parse::<usize>() {
                            Ok(index) => match elements.get(index) {
                                Some(field_ty) => {
                                    let ty = sess.tcx.bound(field_ty.clone(), access.span);

                                    if let Some(ConstValue::Tuple(const_elements)) = node.as_const_value() {
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
                    ty @ Type::Struct(st) => match st.field_and_position(access.member) {
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
                    Type::Array(_, size) if access.member.as_str() == sym::BUILTIN_FIELD_LEN => {
                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Int(*size as _),
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

                                let function_scope =
                                    sess.function_frame().map_or(ScopeLevel::Global, |f| f.scope_level);

                                if !binding_ty.is_type()
                                    && !binding_ty.is_module()
                                    && !binding_info.scope_level.is_global()
                                    && binding_info.scope_level < function_scope
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
                            sess.check_top_level_name(
                                ident.name,
                                env.module_id(),
                                CallerInfo {
                                    module_id: env.module_id(),
                                    span: ident.span,
                                },
                                false,
                            )
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
                            .or_coerce_into_ty(&mut node, &element_ty, &mut sess.tcx, sess.target_metrics.word_size)
                            .or_report_err(&sess.tcx, &element_ty, Some(element_ty_span), &node.ty(), el.span())?;

                        element_nodes.push(node);
                    }

                    let ty = sess
                        .tcx
                        .bound(Type::Array(Box::new(element_ty.into()), element_nodes.len()), lit.span);

                    let is_const_array = element_nodes.iter().all(|node| node.is_const());

                    if is_const_array {
                        let const_array = ConstValue::Array(ConstArray {
                            values: element_nodes
                                .iter()
                                .map(|node| node.as_const_value().unwrap().clone())
                                .collect(),
                            element_type: element_ty,
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
                    let len = sess.require_const_int(&len_node)?;

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
                            element_type: node.ty(),
                        });

                        Ok(hir::Node::Const(hir::Const {
                            value: const_array,
                            ty,
                            span: lit.span,
                        }))
                    } else {
                        Ok(hir::Node::Literal(hir::Literal::ArrayFill(hir::ArrayFillLiteral {
                            value: Box::new(node),
                            len: len as usize,
                            ty,
                            span: lit.span,
                        })))
                    }
                }
            },
            ast::Ast::TupleLiteral(lit) => lit.check(sess, env, expected_type),
            ast::Ast::Literal(lit) => {
                let const_value: ConstValue = lit.kind.into();

                let ty = match &lit.kind {
                    ast::LiteralKind::Nil => sess.tcx.var(lit.span),
                    ast::LiteralKind::Bool(_) => sess.tcx.common_types.bool,
                    ast::LiteralKind::Int(_) => sess.tcx.anyint(lit.span),
                    ast::LiteralKind::Float(_) => sess.tcx.anyfloat(lit.span),
                    ast::LiteralKind::Str(_) => sess.tcx.common_types.str_pointer,
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
            ast::Ast::ArrayType(ast::ArrayType { inner, size, span, .. }) => {
                let inner_type = check_type_expr(inner, sess, env)?;

                let size_node = size.check(sess, env, None)?;
                let size_value = sess.require_const_int(&size_node)?;

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

                for param in sig.params.iter() {
                    match &param.pat {
                        Pat::Ignore(_) | Pat::Name(_) => (),
                        Pat::Struct(_) | Pat::Tuple(_) | Pat::Hybrid(_) => {
                            return Err(Diagnostic::error()
                                .with_message("expected an indentifier or _")
                                .with_label(Label::primary(param.pat.span(), "expected an identifier or _"))
                                .with_note("binding patterns are not allowed in function type parameters"))
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

impl Check for ast::Loop {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        env.push_scope(ScopeKind::Loop);
        sess.loop_depth += 1;

        let block_node = self.block.check(sess, env, None)?;

        sess.loop_depth -= 1;
        env.pop_scope();

        Ok(hir::Node::Control(hir::Control::While(hir::While {
            condition: Box::new(hir::Node::Const(hir::Const {
                value: ConstValue::Bool(true),
                ty: sess.tcx.common_types.bool,
                span: self.span,
            })),
            body: Box::new(block_node),
            ty: sess.tcx.common_types.never,
            span: self.span,
        })))
    }
}
impl Check for ast::While {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        let bool_type = sess.tcx.common_types.bool;

        let mut condition_node = self.condition.check(sess, env, Some(bool_type))?;

        condition_node
            .ty()
            .unify(&bool_type, &mut sess.tcx)
            .or_coerce_into_ty(
                &mut condition_node,
                &bool_type,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            )
            .or_report_err(&sess.tcx, &bool_type, None, &condition_node.ty(), self.condition.span())?;

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
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
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

                start_node.ty().unify(&anyint, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    &anyint,
                    None,
                    &start_node.ty(),
                    start_node.span(),
                )?;

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
                        &start_node.ty(),
                        Some(start.span()),
                        &end_node.ty(),
                        end.span(),
                    )?;

                env.push_scope(ScopeKind::Loop);
                sess.loop_depth += 1;

                let mut statements: Vec<hir::Node> = vec![];

                // let mut index = 0
                let index_binding = self.index_binding.clone().unwrap_or_else(|| ast::NameAndSpan {
                    name: sess.generate_name("index"),
                    span: self.span,
                });

                let (index_id, index_binding) = sess.bind_name(
                    env,
                    index_binding.name,
                    ast::Vis::Private,
                    index_type,
                    Some(hir::Node::Const(hir::Const {
                        value: ConstValue::Int(0),
                        ty: index_type,
                        span: index_binding.span,
                    })),
                    false,
                    BindingInfoKind::LetConst,
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
                    ast::Vis::Private,
                    iter_type,
                    Some(start_node),
                    false,
                    BindingInfoKind::LetConst,
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
                let mut block_node = self.block.check(sess, env, None)?.force_into_sequence();

                // index += 1
                block_node.statements.push(hir::Node::Assign(hir::Assign {
                    lhs: Box::new(index_id_node.clone()),
                    rhs: Box::new(hir::Node::Builtin(hir::Builtin::Add(hir::Binary {
                        ty: index_type,
                        span: self.span,
                        lhs: Box::new(index_id_node),
                        rhs: Box::new(hir::Node::Const(hir::Const {
                            value: ConstValue::Int(1),
                            ty: index_type,
                            span: self.span,
                        })),
                    }))),
                    ty: unit_type,
                    span: self.span,
                }));

                // iter += 1
                block_node.statements.push(hir::Node::Assign(hir::Assign {
                    lhs: Box::new(iter_id_node.clone()),
                    rhs: Box::new(hir::Node::Builtin(hir::Builtin::Add(hir::Binary {
                        ty: iter_type,
                        span: self.span,
                        lhs: Box::new(iter_id_node),
                        rhs: Box::new(hir::Node::Const(hir::Const {
                            value: ConstValue::Int(1),
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
                    is_scope: false,
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
                        Type::Array(inner, _) | Type::Slice(inner) | Type::Str(inner) => inner.clone(),
                        _ => {
                            // TODO: duplicate error
                            return Err(Diagnostic::error()
                                .with_message(format!("can't iterate over `{}`", value_node_type.display(&sess.tcx)))
                                .with_label(Label::primary(value.span(), "can't iterate")));
                        }
                    },
                    _ => {
                        // TODO: duplicate error
                        return Err(Diagnostic::error()
                            .with_message(format!("can't iterate over `{}`", value_node_type.display(&sess.tcx)))
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
                    ast::Vis::Private,
                    value_type,
                    Some(value_node.clone()),
                    false,
                    BindingInfoKind::LetConst,
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
                let index_binding = self.index_binding.clone().unwrap_or_else(|| ast::NameAndSpan {
                    name: sess.generate_name("index"),
                    span: self.span,
                });

                let (index_id, index_binding) = sess.bind_name(
                    env,
                    index_binding.name,
                    ast::Vis::Private,
                    index_type,
                    Some(hir::Node::Const(hir::Const {
                        value: ConstValue::Int(0),
                        ty: index_type,
                        span: index_binding.span,
                    })),
                    false,
                    BindingInfoKind::LetConst,
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
                let value_len_node = CheckSess::get_len_node(&sess.tcx, &value_node).unwrap();

                let condition = hir::Node::Builtin(hir::Builtin::Lt(hir::Binary {
                    ty: bool_type,
                    span: self.span,
                    lhs: Box::new(index_id_node.clone()),
                    rhs: Box::new(value_len_node),
                }));

                // bind before block is checked: let iter = value[index]
                let iter_type = sess.tcx.bound(inner.as_ref().clone(), self.iter_binding.span);

                let (_, iter_binding) = sess.bind_name(
                    env,
                    self.iter_binding.name,
                    ast::Vis::Private,
                    iter_type,
                    Some(hir::Node::Builtin(hir::Builtin::Offset(hir::Offset {
                        value: Box::new(value_id_node),
                        index: Box::new(index_id_node.clone()),
                        ty: iter_type,
                        span: self.span,
                    }))),
                    false,
                    BindingInfoKind::LetConst,
                    self.iter_binding.span,
                    BindingInfoFlags::IS_USER_DEFINED
                        | BindingInfoFlags::TYPE_WAS_INFERRED
                        | BindingInfoFlags::NO_CONST_FOLD,
                )?;

                // loop block { ... }
                let mut block_node = self.block.check(sess, env, None)?.force_into_sequence();

                // let iter = value[index]
                block_node.statements.insert(0, iter_binding);

                // index += 1
                block_node.statements.push(hir::Node::Assign(hir::Assign {
                    lhs: Box::new(index_id_node.clone()),
                    rhs: Box::new(hir::Node::Builtin(hir::Builtin::Add(hir::Binary {
                        ty: index_type,
                        span: self.span,
                        lhs: Box::new(index_id_node),
                        rhs: Box::new(hir::Node::Const(hir::Const {
                            value: ConstValue::Int(1),
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
                    is_scope: false,
                }))
            }
        }
    }
}

impl Check for ast::Comptime {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        // Notes (Ron 02/07/2022):
        // The inner expression of `static` isn't allowed to capture its outer environment yet.
        // TODO: Running arbitrary should code require these preconditions to be met:
        //       1. All types are concrete
        //       2. All types in all memory locations are sized
        let node = sess.with_env(env.module_id(), |sess, mut env| {
            env.with_scope(ScopeKind::Block, |env| self.expr.check(sess, env, expected_type))
        })?;

        if sess.workspace.build_options.check_mode {
            // TODO: This is a hack so that printing won't interfere with our communication
            // TODO: with the language server. This causes false-positives, and needs to be fixed.
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
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        check_function(
            sess,
            env,
            &self.sig,
            &ast::Ast::Block(self.body.clone()),
            self.span,
            expected_type,
            TrackCaller::No,
        )
    }
}

impl Check for ast::Return {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        let function_frame = sess
            .function_frame()
            .ok_or_else(|| SyntaxError::outside_of_function(self.span, "return"))?;

        let value = if let Some(expr) = &self.expr {
            let return_type = function_frame.return_type;
            let mut node = expr.check(sess, env, Some(return_type))?;

            node.ty()
                .unify(&return_type, &mut sess.tcx)
                .or_coerce_into_ty(&mut node, &return_type, &mut sess.tcx, sess.target_metrics.word_size)
                .or_report_err(
                    &sess.tcx,
                    &return_type,
                    Some(function_frame.return_type_span),
                    &node.ty(),
                    expr.span(),
                )?;

            node
        } else {
            let unit_type = sess.tcx.common_types.unit;

            function_frame
                .return_type
                .unify(&unit_type, &mut sess.tcx)
                .or_report_err(&sess.tcx, &unit_type, None, &function_frame.return_type, self.span)?;

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
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        let unit_type = sess.tcx.common_types.unit;
        let bool_type = sess.tcx.common_types.bool;

        let mut condition_node = self.condition.check(sess, env, Some(bool_type))?;

        condition_node
            .ty()
            .unify(&bool_type, &mut sess.tcx)
            .or_coerce_into_ty(
                &mut condition_node,
                &bool_type,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            )
            .or_report_err(&sess.tcx, &bool_type, None, &condition_node.ty(), self.condition.span())?;

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

                if let Some(otherwise) = &self.otherwise {
                    let mut otherwise_node = otherwise.check(sess, env, Some(then_node.ty()))?;

                    let unify_nodes = otherwise_node.ty().unify(&then_node.ty(), &mut sess.tcx).or_coerce(
                        &mut then_node,
                        &mut otherwise_node,
                        &mut sess.tcx,
                        sess.target_metrics.word_size,
                    );

                    match unify_nodes {
                        Ok(_) => Ok(hir::Node::Control(hir::Control::If(hir::If {
                            ty: then_node.ty(),
                            span: self.span,
                            condition: Box::new(condition_node),
                            then: Box::new(then_node),
                            otherwise: Some(Box::new(otherwise_node)),
                        }))),
                        Err(_) if expected_type.map_or(false, |ty| ty.normalize(&sess.tcx).is_unit()) => {
                            // If the types don't match, and the expected type is unit, then we can assume
                            // that the if's result is not used
                            let unit_type = sess.tcx.common_types.unit;

                            Ok(hir::Node::Sequence(hir::Sequence {
                                statements: vec![
                                    hir::Node::Control(hir::Control::If(hir::If {
                                        ty: then_node.ty(),
                                        span: self.span,
                                        condition: Box::new(condition_node),
                                        then: Box::new(then_node),
                                        otherwise: Some(Box::new(otherwise_node)),
                                    })),
                                    hir::Node::Const(hir::Const {
                                        value: ConstValue::Unit(()),
                                        ty: unit_type,
                                        span: self.span,
                                    }),
                                ],
                                is_scope: false,
                                ty: unit_type,
                                span: self.span,
                            }))
                        }
                        Err(err) => Err(err.into_diagnostic(
                            &sess.tcx,
                            &then_node.ty(),
                            Some(self.then.span()),
                            &otherwise_node.ty(),
                            otherwise.span(),
                        )),
                    }
                } else {
                    Ok(hir::Node::Control(hir::Control::If(hir::If {
                        ty: unit_type,
                        span: self.span,
                        condition: Box::new(condition_node),
                        then: Box::new(then_node),
                        otherwise: None,
                    })))
                }
            }
        }
    }
}

impl Check for ast::Binary {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        let is_assignment = self.op.is_assignment();

        sess.in_lvalue_context = is_assignment;

        let mut lhs_node = self.lhs.check(sess, env, None)?;

        sess.in_lvalue_context = false;

        // Apply short circuiting to && and || when const folding
        match (&self.op, lhs_node.as_const_value()) {
            (ast::BinaryOp::And, Some(ConstValue::Bool(false))) => {
                let bool_type = sess.tcx.common_types.bool;

                lhs_node.ty().unify(&bool_type, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    &bool_type,
                    None,
                    &lhs_node.ty(),
                    lhs_node.span(),
                )?;

                return Ok(hir::Node::Const(hir::Const {
                    value: ConstValue::Bool(false),
                    ty: bool_type,
                    span: self.span,
                }));
            }
            (ast::BinaryOp::Or, Some(ConstValue::Bool(true))) => {
                let bool_type = sess.tcx.common_types.bool;

                lhs_node.ty().unify(&bool_type, &mut sess.tcx).or_report_err(
                    &sess.tcx,
                    &bool_type,
                    None,
                    &lhs_node.ty(),
                    lhs_node.span(),
                )?;

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
                        .with_message(format!("invalid operator `{}` used in pointer arithmetic", op))
                        .with_label(Label::primary(self.span, "invalid in pointer arithmetic"))
                        .with_label(Label::secondary(
                            lhs_node.span(),
                            format!("because this is of type {}", lhs_node_type.display(&sess.tcx)),
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

                    lhs_node.ty().unify(&anyint_type, &mut sess.tcx).or_report_err(
                        &sess.tcx,
                        &anyint_type,
                        None,
                        &lhs_node.ty(),
                        lhs_node.span(),
                    )?;

                    anyint_type
                }
                ast::BinaryOp::And | ast::BinaryOp::Or => {
                    let bool_type = sess.tcx.common_types.bool;

                    lhs_node.ty().unify(&bool_type, &mut sess.tcx).or_report_err(
                        &sess.tcx,
                        &bool_type,
                        None,
                        &lhs_node.ty(),
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
            .or_report_err(&sess.tcx, &expected_rhs_type, None, &rhs_node.ty(), self.rhs.span())?;

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

                ast::BinaryOp::Assign
                | ast::BinaryOp::AddAssign
                | ast::BinaryOp::SubAssign
                | ast::BinaryOp::MulAssign
                | ast::BinaryOp::DivAssign
                | ast::BinaryOp::RemAssign
                | ast::BinaryOp::AndAssign
                | ast::BinaryOp::OrAssign
                | ast::BinaryOp::ShlAssign
                | ast::BinaryOp::ShrAssign
                | ast::BinaryOp::BitAndAssign
                | ast::BinaryOp::BitOrAssign
                | ast::BinaryOp::BitXorAssign => sess.tcx.common_types.unit,
            },
        };

        match (lhs_node.as_const_value(), rhs_node.as_const_value()) {
            (Some(lhs), Some(rhs)) if const_fold::is_valid_binary_op(self.op) => {
                let const_value = const_fold::binary(lhs, rhs, self.op, self.span, &sess.tcx)?;

                // println!(
                //     "{} {} {} => {}",
                //     lhs.display(&sess.tcx),
                //     self.op,
                //     rhs.display(&sess.tcx),
                //     const_value.display(&sess.tcx)
                // );

                Ok(hir::Node::Const(hir::Const {
                    value: const_value,
                    ty: result_type,
                    span: self.span,
                }))
            }
            _ => {
                let op_node = |op: ast::BinaryOp| {
                    let binary = hir::Binary {
                        lhs: Box::new(lhs_node.clone()),
                        rhs: Box::new(rhs_node.clone()),
                        ty: result_type,
                        span: self.span,
                    };

                    match op {
                        ast::BinaryOp::Add => hir::Node::Builtin(hir::Builtin::Add(binary)),
                        ast::BinaryOp::Sub => hir::Node::Builtin(hir::Builtin::Sub(binary)),
                        ast::BinaryOp::Mul => hir::Node::Builtin(hir::Builtin::Mul(binary)),
                        ast::BinaryOp::Div => hir::Node::Builtin(hir::Builtin::Div(binary)),
                        ast::BinaryOp::Rem => hir::Node::Builtin(hir::Builtin::Rem(binary)),
                        ast::BinaryOp::Eq => hir::Node::Builtin(hir::Builtin::Eq(binary)),
                        ast::BinaryOp::Ne => hir::Node::Builtin(hir::Builtin::Ne(binary)),
                        ast::BinaryOp::Lt => hir::Node::Builtin(hir::Builtin::Lt(binary)),
                        ast::BinaryOp::Le => hir::Node::Builtin(hir::Builtin::Le(binary)),
                        ast::BinaryOp::Gt => hir::Node::Builtin(hir::Builtin::Gt(binary)),
                        ast::BinaryOp::Ge => hir::Node::Builtin(hir::Builtin::Ge(binary)),
                        ast::BinaryOp::And => hir::Node::Builtin(hir::Builtin::And(binary)),
                        ast::BinaryOp::Or => hir::Node::Builtin(hir::Builtin::Or(binary)),
                        ast::BinaryOp::Shl => hir::Node::Builtin(hir::Builtin::Shl(binary)),
                        ast::BinaryOp::Shr => hir::Node::Builtin(hir::Builtin::Shr(binary)),
                        ast::BinaryOp::BitAnd => hir::Node::Builtin(hir::Builtin::BitAnd(binary)),
                        ast::BinaryOp::BitOr => hir::Node::Builtin(hir::Builtin::BitOr(binary)),
                        ast::BinaryOp::BitXor => hir::Node::Builtin(hir::Builtin::BitXor(binary)),
                        _ => unreachable!(),
                    }
                };

                if is_assignment {
                    sess.check_mutable_lvalue_access(&lhs_node)?;
                }

                match self.op {
                    ast::BinaryOp::Add
                    | ast::BinaryOp::Sub
                    | ast::BinaryOp::Mul
                    | ast::BinaryOp::Div
                    | ast::BinaryOp::Rem
                    | ast::BinaryOp::Eq
                    | ast::BinaryOp::Ne
                    | ast::BinaryOp::Lt
                    | ast::BinaryOp::Le
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::Ge
                    | ast::BinaryOp::And
                    | ast::BinaryOp::Or
                    | ast::BinaryOp::Shl
                    | ast::BinaryOp::Shr
                    | ast::BinaryOp::BitAnd
                    | ast::BinaryOp::BitOr
                    | ast::BinaryOp::BitXor => Ok(op_node(self.op)),

                    ast::BinaryOp::Assign => Ok(hir::Node::Assign(hir::Assign {
                        lhs: Box::new(lhs_node),
                        rhs: Box::new(rhs_node),
                        ty: result_type,
                        span: self.span,
                    })),

                    ast::BinaryOp::AddAssign
                    | ast::BinaryOp::SubAssign
                    | ast::BinaryOp::MulAssign
                    | ast::BinaryOp::DivAssign
                    | ast::BinaryOp::RemAssign
                    | ast::BinaryOp::AndAssign
                    | ast::BinaryOp::OrAssign
                    | ast::BinaryOp::ShlAssign
                    | ast::BinaryOp::ShrAssign
                    | ast::BinaryOp::BitAndAssign
                    | ast::BinaryOp::BitOrAssign
                    | ast::BinaryOp::BitXorAssign => {
                        let inner_op = self.op.inner();

                        let assign = hir::Node::Assign(hir::Assign {
                            lhs: Box::new(lhs_node.clone()),
                            rhs: Box::new(op_node(inner_op)),
                            ty: result_type,
                            span: self.span,
                        });

                        Ok(assign)
                    }
                }
            }
        }
    }
}
impl Check for ast::Unary {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        match self.op {
            ast::UnaryOp::Ref(is_mutable) => {
                sess.in_lvalue_context = true;
                let node = self.value.check(sess, env, None)?;
                sess.in_lvalue_context = false;

                let node_type = node.ty();
                let ptr_type = sess
                    .tcx
                    .bound(Type::Pointer(Box::new(node_type.as_kind()), is_mutable), self.span);

                if sess.is_lvalue(&node) || node_type.normalize(&sess.tcx).is_unsized() {
                    Ok(hir::Node::Builtin(hir::Builtin::Ref(hir::Ref {
                        value: Box::new(node),
                        is_mutable,
                        ty: ptr_type,
                        span: self.span,
                    })))
                } else {
                    let (bound_node, rvalue_node) = sess.build_rvalue_ref(env, node, is_mutable, self.span)?;

                    Ok(hir::Node::Sequence(hir::Sequence {
                        statements: vec![bound_node, rvalue_node],
                        ty: ptr_type,
                        span: self.span,
                        is_scope: false,
                    }))
                }
            }
            ast::UnaryOp::Deref => {
                let mut node = self.value.check(sess, env, None)?;
                let node_type = node.ty();

                let node_type_norm = node_type.normalize(&sess.tcx);
                match &node_type_norm {
                    Type::Pointer(inner, _) => {
                        if inner.is_unsized() {
                            Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot dereference value of type `{}`",
                                    node_type_norm.display(&sess.tcx)
                                ))
                                .with_label(Label::primary(self.span, "cannot dereference")))
                        } else {
                            Ok(hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                                ty: sess.tcx.bound(inner.as_ref().clone(), self.span),
                                span: self.span,
                                value: Box::new(node),
                            })))
                        }
                    }
                    Type::Var(_) => {
                        let pointee_ty = sess.tcx.var(self.span);

                        let ptr_ty = sess
                            .tcx
                            .bound(Type::Pointer(Box::new(pointee_ty.into()), true), self.span);

                        node_type
                            .unify(&ptr_ty, &mut sess.tcx)
                            .or_coerce_into_ty(&mut node, &ptr_ty, &mut sess.tcx, sess.target_metrics.word_size)
                            .or_report_err(&sess.tcx, &ptr_ty, None, &node_type, self.value.span())?;

                        Ok(hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
                            ty: pointee_ty,
                            span: self.span,
                            value: Box::new(node),
                        })))
                    }
                    ty => Err(Diagnostic::error()
                        .with_message(format!("cannot dereference value of type `{}`", ty.display(&sess.tcx)))
                        .with_label(Label::primary(self.span, "cannot dereference"))),
                }
            }
            ast::UnaryOp::Not => {
                let node = self.value.check(sess, env, None)?;
                let node_type = node.ty();

                let anyint = sess.tcx.anyint(self.span);
                let bool = sess.tcx.common_types.bool;

                node_type
                    .unify(&bool, &mut sess.tcx)
                    .or_else(|_| node_type.unify(&anyint, &mut sess.tcx))
                    .or_report_err(&sess.tcx, &bool, None, &node_type, self.value.span())?;

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
                    &anyint,
                    None,
                    &node_type,
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
                    &anyint,
                    None,
                    &node_type,
                    self.value.span(),
                )?;

                Ok(node)
            }
        }
    }
}

impl Check for ast::Cast {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        let node = self.expr.check(sess, env, None)?;

        let target_type = check_type_expr(&self.target_type, sess, env)?;

        if let Some(expected_type) = expected_type {
            target_type.unify(&expected_type, &mut sess.tcx).or_report_err(
                &sess.tcx,
                &expected_type,
                sess.tcx.ty_span(expected_type),
                &target_type,
                self.target_type.span(),
            )?;
        }

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
                .with_message(format!(
                    "cannot cast from `{}` to `{}`",
                    from.display(&sess.tcx),
                    to.display(&sess.tcx)
                ))
                .with_label(Label::primary(
                    self.span,
                    format!("invalid cast to `{}`", to.display(&sess.tcx)),
                )))
        }
    }
}

impl Check for ast::Block {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        let unit_type = sess.tcx.common_types.unit;

        match self.statements.len() {
            0 => Ok(hir::Node::Sequence(hir::Sequence {
                statements: vec![hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty: unit_type,
                    span: self.span,
                })],
                ty: unit_type,
                span: self.span,
                is_scope: true,
            })),
            1 => self.statements[0].check(sess, env, expected_type),
            _ => {
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

                let ty = statements.last().unwrap().ty();

                if statements.iter().all(|stmt| stmt.is_const()) {
                    Ok(statements.pop().unwrap())
                } else {
                    Ok(hir::Node::Sequence(hir::Sequence {
                        statements,
                        ty,
                        span: self.span,
                        is_scope: true,
                    }))
                }
            }
        }
    }
}

impl Check for ast::TupleLiteral {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, expected_type: Option<TypeId>) -> CheckResult {
        // when a tuple literal is empty, it is either a unit value or unit type
        if self.elements.is_empty() {
            let unit_ty = sess.tcx.common_types.unit;

            let (ty, const_value) = if expected_type.map_or(false, |ty| ty.normalize(&sess.tcx).is_type()) {
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
            let const_values: Vec<&ConstValue> = elements.iter().map(|node| node.as_const_value().unwrap()).collect();

            let is_tuple_type = const_values.iter().all(|v| v.as_type().is_some());

            if is_tuple_type {
                let element_types: Vec<Type> = elements
                    .iter()
                    .map(|node| node.as_const_value().unwrap().as_type().unwrap().normalize(&sess.tcx))
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

                let element_type_norms: Vec<Type> = element_tys.iter().map(|ty| ty.as_kind()).collect();

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

            let ty = sess.tcx.bound(kind, self.span);

            Ok(hir::Node::Literal(hir::Literal::Tuple(hir::TupleLiteral {
                elements,
                ty,
                span: self.span,
            })))
        }
    }
}
impl Check for ast::StructType {
    fn check(&self, sess: &mut CheckSess, env: &mut Env, _expected_type: Option<TypeId>) -> CheckResult {
        let is_named = !self.name.is_empty();

        let name = if is_named {
            self.name
        } else {
            get_anonymous_struct_name(self.span)
        };

        // the struct's main type variable
        let struct_type_var = sess
            .tcx
            .bound(Type::Struct(StructType::empty(name, None, self.kind)), self.span);

        // the struct's main type variable, in its `type` variation
        let struct_type_type_var = sess.tcx.bound(struct_type_var.as_kind().create_type(), self.span);

        env.push_scope(ScopeKind::Block);

        let id = if is_named {
            sess.self_types.push(struct_type_var);

            let (binding_id, _) = sess.bind_name(
                env,
                name,
                ast::Vis::Private,
                struct_type_type_var,
                Some(hir::Node::Const(hir::Const {
                    value: ConstValue::Type(struct_type_var),
                    ty: sess.tcx.common_types.anytype,
                    span: self.span,
                })),
                false,
                BindingInfoKind::LetConst,
                self.span,
                BindingInfoFlags::empty(),
            )?;

            sess.tcx.bind_ty(
                struct_type_var,
                Type::Struct(StructType::empty(name, Some(binding_id), self.kind)),
            );

            Some(binding_id)
        } else {
            None
        };

        let mut field_map = UstrMap::<Span>::default();
        let mut struct_type_fields = vec![];

        for field in self.fields.iter() {
            let node = field.ty.check(sess, env, Some(sess.tcx.common_types.anytype))?;
            let ty = sess.require_const_type(&node)?;

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

        if is_named {
            sess.self_types.pop();
        }

        env.pop_scope();

        for field in struct_type_fields.iter() {
            let field_type = field.ty.normalize(&sess.tcx);
            if field_type.is_unsized() {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "the size of `{}`s type `{}` cannot be known at compile-time",
                        field.name,
                        field_type.display(&sess.tcx)
                    ))
                    .with_label(Label::primary(field.span, "doesn't have a size known at compile-time"))
                    .with_note("struct field sizes must be known at compile-time"));
            }
        }

        let struct_type = Type::Struct(StructType {
            name,
            id,
            kind: self.kind,
            fields: struct_type_fields,
        });

        if occurs(struct_type_var, &struct_type, &sess.tcx) {
            return Err(UnifyTypeErr::Occurs.into_diagnostic(
                &sess.tcx,
                &struct_type,
                None,
                &struct_type_var,
                self.span,
            ));
        }

        Ok(hir::Node::Const(hir::Const {
            ty: sess.tcx.bound(struct_type.clone().create_type(), self.span),
            span: self.span,
            value: ConstValue::Type(sess.tcx.bound(struct_type, self.span)),
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
    sess.require_const_type(&node)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TrackCaller {
    Yes,
    No,
}

fn check_function<'s>(
    sess: &mut CheckSess<'s>,
    env: &mut Env,
    sig: &ast::FunctionSig,
    body: &ast::Ast,
    span: Span,
    expected_type: Option<TypeId>,
    track_caller: TrackCaller,
) -> CheckResult {
    let name = sig.name_or_anonymous();
    let qualified_name = get_qualified_name(env.scope_name(), name);

    let sig_node = check_function_sig(sess, env, sig, expected_type, None, track_caller)?;
    let sig_type = sess.require_const_type(&sig_node)?;
    let function_type = sig_type.normalize(&sess.tcx).into_function();

    let return_type = sess.tcx.bound(
        function_type.return_type.as_ref().clone(),
        sig.return_type.as_ref().map_or(sig.span, |e| e.span()),
    );

    let return_type_span = sig.return_type.as_ref().map_or(sig.span, |e| e.span());

    env.push_scope(ScopeKind::Function);

    let mut params: Vec<hir::FunctionParam> = vec![];
    let mut param_bind_statements: Vec<hir::Node> = vec![];

    for (index, param_type) in function_type.params.iter().enumerate() {
        let (param, bound_node) = match sig.params.get(index) {
            Some(param) if !sym::is_implicitly_generated_param(&param_type.name) => {
                let ty = sess.tcx.bound(
                    param_type.ty.clone(),
                    param.type_expr.as_ref().map_or(param.pat.span(), |e| e.span()),
                );

                let (bound_id, bound_node) = sess.bind_pat(
                    env,
                    &param.pat,
                    ast::Vis::Private,
                    ty,
                    None,
                    BindingInfoKind::LetConst,
                    param.pat.span(),
                    if param.type_expr.is_some() {
                        BindingInfoFlags::IS_USER_DEFINED
                    } else {
                        BindingInfoFlags::IS_USER_DEFINED | BindingInfoFlags::TYPE_WAS_INFERRED
                    },
                )?;

                (
                    hir::FunctionParam {
                        id: bound_id,
                        ty,
                        span: param.pat.span(),
                    },
                    bound_node,
                )
            }
            _ => {
                // This parameter was inserted implicitly
                let span = sig.span;
                let ty = sess.tcx.bound(param_type.ty.clone(), span);

                let (bound_id, bound_node) = sess.bind_name(
                    env,
                    param_type.name,
                    ast::Vis::Private,
                    ty,
                    None,
                    false,
                    BindingInfoKind::LetConst,
                    span,
                    if param_type.name == "it" {
                        BindingInfoFlags::IMPLICIT_IT_FUNCTION_PARAM
                    } else {
                        BindingInfoFlags::empty()
                    },
                )?;

                (hir::FunctionParam { id: bound_id, ty, span }, bound_node)
            }
        };

        params.push(param);

        // If this is a single statement, we ignore it,
        // As it doesn't include any destructuring statements.
        if let Ok(sequence) = bound_node.into_sequence() {
            param_bind_statements.extend(sequence.statements)
        }
    }

    if let Some(varargs) = &function_type.varargs {
        match &varargs.ty {
            Some(ty) => {
                let span = sig.varargs.as_ref().unwrap().type_expr.as_ref().unwrap().span();
                let ty = sess.tcx.bound(Type::slice_pointer(ty.clone(), false), span);

                let (bound_id, _) = sess.bind_name(
                    env,
                    varargs.name,
                    ast::Vis::Private,
                    ty,
                    None,
                    false,
                    BindingInfoKind::LetConst,
                    span,
                    BindingInfoFlags::empty(),
                )?;

                params.push(hir::FunctionParam { id: bound_id, ty, span });
            }
            None => {
                return Err(Diagnostic::error()
                    .with_message("untyped variadic parameters are only valid in extern functions")
                    .with_label(Label::primary(
                        sig.varargs.as_ref().unwrap().span,
                        "variadic parameter must be typed",
                    )))
            }
        }
    }

    let function_id = sess.cache.functions.insert_with_id(hir::Function {
        id: hir::FunctionId::unknown(),
        module_id: env.module_id(),
        name,
        qualified_name,
        kind: hir::FunctionKind::Orphan {
            params: params.clone(),
            inferred_return_type_span: if sig.return_type.is_some() {
                None
            } else {
                Some(sig.span)
            },
            body: None,
        },
        ty: sig_type,
        span,
    });

    env.insert_function(name, function_id);

    let body_node = sess.with_function_frame(
        FunctionFrame {
            return_type,
            return_type_span,
            scope_level: env.scope_level(),
        },
        |sess| body.check(sess, env, Some(return_type)),
    )?;

    let mut body_sequence = match body_node {
        hir::Node::Sequence(sequence) => sequence,
        node => {
            let ty = node.ty();
            let span = node.span();

            hir::Sequence {
                statements: vec![node],
                is_scope: true,
                ty,
                span,
            }
        }
    };

    param_bind_statements.append(&mut body_sequence.statements);
    body_sequence.statements = param_bind_statements;

    // Unify the function's body with the its return type
    {
        let mut unify_node = body_sequence.ty.unify(&return_type, &mut sess.tcx);

        if let Some(last_statement) = body_sequence.statements.last_mut() {
            unify_node = unify_node.or_coerce_into_ty(
                last_statement,
                &return_type,
                &mut sess.tcx,
                sess.target_metrics.word_size,
            );
        }

        match unify_node {
            Ok(_) => (),
            Err(_) if return_type.normalize(&sess.tcx).is_unit() => {
                // Make the body implicitly return unit, so that we won't have to
                // add a `()` at the end of the body.
                let unit_type = sess.tcx.common_types.unit;

                body_sequence.statements.push(hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty: unit_type,
                    span: body_sequence.span,
                }));

                body_sequence.ty = unit_type;
            }
            Err(err) => {
                return Err(err.into_diagnostic(
                    &sess.tcx,
                    &return_type,
                    Some(return_type_span),
                    &body_sequence.ty,
                    body.span(),
                ))
            }
        }
    }

    env.pop_scope();

    check_function_param_types(
        sess,
        sig,
        &function_type.params,
        function_type.varargs.as_ref().map(|v| v.as_ref()),
    )?;

    check_function_return_type(
        sess,
        &function_type.return_type,
        sig.return_type.as_ref().map(|t| t.span()).unwrap_or(sig.span),
    )?;

    sess.cache
        .functions
        .get_mut(function_id)
        .unwrap()
        .set_body(body_sequence);

    Ok(hir::Node::Const(hir::Const {
        value: ConstValue::Function(ConstFunction { id: function_id, name }),
        ty: sig_type,
        span,
    }))
}

fn check_function_sig<'s>(
    sess: &mut CheckSess<'s>,
    env: &mut Env,
    sig: &ast::FunctionSig,
    expected_type: Option<TypeId>,
    default_return_type: Option<TypeId>,
    track_caller: TrackCaller,
) -> CheckResult {
    let mut extra_diagnostics: Vec<Diagnostic> = vec![];

    let mut param_types: Vec<FunctionTypeParam> = vec![];
    let mut defined_params = UstrMap::default();
    let mut first_default_param_span: Option<Span> = None;

    // Whenever a function declaration is annotated with @track_caller, we implicitly
    // insert a location parameter at the start - track_caller@location: Location
    if track_caller == TrackCaller::Yes {
        let name = ustr(sym::TRACK_CALLER_LOCATION_PARAM);
        let ty = sess.location_type()?.normalize(&sess.tcx);

        if let Some(already_defined_span) = defined_params.insert(name, sig.span) {
            return Err(SyntaxError::duplicate_binding(name, sig.span, already_defined_span));
        }

        param_types.push(FunctionTypeParam {
            name,
            ty,
            default_value: None,
        })
    }

    for param in sig.params.iter() {
        let param_span = param.pat.span();

        let name = match &param.pat {
            Pat::Name(p) => p.name,
            Pat::Ignore(_) | Pat::Struct(_) | Pat::Tuple(_) => ustr("_"),
            Pat::Hybrid(p) => p.name_pat.name,
        };

        let param_type = check_optional_type_expr(&param.type_expr, sess, env, param_span)?;

        for pat in param.pat.iter() {
            let name = pat.name;

            if let Some(already_defined_span) = defined_params.insert(name, pat.span) {
                return Err(SyntaxError::duplicate_binding(name, pat.span, already_defined_span));
            }
        }

        let default_value = if let Some(default_value) = &param.default_value {
            first_default_param_span = Some(param_span);

            let node = default_value.check(sess, env, Some(param_type))?;

            node.ty().unify(&param_type, &mut sess.tcx).or_report_err(
                &sess.tcx,
                &param_type,
                Some(param_span),
                &node.ty(),
                node.span(),
            )?;

            let default_value = node.into_const_value().ok_or_else(|| {
                Diagnostic::error()
                    .with_message("default parameter value must be a constant value")
                    .with_label(Label::primary(default_value.span(), "not a constant value"))
            })?;

            Some(default_value)
        } else if let Some(first_default_param_span) = first_default_param_span {
            return Err(Diagnostic::error()
                .with_message("parameter is missing a default value")
                .with_label(Label::primary(param_span, "missing a default value"))
                .with_label(Label::secondary(
                    first_default_param_span,
                    "first parameter with default value here",
                ))
                .with_note("parameters following a parameter with a default value, must also have a default value"));
        } else {
            None
        };

        param_types.push(FunctionTypeParam {
            name,
            ty: param_type.as_kind(),
            default_value,
        });
    }

    let varargs = if let Some(varargs) = &sig.varargs {
        if let Some(type_expr) = varargs.type_expr.as_ref() {
            let ty = check_type_expr(type_expr, sess, env)?;
            Some(Box::new(FunctionTypeVarargs {
                name: varargs.name.name,
                ty: Some(ty.as_kind()),
            }))
        } else if !sig.kind.is_extern() {
            return Err(Diagnostic::error()
                .with_message("variadic parameter must be typed")
                .with_label(Label::primary(varargs.span, "missing a type annotation")));
        } else {
            Some(Box::new(FunctionTypeVarargs {
                name: varargs.name.name,
                ty: None,
            }))
        }
    } else {
        None
    };

    let return_type = match (sig.return_type.as_ref(), default_return_type) {
        // there's a return type annotation
        (Some(return_type), _) => check_type_expr(return_type, sess, env)?,
        // there's no annotation, but we have a default return type
        (None, Some(default)) => default,
        // else
        _ => sess.tcx.var(sig.span),
    };

    let expected_type_norm = expected_type.map(|ty| ty.normalize(&sess.tcx));
    if let Some(Type::Function(expected_function_type)) = &expected_type_norm {
        // if the function signature has no parameters, and the
        // parent type is a function with 1 parameter, add an implicit `it` parameter
        if sig.params.is_empty() && varargs.is_none() && expected_function_type.params.len() == 1 {
            let param = &expected_function_type.params[0];
            param_types.push(FunctionTypeParam {
                name: ustr("it"),
                ty: param.ty.clone(),
                default_value: param.default_value.clone(),
            });
        }
    }

    let function_type = sess.tcx.bound(
        Type::Function(FunctionType {
            params: param_types,
            return_type: Box::new(return_type.as_kind()),
            varargs,
            kind: sig.kind.clone(),
        }),
        sig.span,
    );

    if let Some(expected_type_norm @ Type::Function(_)) = expected_type_norm {
        // unify this signature with the expected signature
        function_type.unify(&expected_type_norm, &mut sess.tcx).or_report_err(
            &sess.tcx,
            &expected_type_norm,
            sess.tcx.ty_span(expected_type.unwrap()),
            &function_type,
            sig.span,
        )?;
    }

    if extra_diagnostics.is_empty() {
        Ok(hir::Node::Const(hir::Const {
            value: ConstValue::Type(function_type),
            ty: sess.tcx.bound(function_type.as_kind().create_type(), sig.span),
            span: sig.span,
        }))
    } else {
        let last = extra_diagnostics.pop().unwrap();
        sess.workspace.diagnostics.extend(extra_diagnostics);
        Err(last)
    }
}

fn check_function_sig_has_type_annotations<'s>(sess: &mut CheckSess<'s>, sig: &ast::FunctionSig) -> CheckResult<()> {
    let mut diagnostics = sig
        .params
        .iter()
        .filter_map(|param| {
            param.type_expr.is_none().then(|| {
                Diagnostic::error()
                    .with_message(format!("missing type for function parameter `{}`", param.pat))
                    .with_label(Label::primary(param.pat.span(), "missing type annotation"))
            })
        })
        .collect::<Vec<_>>();

    if diagnostics.is_empty() {
        Ok(())
    } else {
        let last = diagnostics.pop().unwrap();
        sess.workspace.diagnostics.extend(diagnostics);
        Err(last)
    }
}

fn check_function_param_types<'s>(
    sess: &mut CheckSess<'s>,
    sig: &ast::FunctionSig,
    params: &[FunctionTypeParam],
    varargs: Option<&FunctionTypeVarargs>,
) -> CheckResult<()> {
    fn inner_check<'s>(
        sess: &mut CheckSess<'s>,
        diagnostics: &mut Vec<Diagnostic>,
        ty: &Type,
        name: &impl Display,
        span: Span,
    ) {
        let mut ty = ty.normalize(&sess.tcx);
        sess.tcx.make_concrete(&mut ty);

        if let Err(faulty_ty) = ty.is_concrete(&sess.tcx) {
            diagnostics.push(
                Diagnostic::error()
                    .with_message(format!("function parameter `{}` isn't fully typed", name))
                    .with_label(Label::primary(span, "defined here"))
                    .maybe_with_label(sess.tcx.ty_span(faulty_ty).map(|span| {
                        Label::secondary(
                            span,
                            format!("because this is of type `{}`", faulty_ty.display(&sess.tcx)),
                        )
                    })),
            )
        } else if ty.is_unsized() {
            diagnostics.push(
                Diagnostic::error()
                    .with_message(format!(
                        "the size of function parameter's type `{}` cannot be known at compile-time",
                        ty.display(&sess.tcx)
                    ))
                    .with_label(Label::primary(
                        span,
                        format!("`{}` doesn't have a size known at compile-time", name),
                    )),
            )
        } else if !can_type_be_in_function_sig(&ty) {
            diagnostics.push(
                Diagnostic::error()
                    .with_message(format!(
                        "function parameter cannot have type `{}`",
                        ty.display(&sess.tcx)
                    ))
                    .with_label(Label::primary(span, "invalid function parameter type")),
            )
        }
    }

    let mut diagnostics = vec![];

    for (param_type, param) in params.iter().zip(sig.params.iter()) {
        inner_check(
            sess,
            &mut diagnostics,
            &param_type.ty.normalize(&sess.tcx),
            &param.pat,
            param.pat.span(),
        );
    }

    if let Some(FunctionTypeVarargs { ty: Some(ty), .. }) = varargs {
        let varargs = sig.varargs.as_ref().unwrap();
        inner_check(
            sess,
            &mut diagnostics,
            &ty.normalize(&sess.tcx),
            &varargs.name,
            varargs.span,
        );
    }

    if diagnostics.is_empty() {
        Ok(())
    } else {
        let last = diagnostics.pop().unwrap();
        sess.workspace.diagnostics.extend(diagnostics);
        Err(last)
    }
}

fn check_function_return_type<'s>(sess: &mut CheckSess<'s>, return_type: &Type, span: Span) -> CheckResult<()> {
    let mut return_type = return_type.normalize(&sess.tcx);
    sess.tcx.make_concrete(&mut return_type);

    if let Err(faulty_ty) = return_type.is_concrete(&sess.tcx) {
        Err(Diagnostic::error()
            .with_message("function return isn't fully typed")
            .with_label(Label::primary(span, "incomplete return type"))
            .maybe_with_label(sess.tcx.ty_span(faulty_ty).map(|span| {
                Label::secondary(
                    span,
                    format!("because this is of type `{}`", faulty_ty.display(&sess.tcx)),
                )
            })))
    } else if return_type.is_unsized() {
        Err(TypeError::type_is_unsized(return_type.display(&sess.tcx), span))
    } else if !can_type_be_in_function_sig(&return_type) {
        Err(Diagnostic::error()
            .with_message(format!(
                "type `{}` cannot be returned from a function",
                return_type.display(&sess.tcx)
            ))
            .with_label(Label::primary(span, "invalid return type")))
    } else {
        Ok(())
    }
}

fn can_type_be_in_function_sig(ty: &Type) -> bool {
    match ty {
        Type::Never
        | Type::Unit
        | Type::Bool
        | Type::Int(_)
        | Type::Uint(_)
        | Type::Float(_)
        | Type::Pointer(_, _)
        | Type::Function(_)
        | Type::Array(_, _)
        | Type::Slice(_)
        | Type::Str(_)
        | Type::Tuple(_)
        | Type::Struct(_) => true,

        Type::Module(_) | Type::Type(_) | Type::AnyType | Type::Var(_) | Type::Infer(_, _) => false,
    }
}
