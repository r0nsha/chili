mod builtin;
mod check_assign;
mod check_binary;
mod check_binding;
mod check_call;
mod check_expr;
mod check_fn;
mod check_pattern;
mod check_unary;
mod lints;

use builtin::get_builtin_types;
use chili_ast::{
    ast::{Ast, Expr, ExprKind, ModuleInfo},
    value::Value,
    workspace::{BindingInfoIdx, Workspace},
};
use chili_error::{DiagnosticResult, TypeError};
use chili_infer::infer::InferenceContext;
use chili_span::Span;
use chili_ty::Ty;
use common::scopes::Scopes;
use ustr::{Ustr, UstrMap};

pub fn check<'w>(
    workspace: &'w mut Workspace<'w>,
    asts: &mut Vec<Ast>,
) -> DiagnosticResult<()> {
    let target_metrics = workspace.build_options.target_platform.metrics();
    let mut infcx = InferenceContext::new(target_metrics.word_size);
    let mut sess = CheckSess::new(workspace, &mut infcx);

    for ast in asts {
        for import in ast.imports.iter() {
            sess.check_import(ast.module_info, import)?;
        }
        for binding in ast.bindings.iter() {
            sess.check_top_level_binding_internal(
                ast.module_info,
                binding,
                ast.module_info,
                Span::unknown(),
            )?;
        }
    }

    Ok(())
}

pub(crate) struct CheckedExpr {
    expr: Expr,
    ty: Ty,
    value: Option<Value>,
}

impl CheckedExpr {
    pub(crate) fn new(
        expr: ExprKind,
        ty: Ty,
        value: Option<Value>,
        span: Span,
    ) -> Self {
        Self {
            expr: Expr::typed(expr, ty.clone(), span),
            ty,
            value,
        }
    }
}

pub(crate) struct ProcessedItem {
    pub(crate) module_name: Ustr,
    pub(crate) symbol: Ustr,
}

pub(crate) struct CheckSess<'a> {
    pub(crate) workspace: &'a mut Workspace<'a>,
    pub(crate) infcx: &'a mut InferenceContext,
    pub(crate) builtin_types: UstrMap<Ty>,
    pub(crate) processed_items_stack: Vec<ProcessedItem>,
    pub(crate) init_scopes: Scopes<BindingInfoIdx, InitState>,
}

impl<'a> CheckSess<'a> {
    pub(crate) fn new(
        workspace: &'a mut Workspace<'a>,
        infcx: &'a mut InferenceContext,
    ) -> Self {
        Self {
            workspace,
            infcx,
            builtin_types: get_builtin_types(),
            processed_items_stack: vec![],
            init_scopes: Scopes::new(),
        }
    }

    pub(crate) fn expect_value_is_int(
        &mut self,
        value: Option<Value>,
        ty: &Ty,
        span: Span,
    ) -> DiagnosticResult<i64> {
        match &value {
            Some(value) => {
                if !value.is_int() {
                    Err(TypeError::expected(
                        span,
                        &self.infcx.normalize_ty_and_untyped(ty),
                        "compile-time known integer",
                    ))
                } else {
                    Ok(value.clone().into_int())
                }
            }
            None => Err(TypeError::expected(
                span,
                &self.infcx.normalize_ty_and_untyped(ty),
                "compile-time known integer",
            )),
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum InitState {
    NotInit,
    Init,
}

impl InitState {
    pub(crate) fn is_not_init(&self) -> bool {
        match self {
            InitState::NotInit => true,
            _ => false,
        }
    }

    pub(crate) fn is_init(&self) -> bool {
        match self {
            InitState::Init => true,
            _ => false,
        }
    }
}

pub(crate) struct CheckFrame {
    pub(crate) module_info: ModuleInfo,
    pub(crate) expected_return_ty: Option<Ty>,
    pub(crate) loop_depth: usize,
    pub(crate) self_types: Vec<Ty>,
}

impl CheckFrame {
    pub(crate) fn new(
        module_info: ModuleInfo,
        expected_return_ty: Option<Ty>,
    ) -> Self {
        Self {
            module_info,
            expected_return_ty,
            loop_depth: 0,
            self_types: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum TopLevelLookupKind {
    CurrentModule,
    OtherModule,
}
