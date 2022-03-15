mod check_assign;
mod check_binary;
mod check_binding;
mod check_call;
mod check_expr;
mod check_fn;
mod check_pattern;
mod check_unary;
mod const_fold;

use chili_ast::ast::{Expr, ExprKind};
use chili_ast::ty::Ty;
use chili_ast::value::Value;
use chili_ast::workspace::ModuleId;
use chili_ast::{
    ast::Ast,
    workspace::{BindingInfoId, Workspace},
};
use chili_error::{DiagnosticResult, TypeError};
use chili_infer::sess::InferSess;
use chili_infer::substitute::{Substitute, SubstituteTy};
use chili_span::Span;

pub fn check(workspace: &mut Workspace, asts: &mut Vec<Ast>) -> DiagnosticResult<()> {
    let target_metrics = workspace.build_options.target_platform.metrics();
    let mut infcx = InferSess::new(target_metrics.word_size);

    {
        let mut sess = CheckSess::new(workspace, &mut infcx);

        for ast in asts.iter_mut() {
            for import in ast.imports.iter_mut() {
                sess.check_import(import)?;
            }

            for binding in ast.bindings.iter_mut() {
                sess.check_top_level_binding(binding, ast.module_id, binding.pattern.span())?;
            }
        }
    }

    let table = infcx.get_table_mut();

    for ast in asts.iter_mut() {
        for binding in ast.bindings.iter_mut() {
            binding.substitute(table)?;
        }
    }

    for binding_info in workspace.binding_infos.iter_mut() {
        binding_info.ty = binding_info.ty.substitute_ty(table, binding_info.span)?;
    }

    Ok(())
}

pub(crate) struct CheckSess<'c> {
    pub(crate) workspace: &'c mut Workspace,
    pub(crate) infcx: &'c mut InferSess,
}

impl<'c> CheckSess<'c> {
    pub(crate) fn new(workspace: &'c mut Workspace, infcx: &'c mut InferSess) -> Self {
        Self { workspace, infcx }
    }

    pub(crate) fn update_binding_info_ty(&mut self, id: BindingInfoId, ty: Ty) {
        self.workspace.get_binding_info_mut(id).unwrap().ty = ty;
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
                        self.infcx.normalize_ty_and_untyped(ty).to_string(),
                        "compile-time known integer",
                    ))
                } else {
                    Ok(value.clone().into_int())
                }
            }
            None => Err(TypeError::expected(
                span,
                self.infcx.normalize_ty_and_untyped(ty).to_string(),
                "compile-time known integer",
            )),
        }
    }
}

pub(crate) struct CheckFrame {
    pub(crate) depth: usize,
    pub(crate) module_id: ModuleId,
    pub(crate) expected_return_ty: Option<Ty>,
    pub(crate) self_types: Vec<Ty>,
}

impl CheckFrame {
    pub(crate) fn new(
        previous_depth: usize,
        module_id: ModuleId,
        expected_return_ty: Option<Ty>,
    ) -> Self {
        Self {
            depth: previous_depth + 1,
            module_id,
            expected_return_ty,
            self_types: vec![],
        }
    }
}

pub(crate) struct CheckedExpr {
    expr: Expr,
    ty: Ty,
    value: Option<Value>,
}

impl CheckedExpr {
    pub(crate) fn new(expr: ExprKind, ty: Ty, value: Option<Value>, span: Span) -> Self {
        Self {
            expr: Expr::typed(expr, ty.clone(), span),
            ty,
            value,
        }
    }
}
