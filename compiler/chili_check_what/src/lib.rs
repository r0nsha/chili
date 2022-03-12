mod check_assign;
mod check_binary;
mod check_binding;
mod check_call;
mod check_expr;
mod check_fn;
mod check_pattern;
mod check_unary;
mod const_fold;

use std::collections::HashMap;

use chili_ast::ty::TyKind;
use chili_ast::value::Value;
use chili_ast::workspace::ModuleIdx;
use chili_ast::{
    ast::Ast,
    workspace::{BindingInfoIdx, Workspace},
};
use chili_error::DiagnosticResult;
use chili_infer::sess::InferSess;
use chili_infer::substitute::{Substitute, SubstituteTy};
use common::scopes::Scopes;

pub fn check<'w>(workspace: &mut Workspace<'w>, asts: &mut Vec<Ast>) -> DiagnosticResult<()> {
    let target_metrics = workspace.build_options.target_platform.metrics();
    let mut infcx = InferSess::new(target_metrics.word_size);

    // infer types
    {
        let mut sess = CheckSess::new(workspace, &mut infcx);

        sess.init_scopes.push_scope();

        for ast in asts.iter_mut() {
            for import in ast.imports.iter_mut() {
                sess.check_import(import)?;
            }

            for binding in ast.bindings.iter_mut() {
                sess.check_top_level_binding(binding, ast.module_idx, binding.pattern.span())?;
            }
        }

        sess.init_scopes.pop_scope();
    }

    // substitute type variables
    let table = infcx.get_table_mut();

    for ast in asts.iter_mut() {
        for binding in ast.bindings.iter_mut() {
            binding.substitute(table)?;
        }
    }

    for binding_info in workspace.binding_infos.iter_mut() {
        binding_info.ty = binding_info.ty.substitute(table, binding_info.span)?;
    }

    Ok(())
}

pub(crate) struct CheckSess<'w, 'a> {
    pub(crate) workspace: &'a mut Workspace<'w>,
    pub(crate) infcx: &'a mut InferSess,
    pub(crate) consts_map: HashMap<BindingInfoIdx, Value>,
    pub(crate) init_scopes: Scopes<BindingInfoIdx, InitState>,
}

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn new(workspace: &'a mut Workspace<'w>, infcx: &'a mut InferSess) -> Self {
        Self {
            workspace,
            infcx,
            consts_map: Default::default(),
            init_scopes: Scopes::new(),
        }
    }

    pub(crate) fn update_binding_info_ty(&mut self, idx: BindingInfoIdx, ty: TyKind) {
        self.workspace.get_binding_info_mut(idx).unwrap().ty = ty;
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
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
    pub(crate) depth: usize,
    pub(crate) module_idx: ModuleIdx,
    pub(crate) expected_return_ty: Option<TyKind>,
    pub(crate) self_types: Vec<TyKind>,
}

impl CheckFrame {
    pub(crate) fn new(
        previous_depth: usize,
        module_idx: ModuleIdx,
        expected_return_ty: Option<TyKind>,
    ) -> Self {
        Self {
            depth: previous_depth + 1,
            module_idx,
            expected_return_ty,
            self_types: vec![],
        }
    }
}

pub(crate) struct CheckResult {
    pub(crate) ty: TyKind,
    pub(crate) value: Option<Value>,
}

impl CheckResult {
    pub(crate) fn new(ty: TyKind, value: Option<Value>) -> Self {
        Self { ty, value }
    }
}
