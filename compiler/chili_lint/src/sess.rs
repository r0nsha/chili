use chili_ast::workspace::{BindingInfoId, Workspace};
use chili_infer::ty_ctx::TyCtx;
use common::scopes::Scopes;

pub(crate) struct LintSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) tycx: &'s TyCtx,
    pub(crate) init_scopes: Scopes<BindingInfoId, InitState>,
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
