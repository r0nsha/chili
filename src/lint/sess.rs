use crate::{
    common::scopes::Scopes,
    infer::ty_ctx::TyCtx,
    workspace::{BindingId, Workspace},
};

pub struct LintSess<'s> {
    pub workspace: &'s mut Workspace,
    pub tycx: &'s TyCtx,
    pub init_scopes: Scopes<BindingId, InitState>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum InitState {
    NotInit,
    Init,
}

impl InitState {
    pub fn is_not_init(&self) -> bool {
        matches!(self, InitState::NotInit)
    }

    pub fn is_init(&self) -> bool {
        matches!(self, InitState::Init)
    }
}
