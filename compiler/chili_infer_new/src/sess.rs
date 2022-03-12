use crate::tvar::{TyVar, TyVarGen};

pub(crate) struct InferSess {
    tvg: TyVarGen,
}

impl InferSess {
    pub(crate) fn new() -> Self {
        Self {
            tvg: TyVarGen::new(),
        }
    }

    pub(crate) fn new_variable(&mut self) -> TyVar {
        self.tvg.next()
    }
}
