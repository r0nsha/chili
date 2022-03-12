use chili_ast::{
    ast::{self, Ast},
    workspace::Workspace,
};
use chili_error::DiagnosticResult;

pub(crate) struct InferSess<'i> {
    pub(crate) workspace: &'i mut Workspace,
}

pub(crate) trait Infer<'i> {
    fn infer(&mut self, sess: &mut InferSess<'i>) -> DiagnosticResult<()>;
}

impl<'i> Infer<'i> for Ast {
    fn infer(&mut self, sess: &mut InferSess<'i>) -> DiagnosticResult<()> {
        todo!()
    }
}
