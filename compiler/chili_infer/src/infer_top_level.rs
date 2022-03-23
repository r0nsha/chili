use chili_ast::ast;

use crate::new_infer::{Infer, InferResult, InferSess};

pub(crate) trait InferTopLevel
where
    Self: Sized,
{
    fn infer_top_level(&mut self, sess: &mut InferSess) -> InferResult;
}

impl InferTopLevel for ast::Binding {
    fn infer_top_level(&mut self, sess: &mut InferSess) -> InferResult {
        let res = self.infer(sess)?;
        sess.new_ast.add_binding(self.clone());
        Ok(res)
    }
}
