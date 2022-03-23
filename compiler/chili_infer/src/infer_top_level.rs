use chili_ast::{ast, ty::Ty, workspace::BindingInfoId};

use crate::new_infer::{Infer, InferResult, InferSess, Res};

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

impl InferTopLevel for ast::Import {
    fn infer_top_level(&mut self, sess: &mut InferSess) -> InferResult {
        let res = self.infer(sess)?;
        sess.new_ast.add_import(self.clone());
        Ok(res)
    }
}

impl<'s> InferSess<'s> {
    pub(crate) fn infer_binding_by_id(&mut self, id: BindingInfoId) -> InferResult {
        let binding_info = self.workspace.get_binding_info(id).unwrap();

        if binding_info.ty == Ty::unknown() {
            match self.old_ast.get_binding(id) {
                Some(binding) => {
                    binding.clone().infer_top_level(self)?;
                }
                None => match self.old_ast.get_import(id) {
                    Some(import) => {
                        import.clone().infer_top_level(self)?;
                    }
                    None => unreachable!(),
                },
            }
        }

        let binding_info = self.workspace.get_binding_info(id).unwrap();

        // TODO: return const value
        Ok(Res::new(binding_info.ty))
    }
}
