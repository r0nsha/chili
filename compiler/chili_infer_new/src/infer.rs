use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;

use crate::sess::InferSess;

pub(crate) trait Infer {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()>;
}

impl Infer for ast::Ast {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
        for binding_info in workspace.binding_infos.iter_mut() {
            if binding_info.ty.is_unknown() {
                binding_info.ty = sess.new_variable().into();
                println!("{}", binding_info.ty);
            } else {
                println!("bound: {:?}", binding_info.ty);
            }
        }

        for import in self.imports.iter_mut() {
            import.infer(sess, workspace)?;
        }

        for binding in self.bindings.iter_mut() {
            binding.infer(sess, workspace)?;
        }

        Ok(())
    }
}

impl Infer for ast::Import {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
        todo!("import")
    }
}

impl Infer for ast::Binding {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
        todo!()
    }
}
