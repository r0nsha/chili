use chili_ast::{
    ast,
    ty::Ty,
    workspace::{BindingInfoId, ModuleId},
};
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

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

        Ok(Res::new_maybe_const(
            binding_info.ty,
            self.const_bindings.get(&id).map(|v| *v),
        ))
    }

    pub(crate) fn find_binding_info_id_in_module(
        &self,
        module_id: ModuleId,
        symbol: Ustr,
        span: Span,
    ) -> DiagnosticResult<BindingInfoId> {
        let info = self.workspace.find_binding_info_by_name(module_id, symbol);

        match info {
            Some(info) => Ok(info.id),
            None => {
                let module_info = self.workspace.get_module_info(module_id).unwrap();

                return Err(Diagnostic::error()
                    .with_message(format!(
                        "cannot find value `{}` in module `{}`",
                        symbol, module_info.name
                    ))
                    .with_labels(vec![Label::primary(span.file_id, span.range())
                        .with_message(format!("not found in `{}`", module_info.name))]));
            }
        }
    }
}
