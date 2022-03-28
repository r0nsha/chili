use chili_ast::{ast, workspace::BindingInfoId};
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

use crate::{display::DisplayTy, Check, CheckResult, CheckSess, Res};

pub(crate) trait CheckTopLevel
where
    Self: Sized,
{
    fn check_top_level(&mut self, sess: &mut CheckSess) -> CheckResult;
}

impl CheckTopLevel for ast::Binding {
    fn check_top_level(&mut self, sess: &mut CheckSess) -> CheckResult {
        sess.set_module(self.module_id);
        let res = self.check(sess, None)?;
        sess.new_ast.add_binding(self.clone());
        Ok(res)
    }
}

impl CheckTopLevel for ast::Import {
    fn check_top_level(&mut self, sess: &mut CheckSess) -> CheckResult {
        sess.set_module(self.module_id);
        let res = self.check(sess, None)?;
        sess.new_ast.add_import(self.clone());
        Ok(res)
    }
}

impl<'s> CheckSess<'s> {
    pub(crate) fn check_symbol(
        &mut self,
        symbol: Ustr,
        span: Span,
    ) -> DiagnosticResult<(Res, BindingInfoId)> {
        // if let None = self.workspace.get_binding_info(id) {
        //     match self.old_asts.get_binding(id) {
        //         Some(binding) => {
        //             binding.clone().check_top_level(self)?;
        //         }
        //         None => match self.old_asts.get_import(id) {
        //             Some(import) => {
        //                 import.clone().check_top_level(self)?;
        //             }
        //             None => {
        //                 // TODO: builtin types
        //             }
        //         },
        //     }
        // }

        // let binding_info = self.workspace.get_binding_info(id).unwrap();

        // Ok(Res::new_maybe_const(
        //     binding_info.ty,
        //     self.const_bindings.get(&id).map(|v| *v),
        // ))

        match self.env.lookup_binding(self.workspace, symbol) {
            Some(id) => {
                // this binding has already been check, so just return its data
                Ok((self.get_binding_res(id).unwrap(), id))
            }
            None => {
                // this binding hasn't been checked yet - check it and then return its data
                for ast in self.old_asts.iter() {
                    for binding in ast.bindings.iter() {
                        if binding.pattern.as_single_ref().symbol == symbol {
                            let mut binding = binding.clone();
                            binding.check_top_level(self)?;
                            let id = binding.pattern.as_single_ref().binding_info_id;
                            return Ok((self.get_binding_res(id).unwrap(), id));
                        }
                    }

                    for import in ast.imports.iter() {
                        if import.alias == symbol {
                            let mut import = import.clone();
                            import.check_top_level(self)?;
                            let id = import.binding_info_id;
                            return Ok((self.get_binding_res(id).unwrap(), id));
                        }
                    }
                }

                Err(Diagnostic::error()
                    .with_message(format!("cannot find symbol `{}` in this scope", symbol))
                    .with_labels(vec![Label::primary(span.file_id, span.range())
                        .with_message("not found in this scope")]))
            }
        }
    }

    // pub(crate) fn find_binding_info_id_in_module(
    //     &self,
    //     module_id: ModuleId,
    //     symbol: Ustr,
    //     span: Span,
    // ) -> DiagnosticResult<BindingInfoId> {
    //     let info = self.workspace.find_binding_info_by_name(module_id, symbol);

    //     match info {
    //         Some(info) => Ok(info.id),
    //         None => {
    //             let module_info = self.workspace.get_module_info(module_id).unwrap();

    //             return Err(Diagnostic::error()
    //                 .with_message(format!(
    //                     "cannot find symbol `{}` in module `{}`",
    //                     symbol, module_info.name
    //                 ))
    //                 .with_labels(vec![Label::primary(span.file_id, span.range())
    //                     .with_message(format!("not found in `{}`", module_info.name))]));
    //         }
    //     }
    // }

    pub(crate) fn get_binding_res(&self, id: BindingInfoId) -> Option<Res> {
        self.workspace.get_binding_info(id).map(|binding_info| {
            Res::new_maybe_const(
                binding_info.ty,
                self.env.const_bindings.get(&id).map(|v| *v),
            )
        })
    }
}
