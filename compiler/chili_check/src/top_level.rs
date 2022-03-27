use chili_ast::{ast, workspace::BindingInfoId};
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

use crate::{Check, CheckResult, CheckSess, Res};

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
                Ok((self.get_binding_res(id).unwrap(), id))
                // if let Some(binding_info) = sess.workspace.get_binding_info(id) {
                //     let min_scope_level = sess
                //         .function_frame()
                //         .map_or(ScopeLevel::Global, |f| f.scope_level);

                //     if !binding_info.kind.is_type()
                //         && !binding_info.scope_level.is_global()
                //         && binding_info.scope_level < min_scope_level
                //     {
                //         return Err(Diagnostic::error()
                //             .with_message("can't capture dynamic environment yet - not implemented")
                //             .with_labels(vec![Label::primary(
                //                 self.span.file_id,
                //                 self.span.range(),
                //             )]));
                //     }

                //     Ok(Res::new_maybe_const(
                //         binding_info.ty,
                //         sess.env.const_bindings.get(&id).map(|v| *v),
                //     ))
                // } else {
                //     sess.check_binding_by_symbol(*binding_info_id)
                // }
            }
            None => {
                for ast in self.old_asts.iter() {
                    for binding in ast.bindings.iter() {
                        let pat = binding.pattern.as_single_ref();
                        if pat.symbol == symbol {
                            let id = pat.binding_info_id;
                            binding.clone().check_top_level(self)?;
                            return Ok((self.get_binding_res(id).unwrap(), id));
                        }
                    }

                    for import in ast.imports.iter() {
                        if import.alias == symbol {
                            let id = import.binding_info_id;
                            import.clone().check_top_level(self)?;
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
