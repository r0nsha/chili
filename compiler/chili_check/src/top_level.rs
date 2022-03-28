use crate::{Check, CheckResult, CheckSess, Res};
use chili_ast::{
    ast,
    workspace::{BindingInfoId, ModuleId},
};
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

pub(crate) trait CheckTopLevel
where
    Self: Sized,
{
    fn check_top_level(&mut self, sess: &mut CheckSess, module_id: ModuleId) -> CheckResult;
}

impl CheckTopLevel for ast::Binding {
    fn check_top_level(&mut self, sess: &mut CheckSess, module_id: ModuleId) -> CheckResult {
        let res = sess.with_env(module_id, |sess, mut env| self.check(sess, &mut env, None))?;
        sess.new_ast.add_binding(self.clone());
        Ok(res)
    }
}

impl CheckTopLevel for ast::Import {
    fn check_top_level(&mut self, sess: &mut CheckSess, module_id: ModuleId) -> CheckResult {
        let res = sess.with_env(module_id, |sess, mut env| self.check(sess, &mut env, None))?;
        sess.new_ast.add_import(self.clone());
        Ok(res)
    }
}

impl<'s> CheckSess<'s> {
    pub(crate) fn check_top_level_symbol(
        &mut self,
        module_id: ModuleId,
        symbol: Ustr,
        span: Span,
    ) -> DiagnosticResult<(Res, BindingInfoId)> {
        // check if the binding has already been checked
        if let Some(id) = self.get_global_symbol(module_id, symbol) {
            // this binding has already been checked, so just return its data
            return Ok((self.get_binding_res(id).unwrap(), id));
        }

        // this binding hasn't been checked yet - check it and then return its data
        let ast = self
            .old_asts
            .iter()
            .find(|ast| ast.module_id == module_id)
            .unwrap();

        if let Some(binding) = ast
            .bindings
            .iter()
            .find(|binding| binding.pattern.as_single_ref().symbol == symbol)
        {
            // this symbol points to a binding
            let mut binding = binding.clone();
            let res = binding.check_top_level(self, ast.module_id)?;
            Ok((res, binding.pattern.as_single_ref().binding_info_id))
        } else if let Some(import) = ast.imports.iter().find(|import| import.alias == symbol) {
            // this symbol points to an import
            let mut import = import.clone();
            let res = import.check_top_level(self, ast.module_id)?;
            Ok((res, import.binding_info_id))
        } else if let Some(&builtin_id) = self.builtin_types.get(&symbol) {
            // this is a builtin symbol, such as i32, bool, etc.
            Ok((self.get_binding_res(builtin_id).unwrap(), builtin_id))
        } else {
            // the symbol doesn't exist, return an error
            let module_info = self.workspace.get_module_info(module_id).unwrap();

            let message = if module_info.name.is_empty() {
                format!("cannot find value `{}` in this scope", symbol)
            } else {
                format!(
                    "cannot find value `{}` in module `{}`",
                    symbol, module_info.name
                )
            };

            let label_message = if module_info.name.is_empty() {
                "not found in this scope".to_string()
            } else {
                format!("not found in `{}`", module_info.name)
            };

            Err(Diagnostic::error()
                .with_message(message)
                .with_labels(vec![
                    Label::primary(span.file_id, span.range()).with_message(label_message)
                ]))
        }
    }
}
