use crate::{Check, CheckResult, CheckSess, Res};
use chili_ast::{
    ast,
    workspace::{BindingInfo, BindingInfoId, ModuleId},
};
use chili_error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult,
};
use chili_span::Span;
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
        sess.new_ast
            .bindings
            .insert(self.pattern.as_single_ref().binding_info_id, self.clone());
        Ok(res)
    }
}

impl CheckTopLevel for ast::Import {
    fn check_top_level(&mut self, sess: &mut CheckSess, module_id: ModuleId) -> CheckResult {
        let res = sess.with_env(module_id, |sess, mut env| self.check(sess, &mut env, None))?;
        sess.new_ast
            .imports
            .insert(self.binding_info_id, self.clone());
        Ok(res)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CallerInfo {
    pub(crate) module_id: ModuleId,
    pub(crate) span: Span,
}

impl<'s> CheckSess<'s> {
    pub(crate) fn check_top_level_symbol(
        &mut self,
        caller_info: CallerInfo,
        module_id: ModuleId,
        symbol: Ustr,
    ) -> DiagnosticResult<(Res, BindingInfoId)> {
        // check if the binding has already been checked
        if let Some(id) = self.get_global_symbol(module_id, symbol) {
            // this binding has already been checked, so just return its data
            let binding_info = self.workspace.get_binding_info(id).unwrap();
            self.validate_can_access_item(binding_info, caller_info)?;

            return Ok((
                Res::new_maybe_const(binding_info.ty, binding_info.const_value),
                id,
            ));
        }

        // this binding hasn't been checked yet - check it and then return its data
        let ast = self
            .old_asts
            .iter()
            .find(|ast| ast.module_id == module_id)
            .expect(&format!("{:?}", module_id));

        if let Some(binding) = ast
            .bindings
            .iter()
            .find(|binding| binding.pattern.as_single_ref().symbol == symbol)
        {
            // this symbol points to a binding
            let mut binding = binding.clone();

            let res = binding.check_top_level(self, ast.module_id)?;
            let id = binding.pattern.as_single_ref().binding_info_id;

            self.validate_can_access_item(
                self.workspace.get_binding_info(id).unwrap(),
                caller_info,
            )?;

            Ok((res, id))
        } else if let Some(import) = ast.imports.iter().find(|import| import.alias == symbol) {
            // this symbol points to an import
            let mut import = import.clone();

            let res = import.check_top_level(self, ast.module_id)?;
            let id = import.binding_info_id;

            self.validate_can_access_item(
                self.workspace.get_binding_info(id).unwrap(),
                caller_info,
            )?;

            Ok((res, id))
        } else if let Some(&builtin_id) = self.builtin_types.get(&symbol) {
            // this is a builtin symbol, such as i32, bool, etc.
            let res = self
                .workspace
                .get_binding_info(builtin_id)
                .map(|binding_info| Res::new_maybe_const(binding_info.ty, binding_info.const_value))
                .unwrap();

            Ok((res, builtin_id))
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
                .with_label(Label::primary(caller_info.span, label_message)))
        }
    }

    fn validate_can_access_item(
        &self,
        binding_info: &BindingInfo,
        caller_info: CallerInfo,
    ) -> DiagnosticResult<()> {
        if binding_info.visibility == ast::Visibility::Private
            && binding_info.module_id != caller_info.module_id
        {
            Err(Diagnostic::error()
                .with_message(format!(
                    "associated symbol `{}` is private",
                    binding_info.symbol
                ))
                .with_label(Label::primary(caller_info.span, "accessed here"))
                .with_label(Label::secondary(binding_info.span, "defined here")))
        } else {
            Ok(())
        }
    }
}
