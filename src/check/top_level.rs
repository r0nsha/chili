use super::{Check, CheckSess};
use crate::ast::ty::TypeId;
use crate::error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult,
};
use crate::hir;
use crate::span::Span;
use crate::{
    ast::{
        self,
        ty::Type,
        workspace::{BindingId, BindingInfo, ModuleId},
    },
    check::interp_expr,
};
use ustr::{Ustr, UstrMap};

pub trait CheckTopLevel
where
    Self: Sized,
{
    fn check_top_level(&self, sess: &mut CheckSess) -> DiagnosticResult<UstrMap<BindingId>>;
}

impl CheckTopLevel for ast::Binding {
    fn check_top_level(&self, sess: &mut CheckSess) -> DiagnosticResult<UstrMap<BindingId>> {
        let node = sess.with_env(self.module_id, |sess, mut env| {
            self.check(sess, &mut env, None)
        })?;

        let mut bound_names = UstrMap::<BindingId>::default();

        match node {
            hir::Node::Binding(binding) => {
                let (name, id) = (binding.name, binding.id);
                sess.cache.bindings.insert(binding);
                bound_names.insert(name, id);
            }
            hir::Node::Sequence(sequence) => {
                for statement in sequence.statements.into_iter() {
                    let binding = statement.into_binding().unwrap();
                    let (name, id) = (binding.name, binding.id);

                    sess.cache.bindings.insert(binding);

                    bound_names.insert(name, id);
                }
            }
            _ => (),
        }

        Ok(bound_names)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallerInfo {
    pub module_id: ModuleId,
    pub span: Span,
}

impl<'s> CheckSess<'s> {
    pub fn check_top_level_binding(
        &mut self,
        caller_info: CallerInfo,
        module_id: ModuleId,
        name: Ustr,
    ) -> DiagnosticResult<BindingId> {
        // check if the binding has already been checked
        if let Some(id) = self.get_global_binding_id(module_id, name) {
            // this binding has already been checked, so just return its data

            self.workspace.add_binding_info_use(id, caller_info.span);

            let binding_info = self.workspace.binding_infos.get(id).unwrap();
            self.validate_can_access_item(binding_info, caller_info)?;

            Ok(id)
        } else {
            // this binding hasn't been checked yet - check it and then return its data
            let module = self
                .modules
                .iter()
                .find(|m| m.module_id == module_id)
                .unwrap_or_else(|| panic!("{:?}", module_id));

            if let Some(binding) = module
                .bindings
                .iter()
                .find(|binding| binding.pattern.iter().any(|pat| pat.name == name))
            {
                let bound_names = binding.check_top_level(self)?;
                let desired_id = *bound_names.get(&name).unwrap();

                self.workspace
                    .add_binding_info_use(desired_id, caller_info.span);

                let desired_binding_info = self.workspace.binding_infos.get(desired_id).unwrap();

                if desired_binding_info.name == "Workspace" {
                    println!("{:#?}", desired_binding_info);
                }

                self.validate_can_access_item(desired_binding_info, caller_info)?;

                Ok(desired_binding_info.id)
            } else if let Some(&builtin_id) = self.builtin_types.get(&name) {
                self.workspace
                    .add_binding_info_use(builtin_id, caller_info.span);

                Ok(builtin_id)
            } else {
                let module_info = self.workspace.module_infos.get(module_id).unwrap();

                let message = if module_info.name.is_empty() {
                    format!("cannot find value `{}` in this scope", name)
                } else {
                    format!(
                        "cannot find value `{}` in module `{}`",
                        name, module_info.name
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
    }

    pub fn validate_can_access_item(
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
                    binding_info.name
                ))
                .with_label(Label::primary(caller_info.span, "accessed here"))
                .with_label(Label::secondary(binding_info.span, "defined here")))
        } else {
            Ok(())
        }
    }

    pub fn check_module(&mut self, module: &ast::Module) -> DiagnosticResult<TypeId> {
        if let Some(module_type) = self.checked_modules.get(&module.module_id) {
            Ok(*module_type)
        } else {
            let module_id = module.module_id;

            let module_type = self
                .tycx
                .bound(Type::Module(module_id), Span::initial(module.file_id));

            self.checked_modules.insert(module.module_id, module_type);

            for binding in module.bindings.iter() {
                // 6/6/2022: a binding's pattern has a count of 0 only when a single wildcard symbol is used
                if binding.pattern.iter().count() == 0
                    || binding.pattern.iter().all(|pattern| {
                        self.get_global_binding_id(module_id, pattern.name)
                            .is_none()
                    })
                {
                    binding.clone().check_top_level(self)?;
                }
            }

            for expr in module.run_exprs.iter() {
                // let expr = expr.clone();
                let node =
                    self.with_env(module_id, |sess, mut env| expr.check(sess, &mut env, None))?;

                if !self.workspace.build_options.check_mode {
                    todo!("interp node");
                    // interp_expr(&expr, self, module_id).unwrap();
                }
            }

            Ok(module_type)
        }
    }
}
