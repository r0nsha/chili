use super::{Check, CheckSess, QueuedModule};
use crate::{
    ast,
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult,
    },
    hir,
    span::Span,
    types::{Type, TypeId},
    workspace::{BindingId, ModuleId},
};
use std::collections::HashSet;
use ustr::{Ustr, UstrMap};

pub(super) trait CheckTopLevel
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
                sess.cache.bindings.insert(id, binding);
                bound_names.insert(name, id);
            }
            hir::Node::Sequence(sequence) => {
                for statement in sequence.statements.into_iter() {
                    let binding = statement.into_binding().unwrap();
                    let (name, id) = (binding.name, binding.id);

                    sess.cache.bindings.insert(id, binding);

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
        if let Some(id) = self.get_global_binding_id(module_id, name) {
            self.workspace.add_binding_info_use(id, caller_info.span);
            self.validate_can_access_item(id, caller_info)?;

            Ok(id)
        } else {
            let module = self
                .modules
                .iter()
                .find(|m| m.id == module_id)
                .unwrap_or_else(|| panic!("{:?}", module_id));

            if let Some(binding) = module.bindings.iter().find(|binding| match &binding.kind {
                ast::BindingKind::Orphan { pattern, .. } => {
                    pattern.iter().any(|pattern| pattern.name == name)
                }
                ast::BindingKind::ExternFunction {
                    name:
                        ast::NameAndSpan {
                            name: binding_name, ..
                        },
                    ..
                }
                | ast::BindingKind::ExternVariable {
                    name:
                        ast::NameAndSpan {
                            name: binding_name, ..
                        },
                    ..
                }
                | ast::BindingKind::Intrinsic {
                    name:
                        ast::NameAndSpan {
                            name: binding_name, ..
                        },
                    ..
                } => *binding_name == name,
            }) {
                let bound_names = binding.check_top_level(self)?;
                let desired_id = *bound_names.get(&name).unwrap();

                self.workspace
                    .add_binding_info_use(desired_id, caller_info.span);

                self.validate_can_access_item(desired_id, caller_info)?;

                Ok(desired_id)
            } else if let Some(&builtin_id) = self.builtin_types.get(&name) {
                self.workspace
                    .add_binding_info_use(builtin_id, caller_info.span);

                Ok(builtin_id)
            } else {
                Err(self.name_not_found_error(module_id, name, caller_info))
            }
        }
    }

    pub(super) fn name_not_found_error(
        &mut self,
        module_id: ModuleId,
        name: Ustr,
        caller_info: CallerInfo,
    ) -> Diagnostic {
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

        Diagnostic::error()
            .with_message(message)
            .with_label(Label::primary(caller_info.span, label_message))
    }

    pub fn validate_can_access_item(
        &self,
        id: BindingId,
        caller_info: CallerInfo,
    ) -> DiagnosticResult<()> {
        let binding_info = self.workspace.binding_infos.get(id).unwrap();

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

    pub fn check_module_by_id(&mut self, id: ModuleId) -> DiagnosticResult<TypeId> {
        let module = self
            .modules
            .iter()
            .find(|m| m.id == id)
            .unwrap_or_else(|| panic!("couldn't find {:?}", id));

        self.check_module(module)
    }

    pub fn check_module(&mut self, module: &ast::Module) -> DiagnosticResult<TypeId> {
        match self.queued_modules.get(&module.id) {
            Some(QueuedModule {
                module_type,
                all_complete: true,
                ..
            }) => Ok(*module_type),
            _ => {
                let module_type = match self.queued_modules.get(&module.id) {
                    Some(queued) => queued.module_type,
                    None => {
                        let module_type = self
                            .tycx
                            .bound(Type::Module(module.id), Span::initial(module.file_id));

                        self.queued_modules.insert(
                            module.id,
                            QueuedModule {
                                module_type,
                                all_complete: false,
                                complete_bindings: HashSet::new(),
                            },
                        );

                        module_type
                    }
                };

                for (index, binding) in module.bindings.iter().enumerate() {
                    if self
                        .queued_modules
                        .get_mut(&module.id)
                        .unwrap()
                        .complete_bindings
                        .insert(index)
                    {
                        binding.check_top_level(self)?;
                    }
                }

                self.queued_modules
                    .get_mut(&module.id)
                    .unwrap()
                    .all_complete = true;

                for expr in module.run_exprs.iter() {
                    // let expr = expr.clone();
                    let node =
                        self.with_env(module.id, |sess, mut env| expr.check(sess, &mut env, None))?;

                    if !self.workspace.build_options.check_mode {
                        self.eval(&node, module.id).unwrap();
                    }
                }

                Ok(module_type)
            }
        }
    }
}
