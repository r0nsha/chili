use super::{symbols, Check, CheckResult, CheckSess, QueuedModule};
use crate::{
    ast,
    error::diagnostic::{Diagnostic, Label},
    hir::{self, const_value::ConstValue},
    infer::substitute::substitute_node,
    span::Span,
    types::{Type, TypeId},
    workspace::{BindingId, ModuleId},
};
use std::collections::HashSet;
use ustr::{Ustr, UstrMap};

trait CheckTopLevel
where
    Self: Sized,
{
    fn check_top_level(&self, sess: &mut CheckSess, module_id: ModuleId) -> CheckResult<UstrMap<BindingId>>;
}

impl CheckTopLevel for ast::Binding {
    fn check_top_level(&self, sess: &mut CheckSess, module_id: ModuleId) -> CheckResult<UstrMap<BindingId>> {
        let node = sess.with_env(module_id, |sess, mut env| self.check(sess, &mut env, None))?;

        if let Err(mut diagnostics) = substitute_node(&node, &mut sess.tcx) {
            let last = diagnostics.pop().unwrap();
            sess.workspace.diagnostics.extend(diagnostics);
            return Err(last);
        }

        fn collect_bound_names(node: hir::Node, bound_names: &mut UstrMap<BindingId>, sess: &mut CheckSess) {
            match node {
                hir::Node::Binding(binding) => {
                    let (name, id) = (binding.name, binding.id);
                    sess.cache.bindings.insert(id, binding);
                    bound_names.insert(name, id);
                }
                hir::Node::Sequence(sequence) => {
                    sequence.statements.into_iter().for_each(|statement| {
                        collect_bound_names(statement, bound_names, sess);
                    });
                }
                _ => unreachable!("{:#?}", node),
            }
        }

        let mut bound_names = UstrMap::<BindingId>::default();
        collect_bound_names(node, &mut bound_names, sess);

        Ok(bound_names)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallerInfo {
    pub module_id: ModuleId,
    pub span: Span,
}

impl<'s> CheckSess<'s> {
    pub fn check_top_level_binding(&mut self, name: Ustr, module_id: ModuleId, caller_info: CallerInfo) -> CheckResult {
        // In general, top level names are searched in this order:
        // 1. A binding in current module
        // 2. An extern library name
        // 3. A built-in type name
        // 4. A binding in `std` prelude

        if let Some(result) = self.find_checked_top_level_binding(name, module_id, caller_info) {
            result
        } else {
            let module = self
                .modules
                .iter()
                .find(|m| m.id == module_id)
                .unwrap_or_else(|| panic!("{:?}", module_id));

            // Check if the binding exists in the current module
            match self.check_binding_in_module(name, module, caller_info) {
                Some(Ok(node)) => Ok(node),
                Some(Err(diag)) => Err(diag),
                None => {
                    let find_library_result = self
                        .workspace
                        .libraries
                        .iter()
                        .find(|(_, library)| library.name == name);

                    if let Some((_, library)) = find_library_result {
                        let module_type = self.check_module_by_id(library.root_module_id)?;

                        Ok(hir::Node::Const(hir::Const {
                            value: ConstValue::Unit(()),
                            ty: module_type,
                            span: caller_info.span,
                        }))
                    } else if let Some(ty) = self.get_builtin_type(&name) {
                        let value = ConstValue::Type(ty);
                        let ty = self.tcx.bound_maybe_spanned(ty.as_kind().create_type(), None);

                        Ok(hir::Node::Const(hir::Const {
                            value,
                            ty,
                            span: caller_info.span,
                        }))
                    } else if let Some(result) = self.check_binding_in_std_prelude(name, caller_info) {
                        result
                    } else {
                        Err(self.name_not_found_error(module_id, name, caller_info))
                    }
                }
            }
        }
    }

    // Note(Ron): This function is pretty weird, maybe we should yeet it?
    fn find_checked_top_level_binding(
        &mut self,
        name: Ustr,
        module_id: ModuleId,
        caller_info: CallerInfo,
    ) -> Option<CheckResult<hir::Node>> {
        if let Some(id) = self.get_global_binding_id(module_id, name) {
            self.workspace.add_binding_info_use(id, caller_info.span);

            if let Err(diag) = self.validate_item_visibility(id, caller_info) {
                Some(Err(diag))
            } else {
                Some(Ok(self.id_or_const_by_id(id, caller_info.span)))
            }
        } else {
            None
        }
    }

    fn check_binding_in_module(
        &mut self,
        name: Ustr,
        module: &ast::Module,
        caller_info: CallerInfo,
    ) -> Option<CheckResult<hir::Node>> {
        let (index, binding) = module.find_binding(name)?;

        // Check that this binding isn't cyclic
        if !self.encountered_items.insert((module.id, index)) {
            return Some(Err(Diagnostic::error()
                .with_message(format!(
                    "cycle detected while checking `{}` in module `{}`",
                    name, module.info.name
                ))
                .with_label(Label::primary(caller_info.span, format!("`{}` refers to itself", name)))
                .with_label(Label::secondary(
                    binding.pattern_span(),
                    format!("`{}` is defined here", name),
                ))));
        }

        self.queued_modules
            .get_mut(&module.id)
            .unwrap()
            .queued_bindings
            .insert(index);

        match binding.check_top_level(self, module.id) {
            Ok(bound_names) => {
                let desired_id = *bound_names.get(&name).unwrap();

                self.workspace.add_binding_info_use(desired_id, caller_info.span);
                match self.validate_item_visibility(desired_id, caller_info) {
                    Ok(_) => {
                        self.encountered_items.remove(&(module.id, index));
                        Some(Ok(self.id_or_const_by_id(desired_id, caller_info.span)))
                    }
                    Err(diag) => Some(Err(diag)),
                }
            }
            Err(diag) => Some(Err(diag)),
        }
    }

    fn check_binding_in_std_prelude(&mut self, name: Ustr, caller_info: CallerInfo) -> Option<CheckResult> {
        let std_root_module_id = self.workspace.std_library().root_module_id;

        if let Some(result) = self.find_checked_top_level_binding(name, std_root_module_id, caller_info) {
            Some(result)
        } else {
            let std_root_module = self
                .modules
                .iter()
                .find(|m| m.id == std_root_module_id)
                .unwrap_or_else(|| panic!("{:?}", std_root_module_id));

            self.check_binding_in_module(name, std_root_module, caller_info)
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
            format!("cannot find value `{}` in module `{}`", name, module_info.name)
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

    pub fn validate_item_visibility(&self, id: BindingId, caller_info: CallerInfo) -> CheckResult<()> {
        let binding_info = self.workspace.binding_infos.get(id).unwrap();

        if binding_info.visibility == ast::Visibility::Private && binding_info.module_id != caller_info.module_id {
            Err(Diagnostic::error()
                .with_message(format!("associated symbol `{}` is private", binding_info.name))
                .with_label(Label::primary(caller_info.span, "accessed here"))
                .with_label(Label::secondary(binding_info.span, "defined here")))
        } else {
            Ok(())
        }
    }

    pub fn check_module_by_id(&mut self, id: ModuleId) -> CheckResult<TypeId> {
        let module = self
            .modules
            .iter()
            .find(|m| m.id == id)
            .unwrap_or_else(|| panic!("couldn't find {:?}", id));

        self.check_module(module)
    }

    pub fn check_module(&mut self, module: &ast::Module) -> CheckResult<TypeId> {
        if let Some(ty) = self.get_completed_module_type(module.id) {
            Ok(ty)
        } else {
            let module_type = match self.queued_modules.get(&module.id) {
                Some(queued) => queued.module_type,
                None => {
                    let span = Span::initial(module.file_id);
                    let module_type = self.tcx.bound(Type::Module(module.id), span);

                    // Add the module to the queued modules map
                    self.queued_modules.insert(
                        module.id,
                        QueuedModule {
                            module_type,
                            all_complete: false,
                            queued_bindings: HashSet::new(),
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
                    .queued_bindings
                    .insert(index)
                {
                    binding.check_top_level(self, module.id)?;
                }
            }

            self.queued_modules.get_mut(&module.id).unwrap().all_complete = true;

            for r#static in module.comptime_blocks.iter() {
                let node = self.with_env(module.id, |sess, mut env| r#static.check(sess, &mut env, None))?;

                if !self.workspace.build_options.check_mode {
                    self.eval(&node, module.id, r#static.span)?;
                }
            }

            Ok(module_type)
        }
    }

    fn get_completed_module_type(&self, id: ModuleId) -> Option<TypeId> {
        match self.queued_modules.get(&id) {
            Some(QueuedModule {
                module_type,
                all_complete: true,
                ..
            }) => Some(*module_type),
            _ => None,
        }
    }

    fn get_builtin_type(&self, name: &str) -> Option<TypeId> {
        match name {
            symbols::SYM_UNIT => Some(self.tcx.common_types.unit),
            symbols::SYM_BOOL => Some(self.tcx.common_types.bool),

            symbols::SYM_I8 => Some(self.tcx.common_types.i8),
            symbols::SYM_I16 => Some(self.tcx.common_types.i16),
            symbols::SYM_I32 => Some(self.tcx.common_types.i32),
            symbols::SYM_I64 => Some(self.tcx.common_types.i64),
            symbols::SYM_INT => Some(self.tcx.common_types.int),

            symbols::SYM_U8 => Some(self.tcx.common_types.u8),
            symbols::SYM_U16 => Some(self.tcx.common_types.u16),
            symbols::SYM_U32 => Some(self.tcx.common_types.u32),
            symbols::SYM_U64 => Some(self.tcx.common_types.u64),
            symbols::SYM_UINT => Some(self.tcx.common_types.uint),

            symbols::SYM_F16 => Some(self.tcx.common_types.f16),
            symbols::SYM_F32 => Some(self.tcx.common_types.f32),
            symbols::SYM_F64 => Some(self.tcx.common_types.f64),
            symbols::SYM_FLOAT => Some(self.tcx.common_types.float),

            symbols::SYM_NEVER => Some(self.tcx.common_types.never),

            symbols::SYM_STR => Some(self.tcx.common_types.str),

            _ => None,
        }
    }
}
