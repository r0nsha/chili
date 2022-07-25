use crate::{
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult,
    },
    hir::{
        self,
        attrs::{Attr, AttrKind},
        const_value::ConstValue,
    },
    infer::normalize::Normalize,
    span::Span,
};

use super::{env::Env, CheckSess};

impl<'s> CheckSess<'s> {
    pub(super) fn try_assign_entry_point_function(
        &mut self,
        env: &mut Env,
        value: &hir::Node,
        attr: &Attr,
        binding_span: Span,
    ) -> DiagnosticResult<()> {
        if let Some(ConstValue::Function(f)) = value.as_const_value() {
            let function = self.cache.functions.get(f.id).unwrap();

            match &function.kind {
                hir::FunctionKind::Orphan { .. } => {
                    let ty = function.ty.normalize(&self.tcx).into_function();

                    // Validate its type is fn() -> ()
                    if !(ty.return_type.is_unit() || ty.return_type.is_never())
                        || !ty.params.is_empty()
                        || ty.varargs.is_some()
                    {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "entry point function `{}` has type `{}`, expected `fn() -> ()`",
                                function.name, ty
                            ))
                            .with_label(Label::primary(binding_span, "invalid entry point function type")));
                    }

                    // Validate it's in the root module
                    if env.module_id() != self.workspace.root_module_id {
                        return Err(Diagnostic::error()
                            .with_message("entry point function must be declared in the root module")
                            .with_label(Label::primary(
                                binding_span,
                                format!("declared in `{}`", env.module_info().name),
                            )));
                    }

                    // Validate it's in global scope
                    if !env.scope_level().is_global() {
                        return Err(Diagnostic::error()
                            .with_message("entry point function must be declared at the global scope")
                            .with_label(Label::primary(binding_span, "not declared at global scope")));
                    }

                    // Validate that this is the only entry point function
                    if let Some(entry_point_function_id) = self.cache.entry_point_function_id {
                        let (_, entry_point_function_binding) = self
                            .cache
                            .bindings
                            .iter()
                            .find(|(_, binding)| {
                                if let Some(ConstValue::Function(f)) = binding.value.as_const_value() {
                                    f.id == entry_point_function_id
                                } else {
                                    false
                                }
                            })
                            .unwrap();

                        Err(Diagnostic::error()
                            .with_message("entry point function has already been declared")
                            .with_label(Label::primary(binding_span, "duplicate entry point function"))
                            .with_label(Label::secondary(
                                entry_point_function_binding.span,
                                "already declared here",
                            )))
                    } else {
                        self.cache.entry_point_function_id = Some(function.id);
                        Ok(())
                    }
                }
                _ => Err(Diagnostic::error()
                    .with_message(format!(
                        "the `{}` attribute cannot be used on extern functions",
                        AttrKind::Entry,
                    ))
                    .with_label(Label::primary(attr.span, "cannot be used on extern functions"))),
            }
        } else {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "the `{}` attribute can only be used on function bindings",
                    AttrKind::Entry,
                ))
                .with_label(Label::primary(attr.span, "only valid on function bindings")));
        }
    }

    pub(super) fn check_entry_point_function_exists(&mut self) {
        if self.workspace.build_options.need_entry_point_function() {
            if self.cache.entry_point_function_id.is_none() {
                self.workspace.diagnostics.push(
                    Diagnostic::error()
                        .with_message("entry point function is not defined")
                        .with_label(Label::primary(
                            Span::initial(self.workspace.get_root_module_info().file_id),
                            "",
                        ))
                        .with_note("define function `#[entry] let main = fn() {}` in your root file"),
                );
            }
        }
    }
}
