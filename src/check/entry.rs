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
        value: &hir::Node,
        attr: &Attr,
        env: &mut Env,
    ) -> DiagnosticResult<()> {
        if let Some(ConstValue::Function(f)) = value.as_const_value() {
            let function = self.cache.functions.get(f.id).unwrap();

            match &function.kind {
                hir::FunctionKind::Orphan { .. } => {
                    let ty = function.ty.normalize(&self.tcx).into_function();

                    // if this is the main function, check its type matches a fn() -> [unit | never]
                    if !(ty.return_type.is_unit() || ty.return_type.is_never())
                        || !ty.params.is_empty()
                        || ty.varargs.is_some()
                    {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "entry point function `{}` has type `{}`, expected `fn() -> ()`",
                                function.name, ty
                            ))
                            .with_label(Label::primary(function.span, "invalid entry point function type")));
                    }

                    if env.module_id() != self.workspace.root_module_id {
                        return Err(Diagnostic::error()
                            .with_message("entry point function must be declared in the root module")
                            .with_label(Label::primary(
                                function.span,
                                format!("declared in `{}`", env.module_info().name),
                            )));
                    }

                    if !env.scope_level().is_global() {
                        return Err(Diagnostic::error()
                            .with_message("entry point function must be declared at the global scope")
                            .with_label(Label::primary(function.span, "not declared at global scope")));
                    }

                    self.cache.entry_point_function_id = Some(function.id);

                    Ok(())
                }
                _ => {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "the `{}` attribute cannot be used on extern functions",
                            AttrKind::Entry,
                        ))
                        .with_label(Label::primary(attr.span, "cannot be used on extern functions")))
                }
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
