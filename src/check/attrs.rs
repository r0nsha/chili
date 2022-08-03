use super::{env::Env, Check, CheckResult, CheckSess};
use crate::{
    ast,
    common::path::RelativeTo,
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult,
    },
    hir::{
        attrs::{Attr, AttrKind, Attrs},
        const_value::ConstValue,
    },
    infer::{display::OrReportErr, unify::UnifyType},
    types::TypeId,
};

impl<'s> CheckSess<'s> {
    pub(super) fn check_attrs(&mut self, attrs: &[ast::Attr], env: &mut Env) -> CheckResult<Attrs> {
        let mut new_attrs = Attrs::new();

        for attr in attrs.iter() {
            let span = attr.span;
            let kind = AttrKind::try_from(attr.name.name.as_str()).map_err(|_| {
                Diagnostic::error()
                    .with_message(format!("unknown attribute `{}`", attr.name.name))
                    .with_label(Label::primary(attr.name.span, "unknown attribute"))
            })?;

            let expected_type = self.get_attr_expected_type(kind);

            let value = match &attr.value {
                Some(value) => {
                    let node = value.check(self, env, Some(expected_type))?;

                    node.ty().unify(&expected_type, &mut self.tcx).or_report_err(
                        &self.tcx,
                        &expected_type,
                        None,
                        &node.ty(),
                        node.span(),
                    )?;

                    let node_span = node.span();

                    node.into_const_value().ok_or_else(|| {
                        Diagnostic::error()
                            .with_message("attribute value must be compile-time known")
                            .with_label(Label::primary(node_span, "value is not compile-time known"))
                    })?
                }
                None => {
                    let unit = self.tcx.common_types.unit;

                    unit.unify(&expected_type, &mut self.tcx).or_report_err(
                        &self.tcx,
                        &expected_type,
                        None,
                        &unit,
                        span,
                    )?;

                    ConstValue::Unit(())
                }
            };

            let attr = Attr { kind, value, span };

            if let Some(used_attr) = new_attrs.insert(attr) {
                return Err(Diagnostic::error()
                    .with_message(format!("attribte `{}` has already been used", kind))
                    .with_label(Label::primary(span, "duplicate attribute"))
                    .with_label(Label::secondary(used_attr.span, "already used here")));
            }
        }

        Ok(new_attrs)
    }

    fn get_attr_expected_type(&self, kind: AttrKind) -> TypeId {
        match kind {
            AttrKind::Intrinsic | AttrKind::TrackCaller => self.tcx.common_types.unit,
            AttrKind::Lib | AttrKind::Dylib | AttrKind::LinkName => self.tcx.common_types.str_pointer,
        }
    }

    pub(super) fn check_attrs_are_assigned_to_valid_binding(
        &self,
        attrs: &Attrs,
        binding: &ast::Binding,
    ) -> DiagnosticResult<()> {
        fn invalid_attr_use(attr: &Attr, usage: &'static str) -> Diagnostic {
            Diagnostic::error()
                .with_message(format!("the `{}` attribute {}", attr.kind, usage))
                .with_label(Label::primary(attr.span, "invalid attribute use"))
        }

        for (_, attr) in attrs.iter() {
            match attr.kind {
                AttrKind::Intrinsic => match &binding.kind {
                    ast::BindingKind::ExternFunction { .. } => (),
                    _ => return Err(invalid_attr_use(attr, "can only be used on extern functions")),
                },
                AttrKind::Lib | AttrKind::Dylib | AttrKind::LinkName => match &binding.kind {
                    ast::BindingKind::ExternFunction { .. } | ast::BindingKind::ExternVariable { .. } => (),
                    _ => {
                        return Err(invalid_attr_use(
                            attr,
                            "can only be used on extern variables and extern functions",
                        ))
                    }
                },
                AttrKind::TrackCaller => match &binding.kind {
                    ast::BindingKind::Function { .. } => (),
                    _ => return Err(invalid_attr_use(attr, "can only be used on functions")),
                },
            }
        }

        Ok(())
    }

    pub(super) fn maybe_get_extern_lib_attr(
        &self,
        env: &Env,
        attrs: &Attrs,
        kind: AttrKind,
    ) -> DiagnosticResult<Option<ast::ExternLibrary>> {
        if let Some(attr) = attrs.get(kind) {
            let value = attr.value.as_str().unwrap().as_str();

            let lib = ast::ExternLibrary::try_from_str(value, &RelativeTo::Path(env.module_info().dir()), attr.span)?;

            Ok(Some(lib))
        } else {
            Ok(None)
        }
    }
}
