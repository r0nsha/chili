use super::{env::Env, Check, CheckResult, CheckSess};
use crate::{
    ast::{
        self,
        pattern::{NamePattern, Pattern},
    },
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
                        expected_type,
                        None,
                        node.ty(),
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
                        expected_type,
                        None,
                        unit,
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
            AttrKind::Intrinsic | AttrKind::Entry => self.tcx.common_types.unit,
            AttrKind::Lib => self.tcx.common_types.str,
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
                AttrKind::Entry => {
                    const USAGE: &str = "can only be used on immutable, let bound functions";

                    let err = || -> DiagnosticResult<()> { Err(invalid_attr_use(attr, USAGE)) };

                    match &binding.kind {
                        ast::BindingKind::Orphan {
                            pattern,
                            value,
                            is_static,
                            ..
                        } => {
                            if *is_static {
                                return err();
                            }

                            match pattern {
                                Pattern::Name(NamePattern { is_mutable: false, .. }) => (),
                                _ => return err(),
                            }

                            match value.as_ref() {
                                ast::Ast::Function(_) => (),
                                _ => return err(),
                            }
                        }
                        _ => return err(),
                    }
                }
                AttrKind::Lib => match &binding.kind {
                    ast::BindingKind::ExternFunction { .. } | ast::BindingKind::ExternVariable { .. } => (),
                    _ => {
                        return Err(invalid_attr_use(
                            attr,
                            "can only be used on extern variables and functions",
                        ))
                    }
                },
            }
        }

        Ok(())
    }
}
