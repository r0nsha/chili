use super::{env::Env, Check, CheckResult, CheckSess};
use crate::{
    ast,
    error::diagnostic::{Diagnostic, Label},
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

                    node.ty()
                        .unify(&expected_type, &mut self.tcx)
                        .or_report_err(&self.tcx, expected_type, None, node.ty(), node.span())?;

                    let node_span = node.span();

                    node.into_const_value().ok_or_else(|| {
                        Diagnostic::error()
                            .with_message("attribute value must be compile-time known")
                            .with_label(Label::primary(
                                node_span,
                                "value is not compile-time known",
                            ))
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
        }
    }
}
