use super::{env::Env, CheckResult, CheckSess};
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir::{self, const_value::ConstValue},
    span::Span,
    types::TypeId,
};

pub(super) fn can_dispatch_intrinsic_at_comptime(sess: &mut CheckSess, callee: &hir::Node) -> Option<hir::Intrinsic> {
    if let Some(ConstValue::Function(f)) = callee.as_const_value() {
        let function = sess.cache.functions.get(f.id).unwrap();
        match &function.kind {
            hir::FunctionKind::Intrinsic(intrinsic) => match intrinsic {
                hir::Intrinsic::Location
                | hir::Intrinsic::CallerLocation
                | hir::Intrinsic::CompilerError
                | hir::Intrinsic::CompilerWarning => Some(*intrinsic),
                hir::Intrinsic::StartWorkspace | hir::Intrinsic::Os | hir::Intrinsic::Arch => None,
            },
            _ => None,
        }
    } else {
        None
    }
}

pub(super) fn dispatch_intrinsic(
    sess: &mut CheckSess,
    env: &mut Env,
    intrinsic: &hir::Intrinsic,
    args: &[hir::Node],
    ty: TypeId,
    span: Span,
) -> CheckResult {
    match intrinsic {
        hir::Intrinsic::Location => {
            let value = sess.build_location_value(env, span)?;

            Ok(hir::Node::Const(hir::Const { value, ty, span }))
        }
        hir::Intrinsic::CallerLocation => sess
            .get_track_caller_location_param_id(env, span)
            .map(|id| hir::Node::Id(hir::Id { id, ty, span })),
        hir::Intrinsic::CompilerError => {
            let first_arg = args.first().unwrap();

            if let Some(ConstValue::Str(msg)) = args.first().unwrap().as_const_value() {
                Err(Diagnostic::error()
                    .with_message(msg)
                    .with_label(Label::primary(span, msg)))
            } else {
                Err(Diagnostic::error()
                    .with_message("argument `msg` must be a string literal")
                    .with_label(Label::primary(first_arg.span(), "not a string literal")))
            }
        }
        hir::Intrinsic::CompilerWarning => {
            let first_arg = args.first().unwrap();

            if let Some(ConstValue::Str(msg)) = args.first().unwrap().as_const_value() {
                sess.workspace.diagnostics.push(
                    Diagnostic::warning()
                        .with_message(msg)
                        .with_label(Label::primary(span, msg)),
                );

                Ok(hir::Node::Const(hir::Const {
                    value: ConstValue::Unit(()),
                    ty: sess.tcx.common_types.unit,
                    span,
                }))
            } else {
                Err(Diagnostic::error()
                    .with_message("argument `msg` must be a string literal")
                    .with_label(Label::primary(first_arg.span(), "not a string literal")))
            }
        }
        hir::Intrinsic::StartWorkspace | hir::Intrinsic::Os | hir::Intrinsic::Arch => unreachable!(),
    }
}
