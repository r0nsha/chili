use super::types::*;
use crate::{
    hir,
    infer::{normalize::Normalize, type_ctx::TypeCtx},
    span::{EndPosition, Position, Span},
    types::Type,
    workspace::{BindingInfoFlags, Workspace},
};

pub(super) struct HintSess<'a> {
    pub(super) workspace: &'a Workspace,
    pub(super) tcx: &'a TypeCtx,
    pub(super) hints: Vec<Hint>,
}

impl<'a> HintSess<'a> {
    fn push_hint(&mut self, span: Span, type_name: String, kind: HintKind) {
        if let Some(file) = self.workspace.diagnostics.get_file(span.file_id) {
            self.hints.push(Hint {
                span: IdeSpan::from_span_and_file(span, file.name()),
                type_name,
                kind: kind.to_string(),
            })
        }
    }
}

pub(super) trait CollectHints<'a> {
    fn collect_hints(&self, sess: &mut HintSess<'a>);
}

impl<'a, T: CollectHints<'a>> CollectHints<'a> for Vec<T> {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        for element in self {
            element.collect_hints(sess);
        }
    }
}

impl<'a, T: CollectHints<'a>> CollectHints<'a> for Option<T> {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        if let Some(e) = self {
            e.collect_hints(sess);
        }
    }
}

impl<'a, T: CollectHints<'a>> CollectHints<'a> for Box<T> {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.as_ref().collect_hints(sess)
    }
}

impl<'a> CollectHints<'a> for hir::Node {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match self {
            hir::Node::Const(x) => x.collect_hints(sess),
            hir::Node::Binding(x) => x.collect_hints(sess),
            hir::Node::Id(x) => x.collect_hints(sess),
            hir::Node::Assignment(x) => x.collect_hints(sess),
            hir::Node::MemberAccess(x) => x.collect_hints(sess),
            hir::Node::Call(x) => x.collect_hints(sess),
            hir::Node::Cast(x) => x.collect_hints(sess),
            hir::Node::Sequence(x) => x.collect_hints(sess),
            hir::Node::Control(x) => x.collect_hints(sess),
            hir::Node::Builtin(x) => x.collect_hints(sess),
            hir::Node::Literal(x) => x.collect_hints(sess),
        }
    }
}

impl<'a> CollectHints<'a> for hir::Binding {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        let binding_info = sess.workspace.binding_infos.get(self.id).unwrap();

        let should_show_hint = !binding_info.flags.contains(BindingInfoFlags::IGNORE)
            && binding_info
                .flags
                .contains(BindingInfoFlags::IS_USER_DEFINED | BindingInfoFlags::TYPE_WAS_INFERRED);

        if should_show_hint {
            match binding_info.ty.normalize(sess.tcx) {
                Type::Function(_) | Type::Module(_) | Type::Type(_) | Type::AnyType => (),
                ty => sess.push_hint(self.span, ty.to_string(), HintKind::Binding),
            }
        }

        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Function {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match &self.kind {
            hir::FunctionKind::Orphan {
                params,
                inferred_return_type_span,
                body,
            } => {
                for param in params.iter() {
                    let binding_info = sess.workspace.binding_infos.get(param.id).unwrap();
                    let ty = binding_info.ty.normalize(sess.tcx);

                    if binding_info.flags.contains(BindingInfoFlags::IGNORE) {
                        continue;
                    }

                    if binding_info
                        .flags
                        .contains(BindingInfoFlags::IS_USER_DEFINED | BindingInfoFlags::TYPE_WAS_INFERRED)
                    {
                        sess.push_hint(binding_info.span, ty.to_string(), HintKind::Binding)
                    } else if binding_info
                        .flags
                        .contains(BindingInfoFlags::IS_IMPLICIT_IT_FN_PARAMETER)
                    {
                        let index_after_fn_kw = self.span.start.index + 2;
                        let span_after_fn_kw = self
                            .span
                            .with_start(Position {
                                index: index_after_fn_kw,
                                line: self.span.start.line,
                                column: self.span.start.column,
                            })
                            .with_end(EndPosition {
                                index: index_after_fn_kw,
                            });

                        sess.push_hint(
                            span_after_fn_kw,
                            format!("{}: {}", binding_info.name, ty),
                            HintKind::ImplicitParam,
                        )
                    }
                }

                if let Some(span) = inferred_return_type_span {
                    match self.ty.normalize(sess.tcx).into_function().return_type.as_ref() {
                        Type::Unit => (),
                        return_type => sess.push_hint(*span, return_type.to_string(), HintKind::ReturnType),
                    }
                }

                body.collect_hints(sess);
            }
            hir::FunctionKind::Extern { .. } => (),
            hir::FunctionKind::Intrinsic(_) => (),
        }
    }
}

impl<'a> CollectHints<'a> for hir::Sequence {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.statements.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Const {
    fn collect_hints(&self, _sess: &mut HintSess<'a>) {}
}

impl<'a> CollectHints<'a> for hir::Id {
    fn collect_hints(&self, _sess: &mut HintSess<'a>) {}
}

impl<'a> CollectHints<'a> for hir::Assignment {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.lhs.collect_hints(sess);
        self.rhs.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::MemberAccess {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Call {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.callee.collect_hints(sess);
        self.args.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Cast {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Control {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match self {
            hir::Control::If(x) => x.collect_hints(sess),
            hir::Control::While(x) => x.collect_hints(sess),
            hir::Control::Return(x) => x.collect_hints(sess),
            hir::Control::Break(_) | hir::Control::Continue(_) => (),
        }
    }
}

impl<'a> CollectHints<'a> for hir::If {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.condition.collect_hints(sess);
        self.then.collect_hints(sess);
        self.otherwise.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::While {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.condition.collect_hints(sess);
        self.body.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Return {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Builtin {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match self {
            hir::Builtin::Add(x)
            | hir::Builtin::Sub(x)
            | hir::Builtin::Mul(x)
            | hir::Builtin::Div(x)
            | hir::Builtin::Rem(x)
            | hir::Builtin::Shl(x)
            | hir::Builtin::Shr(x)
            | hir::Builtin::And(x)
            | hir::Builtin::Or(x)
            | hir::Builtin::Lt(x)
            | hir::Builtin::Le(x)
            | hir::Builtin::Gt(x)
            | hir::Builtin::Ge(x)
            | hir::Builtin::Eq(x)
            | hir::Builtin::Ne(x)
            | hir::Builtin::BitAnd(x)
            | hir::Builtin::BitOr(x)
            | hir::Builtin::BitXor(x) => x.collect_hints(sess),
            hir::Builtin::Not(x) | hir::Builtin::Neg(x) | hir::Builtin::Deref(x) => x.collect_hints(sess),
            hir::Builtin::Ref(x) => x.collect_hints(sess),
            hir::Builtin::Offset(x) => x.collect_hints(sess),
            hir::Builtin::Slice(x) => x.collect_hints(sess),
        }
    }
}

impl<'a> CollectHints<'a> for hir::Binary {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.lhs.collect_hints(sess);
        self.rhs.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Unary {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Ref {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Offset {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
        self.index.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Slice {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.value.collect_hints(sess);
        self.low.collect_hints(sess);
        self.high.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for hir::Literal {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match self {
            hir::Literal::Struct(lit) => {
                for field in lit.fields.iter() {
                    field.value.collect_hints(sess);
                }
            }
            hir::Literal::Tuple(lit) => {
                for element in lit.elements.iter() {
                    element.collect_hints(sess);
                }
            }
            hir::Literal::Array(lit) => {
                for element in lit.elements.iter() {
                    element.collect_hints(sess);
                }
            }
            hir::Literal::ArrayFill(lit) => {
                lit.value.collect_hints(sess);
            }
        }
    }
}
