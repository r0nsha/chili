mod ref_access;
mod type_limits;

use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir,
    infer::{normalize::Normalize, ty_ctx::TyCtx},
    span::Span,
    workspace::Workspace,
};

pub fn lint(workspace: &mut Workspace, tycx: &TyCtx, cache: &hir::Cache) {
    let mut sess = LintSess { workspace, tycx };
    cache.lint(&mut sess);

    // Check that an entry point function exists
    if workspace.build_options.need_entry_point_function() {
        if let Some(binding_info) = workspace.entry_point_function() {
            let ty = binding_info.ty.normalize(tycx).into_function();

            // if this is the main function, check its type matches a fn() -> [unit | never]
            if !(ty.return_type.is_unit() || ty.return_type.is_never())
                || !ty.params.is_empty()
                || ty.varargs.is_some()
            {
                workspace.diagnostics.push(
                    Diagnostic::error()
                        .with_message(format!(
                            "entry point function `main` has type `{}`, expected `fn() -> ()`",
                            ty
                        ))
                        .with_label(Label::primary(
                            binding_info.span,
                            "invalid type of entry point function",
                        )),
                );
            }
        } else {
            workspace.diagnostics.push(
                Diagnostic::error()
                    .with_message("entry point function `main` is not defined")
                    .with_label(Label::primary(
                        Span::initial(workspace.get_root_module_info().file_id),
                        "",
                    ))
                    .with_note("define function `let main = fn() {}` in your entry file"),
            );
        }
    }
}

pub struct LintSess<'s> {
    pub workspace: &'s mut Workspace,
    pub tycx: &'s TyCtx,
}

trait Lint {
    fn lint(&self, sess: &mut LintSess);
}

impl<T: Lint> Lint for Vec<T> {
    fn lint(&self, sess: &mut LintSess) {
        for element in self {
            element.lint(sess);
        }
    }
}

impl<T: Lint> Lint for Option<T> {
    fn lint(&self, sess: &mut LintSess) {
        if let Some(e) = self {
            e.lint(sess);
        }
    }
}

impl<T: Lint> Lint for Box<T> {
    fn lint(&self, sess: &mut LintSess) {
        self.as_ref().lint(sess)
    }
}

impl Lint for hir::Cache {
    fn lint(&self, sess: &mut LintSess) {
        for (_, binding) in self.bindings.iter() {
            binding.lint(sess);
        }

        for (_, function) in self.functions.iter() {
            function.lint(sess);
        }
    }
}

impl Lint for hir::Node {
    fn lint(&self, sess: &mut LintSess) {
        match self {
            hir::Node::Const(x) => x.lint(sess),
            hir::Node::Binding(x) => x.lint(sess),
            hir::Node::Id(x) => x.lint(sess),
            hir::Node::Assignment(x) => x.lint(sess),
            hir::Node::MemberAccess(x) => x.lint(sess),
            hir::Node::Call(x) => x.lint(sess),
            hir::Node::Cast(x) => x.lint(sess),
            hir::Node::Sequence(x) => x.lint(sess),
            hir::Node::Control(x) => x.lint(sess),
            hir::Node::Builtin(x) => x.lint(sess),
            hir::Node::Literal(x) => x.lint(sess),
        }
    }
}

impl Lint for hir::Binding {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);
    }
}

impl Lint for hir::Function {
    fn lint(&self, sess: &mut LintSess) {
        match &self.kind {
            hir::FunctionKind::Orphan { body, .. } => body.lint(sess),
            hir::FunctionKind::Extern { .. } | hir::FunctionKind::Intrinsic(..) => (),
        }
    }
}

impl Lint for hir::Sequence {
    fn lint(&self, sess: &mut LintSess) {
        self.statements.lint(sess);
    }
}

impl Lint for hir::Const {
    fn lint(&self, sess: &mut LintSess) {
        sess.check_type_limits(self);
    }
}

impl Lint for hir::Id {
    fn lint(&self, _sess: &mut LintSess) {}
}

impl Lint for hir::Assignment {
    fn lint(&self, sess: &mut LintSess) {
        self.lhs.lint(sess);
    }
}

impl Lint for hir::MemberAccess {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);
    }
}

impl Lint for hir::Call {
    fn lint(&self, sess: &mut LintSess) {
        self.callee.lint(sess);
        self.args.lint(sess);
    }
}

impl Lint for hir::Cast {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);
    }
}

impl Lint for hir::Control {
    fn lint(&self, sess: &mut LintSess) {
        match self {
            hir::Control::If(if_) => {
                if_.condition.lint(sess);
                if_.then.lint(sess);
                if_.otherwise.lint(sess);
            }
            hir::Control::While(while_) => {
                while_.condition.lint(sess);
                while_.body.lint(sess);
            }
            hir::Control::Return(return_) => return_.value.lint(sess),
            hir::Control::Break(_) | hir::Control::Continue(_) => (),
        }
    }
}

impl Lint for hir::Builtin {
    fn lint(&self, sess: &mut LintSess) {
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
            | hir::Builtin::BitXor(x) => x.lint(sess),
            hir::Builtin::Not(x) | hir::Builtin::Neg(x) | hir::Builtin::Deref(x) => x.lint(sess),
            hir::Builtin::Ref(x) => x.lint(sess),
            hir::Builtin::Offset(x) => x.lint(sess),
            hir::Builtin::Slice(x) => x.lint(sess),
        }
    }
}

impl Lint for hir::Binary {
    fn lint(&self, sess: &mut LintSess) {
        self.lhs.lint(sess);
        self.rhs.lint(sess);
    }
}

impl Lint for hir::Unary {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);
    }
}

impl Lint for hir::Ref {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);

        if self.is_mutable {
            sess.check_node_can_be_mutably_referenced(&self.value);
        }
    }
}

impl Lint for hir::Offset {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);
        self.index.lint(sess);
    }
}

impl Lint for hir::Slice {
    fn lint(&self, sess: &mut LintSess) {
        self.value.lint(sess);
        self.low.lint(sess);
        self.high.lint(sess);
    }
}

impl Lint for hir::Literal {
    fn lint(&self, sess: &mut LintSess) {
        match self {
            hir::Literal::Struct(lit) => {
                for field in lit.fields.iter() {
                    field.value.lint(sess);
                }
            }
            hir::Literal::Tuple(lit) => {
                for element in lit.elements.iter() {
                    element.lint(sess);
                }
            }
            hir::Literal::Array(lit) => {
                for element in lit.elements.iter() {
                    element.lint(sess);
                }
            }
            hir::Literal::ArrayFill(lit) => lit.value.lint(sess),
        }
    }
}
