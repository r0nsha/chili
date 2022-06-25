mod access;
mod lvalue_access;
mod ref_access;
mod sess;
mod type_limits;

use crate::ast::{
    self,
    pattern::{HybridPattern, Pattern},
    workspace::Workspace,
    BindingKind,
};
use crate::common::scopes::Scopes;
use crate::error::diagnostic::{Diagnostic, Label};
use crate::infer::{normalize::Normalize, ty_ctx::TyCtx};
use crate::span::Span;
use sess::{InitState, LintSess};

pub fn lint(workspace: &mut Workspace, tycx: &TyCtx, typed_ast: &ast::TypedAst) {
    let mut sess = LintSess {
        workspace,
        tycx,
        init_scopes: Scopes::default(),
    };

    sess.init_scopes.push_scope();

    for (_, binding) in typed_ast.bindings.iter() {
        binding.lint(&mut sess);
    }

    sess.init_scopes.pop_scope();

    // Check that an entry point function exists
    if workspace.build_options.need_entry_point_function() {
        if let Some(binding_info) = workspace.entry_point_function() {
            let ty = binding_info.ty.normalize(tycx).into_fn();

            // if this is the main function, check its type matches a fn() -> [unit | never]
            if !(ty.ret.is_unit() || ty.ret.is_never())
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

impl Lint for ast::Module {
    fn lint(&self, sess: &mut LintSess) {
        for binding in self.bindings.iter() {
            binding.lint(sess);
        }
    }
}

impl Lint for ast::Binding {
    fn lint(&self, sess: &mut LintSess) {
        let init_state = if self.value.is_some()
            || matches!(
                self.kind,
                BindingKind::Extern(_) | BindingKind::Intrinsic(_)
            ) {
            InitState::Init
        } else {
            InitState::NotInit
        };

        for symbol in self.pattern.iter() {
            sess.init_scopes.insert(symbol.id, init_state);
        }

        self.value.lint(sess);

        if let Some(expr) = &self.value {
            let is_a_type = expr.ty().normalize(sess.tycx).is_type();

            // * don't allow types to be bounded to mutable bindings
            if is_a_type {
                match &self.pattern {
                    Pattern::Symbol(symbol) | Pattern::Hybrid(HybridPattern { symbol, .. }) => {
                        if symbol.is_mutable {
                            sess.workspace.diagnostics.push(
                                Diagnostic::error()
                                    .with_message("variable of type `type` must be immutable")
                                    .with_label(Label::primary(symbol.span, "variable is mutable"))
                                    .with_note("try removing the `mut` from the declaration"),
                            );
                        }
                    }
                    Pattern::StructUnpack(_) | Pattern::TupleUnpack(_) => (),
                }
            }
        }
    }
}

impl Lint for ast::Block {
    fn lint(&self, sess: &mut LintSess) {
        sess.init_scopes.push_scope();
        self.statements.lint(sess);
        sess.init_scopes.pop_scope();
    }
}

impl Lint for ast::Ast {
    fn lint(&self, sess: &mut LintSess) {
        match self {
            ast::Ast::Binding(binding) => binding.lint(sess),
            ast::Ast::Assignment(assignment) => {
                assignment.rvalue.lint(sess);

                match assignment.lvalue.as_ref() {
                    ast::Ast::Ident(ident) => {
                        sess.check_assign_lvalue_id_access(&assignment.lvalue, ident.binding_id);
                    }
                    _ => {
                        sess.check_lvalue_access(&assignment.lvalue, assignment.lvalue.span());
                        assignment.lvalue.lint(sess);
                    }
                };
            }
            ast::Ast::Cast(t) => t.expr.lint(sess),
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::Import(_) => (),
                ast::BuiltinKind::SizeOf(expr)
                | ast::BuiltinKind::AlignOf(expr)
                | ast::BuiltinKind::Run(expr, _) => expr.lint(sess),
                ast::BuiltinKind::Panic(e) => e.lint(sess),
            },
            ast::Ast::Function(f) => {
                f.body.lint(sess);
            }
            ast::Ast::While(while_) => {
                while_.condition.lint(sess);
                while_.block.lint(sess);
            }
            ast::Ast::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(s, e) => {
                        s.lint(sess);
                        e.lint(sess);
                    }
                    ast::ForIter::Value(v) => {
                        v.lint(sess);
                    }
                }
                for_.block.lint(sess);
            }
            ast::Ast::Break(_) | ast::Ast::Continue(_) => (),
            ast::Ast::Return(ret) => {
                ret.expr.lint(sess);
            }
            ast::Ast::If(if_) => {
                if_.condition.lint(sess);
                if_.then.lint(sess);
                if_.otherwise.lint(sess);
            }
            ast::Ast::Block(block) => block.lint(sess),
            ast::Ast::Binary(binary) => {
                binary.lhs.lint(sess);
                binary.rhs.lint(sess);
            }
            ast::Ast::Unary(unary) => {
                unary.value.lint(sess);

                if let ast::UnaryOp::Ref(is_mutable_ref) = &unary.op {
                    if *is_mutable_ref {
                        sess.check_expr_can_be_mutably_referenced(&unary.value);
                    }
                }
            }
            ast::Ast::Subscript(sub) => {
                sub.expr.lint(sess);
                sub.index.lint(sess);
            }
            ast::Ast::Slice(slice) => {
                slice.expr.lint(sess);
                slice.low.lint(sess);
                slice.high.lint(sess);

                if slice.high.is_none() {
                    if slice.expr.ty().normalize(sess.tycx).is_multi_pointer() {
                        sess.workspace.diagnostics.push(Diagnostic::error()
                        .with_message(
                            "multi pointer has unknown length, so you must specify the ending index",
                        )
                        .with_label(Label::primary(
                            slice.expr.span(),"multi pointer has unknown length"
                        )));
                    }
                }
            }
            ast::Ast::Call(call) => {
                call.callee.lint(sess);
                call.args.lint(sess);
            }
            ast::Ast::MemberAccess(access) => access.expr.lint(sess),
            ast::Ast::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(l) => l.lint(sess),
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.lint(sess);
                    expr.lint(sess);
                }
            },
            ast::Ast::TupleLiteral(lit) => {
                lit.elements.lint(sess);
            }
            ast::Ast::StructLiteral(lit) => {
                lit.type_expr.lint(sess);
                for field in &lit.fields {
                    field.expr.lint(sess);
                }
            }
            ast::Ast::PointerType(e) | ast::Ast::MultiPointerType(e) | ast::Ast::SliceType(e) => {
                e.inner.lint(sess)
            }
            ast::Ast::ArrayType(at) => at.inner.lint(sess),
            ast::Ast::StructType(s) => {
                for f in &s.fields {
                    f.ty.lint(sess);
                }
            }
            ast::Ast::FunctionType(sig) => {
                for p in &sig.params {
                    p.ty_expr.lint(sess);
                }
                sig.ret.lint(sess);
            }
            ast::Ast::Ident(ident) => sess.check_id_access(ident.binding_id, self.span()),
            ast::Ast::Literal(_) => {
                panic!("Literal expression should have been lowered to a ConstValue")
            }
            ast::Ast::SelfType(_) | ast::Ast::Const(_) | ast::Ast::Placeholder(_) => (),
            ast::Ast::Error(_) => panic!("unexpected error node"),
        }

        sess.check_type_limits(self);
    }
}
