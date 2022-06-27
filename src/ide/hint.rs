use super::types::*;
use crate::{
    ast,
    infer::{normalize::Normalize, ty_ctx::TyCtx},
    span::{EndPosition, Position, Span},
    types::Type,
    workspace::Workspace,
};

pub(super) struct HintSess<'a> {
    pub(super) workspace: &'a Workspace,
    pub(super) tycx: &'a TyCtx,
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

impl<'a> CollectHints<'a> for ast::Binding {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        if self.type_expr.is_none() {
            let ty = self.ty.normalize(sess.tycx);

            match ty {
                Type::Function(_) | Type::Module(_) | Type::Type(_) | Type::AnyType => (),
                _ => sess.push_hint(self.pattern.span(), ty.to_string(), HintKind::Binding),
            }
        }

        self.value.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for ast::Function {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match &self.kind {
            ast::FunctionKind::Orphan { sig, body } => {
                sig.collect_hints(sess);
                body.collect_hints(sess);
            }
            ast::FunctionKind::Extern { .. } => (),
            ast::FunctionKind::Intrinsic(_) => (),
        }
    }
}

impl<'a> CollectHints<'a> for ast::FunctionSig {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.params
            .iter()
            .filter(|param| param.type_expr.is_none())
            .for_each(|param| {
                let ty = param.ty.normalize(sess.tycx);
                let span = param.pattern.span();

                if span.is_unknown() {
                    // This parameter was inserted implicitly
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
                        format!("{}: {}", param.pattern, ty),
                        HintKind::ImplicitParam,
                    )
                } else {
                    sess.push_hint(param.pattern.span(), ty.to_string(), HintKind::Binding)
                }
            });

        if let Some(ret) = self.return_type.as_ref() {
            ret.collect_hints(sess);
        } else {
            let ret_ty = &self.ty.normalize(sess.tycx).into_function().return_type;
            match ret_ty.as_ref() {
                Type::Unit => (),
                _ => sess.push_hint(self.span, ret_ty.to_string(), HintKind::ReturnType),
            }
        }
    }
}

impl<'a> CollectHints<'a> for ast::Block {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.statements.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for ast::Ast {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match self {
            ast::Ast::Binding(binding) => binding.collect_hints(sess),
            ast::Ast::Assignment(assignment) => {
                assignment.lhs.collect_hints(sess);
                assignment.rhs.collect_hints(sess);
            }
            ast::Ast::Cast(t) => t.expr.collect_hints(sess),
            ast::Ast::Builtin(b) => match &b.kind {
                ast::BuiltinKind::Import(_) => (),
                ast::BuiltinKind::SizeOf(expr)
                | ast::BuiltinKind::AlignOf(expr)
                | ast::BuiltinKind::Run(expr) => expr.collect_hints(sess),
                ast::BuiltinKind::Panic(expr) => expr.collect_hints(sess),
            },
            ast::Ast::Function(f) => {
                f.sig.collect_hints(sess);
                f.body.collect_hints(sess);
            }
            ast::Ast::While(while_) => {
                while_.condition.collect_hints(sess);
                while_.block.collect_hints(sess);
            }
            ast::Ast::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(s, e) => {
                        s.collect_hints(sess);
                        e.collect_hints(sess);
                    }
                    ast::ForIter::Value(v) => {
                        v.collect_hints(sess);
                    }
                }
                for_.block.collect_hints(sess);
            }
            ast::Ast::Break(_) | ast::Ast::Continue(_) => (),
            ast::Ast::Return(ret) => {
                ret.expr.collect_hints(sess);
            }
            ast::Ast::If(if_) => {
                if_.condition.collect_hints(sess);
                if_.then.collect_hints(sess);
                if_.otherwise.collect_hints(sess);
            }
            ast::Ast::Block(block) => block.collect_hints(sess),
            ast::Ast::Binary(binary) => {
                binary.lhs.collect_hints(sess);
                binary.rhs.collect_hints(sess);
            }
            ast::Ast::Unary(unary) => {
                unary.value.collect_hints(sess);
            }
            ast::Ast::Subscript(sub) => {
                sub.expr.collect_hints(sess);
                sub.index.collect_hints(sess);
            }
            ast::Ast::Slice(slice) => {
                slice.expr.collect_hints(sess);
                slice.low.collect_hints(sess);
                slice.high.collect_hints(sess);
            }
            ast::Ast::Call(call) => {
                call.callee.collect_hints(sess);
                call.args.collect_hints(sess);
            }
            ast::Ast::MemberAccess(access) => access.expr.collect_hints(sess),
            ast::Ast::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(l) => l.collect_hints(sess),
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.collect_hints(sess);
                    expr.collect_hints(sess);
                }
            },
            ast::Ast::TupleLiteral(lit) => {
                lit.elements.collect_hints(sess);
            }
            ast::Ast::StructLiteral(lit) => {
                lit.type_expr.collect_hints(sess);
                for field in &lit.fields {
                    field.expr.collect_hints(sess);
                }
            }
            ast::Ast::PointerType(e) | ast::Ast::MultiPointerType(e) | ast::Ast::SliceType(e) => {
                e.inner.collect_hints(sess)
            }
            ast::Ast::ArrayType(at) => at.inner.collect_hints(sess),
            ast::Ast::StructType(s) => {
                for f in &s.fields {
                    f.ty.collect_hints(sess);
                }
            }
            ast::Ast::FunctionType(sig) => {
                sig.collect_hints(sess);
            }
            ast::Ast::Ident(_)
            | ast::Ast::Literal(_)
            | ast::Ast::SelfType(_)
            | ast::Ast::Const(_)
            | ast::Ast::Placeholder(_)
            | ast::Ast::Error(_) => (),
        }
    }
}
