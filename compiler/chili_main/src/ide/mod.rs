mod types;
mod util;

use chili_ast::{
    ast::{self, TypedAst},
    ty::TyKind,
    workspace::Workspace,
};
use chili_error::diagnostic::DiagnosticSeverity;
use chili_infer::{normalize::Normalize, ty_ctx::TyCtx};
use chili_span::{EndPosition, Position, Span};
use types::*;
use util::*;

pub(crate) fn diagnostics(
    workspace: &Workspace,
    tycx: Option<&TyCtx>,
    typed_ast: Option<&TypedAst>,
) {
    let mut objects: Vec<IdeObject> = vec![];

    objects.extend(
        workspace
            .diagnostics
            .items()
            .iter()
            .filter(|diag| !diag.labels.is_empty())
            .map(|diag| {
                let label = diag.labels.first().unwrap();
                let file = workspace.diagnostics.get_file(label.span.file_id).unwrap();

                IdeObject::Diagnostic(IdeDiagnostic {
                    severity: match &diag.severity {
                        DiagnosticSeverity::Error => IdeDiagnosticSeverity::Error,
                    },
                    span: IdeSpan::from_span_and_file(label.span, file.name()),
                    message: diag.message.clone().unwrap(),
                })
            }),
    );

    match (tycx, typed_ast) {
        (Some(tycx), Some(typed_ast)) => {
            let mut sess = HintSess {
                workspace,
                tycx,
                hints: vec![],
            };

            typed_ast
                .bindings
                .iter()
                .for_each(|binding| binding.collect_hints(&mut sess));

            objects.extend(sess.hints.into_iter().map(IdeObject::Hint));
        }
        _ => (),
    }

    write(&objects);
}

pub(crate) fn hover_info(workspace: &Workspace, tycx: Option<&TyCtx>, offset: usize) {
    if let Some(tycx) = tycx {
        let searched_binding_info =
            workspace
                .binding_infos
                .iter()
                .map(|(_, b)| b)
                .find(|binding_info| {
                    binding_info.module_id == workspace.root_module_id
                        && binding_info.span.contains(offset)
                });

        if let Some(binding_info) = searched_binding_info {
            write(&HoverInfo {
                contents: binding_info.ty.normalize(tycx).to_string(),
            });
        }
    } else {
        write_null();
    }
}

pub(crate) fn goto_definition(workspace: &Workspace, tycx: Option<&TyCtx>, offset: usize) {
    for (_, binding_info) in workspace.binding_infos.iter() {
        if is_offset_in_span_and_root_module(workspace, offset, binding_info.span) {
            if let Some(tycx) = tycx {
                match binding_info.ty.normalize(tycx) {
                    TyKind::Module(module_id) => {
                        let module_info = workspace.get_module_info(module_id).unwrap();

                        let span = Span {
                            file_id: module_info.file_id,
                            start: Position::initial(),
                            end: EndPosition::initial(),
                        };

                        write(&IdeSpan::from_span_and_file(
                            span,
                            module_info.file_path.to_string(),
                        ));

                        return;
                    }
                    _ => (),
                }
            }

            write(&IdeSpan::from_span_and_file(
                binding_info.span,
                workspace
                    .get_module_info(binding_info.module_id)
                    .unwrap()
                    .file_path
                    .to_string(),
            ));

            return;
        }

        for &use_span in binding_info.uses.iter() {
            if is_offset_in_span_and_root_module(workspace, offset, use_span) {
                write(&IdeSpan::from_span_and_file(
                    binding_info.span,
                    workspace
                        .get_module_info(binding_info.module_id)
                        .unwrap()
                        .file_path
                        .to_string(),
                ));

                return;
            }
        }
    }

    write_null();
}

struct HintSess<'a> {
    workspace: &'a Workspace,
    tycx: &'a TyCtx,
    hints: Vec<Hint>,
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

trait CollectHints<'a> {
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
        if self.ty_expr.is_none() {
            let ty = self.ty.normalize(sess.tycx);

            match ty {
                TyKind::Function(_) | TyKind::Module(_) | TyKind::Type(_) | TyKind::AnyType => (),
                _ => sess.push_hint(self.pattern.span(), ty.to_string(), HintKind::Binding),
            }
        }

        self.expr.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for ast::FunctionSig {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.params
            .iter()
            .filter(|param| param.ty_expr.is_none())
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

        if let Some(ret) = self.ret.as_ref() {
            ret.collect_hints(sess);
        } else {
            let ret_ty = &self.ty.normalize(sess.tycx).into_fn().ret;
            match ret_ty.as_ref() {
                TyKind::Unit => (),
                _ => sess.push_hint(self.span, ret_ty.to_string(), HintKind::ReturnType),
            }
        }
    }
}

impl<'a> CollectHints<'a> for ast::Block {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        self.exprs.collect_hints(sess);
        self.deferred.collect_hints(sess);
    }
}

impl<'a> CollectHints<'a> for ast::Expr {
    fn collect_hints(&self, sess: &mut HintSess<'a>) {
        match &self.kind {
            ast::ExprKind::Binding(binding) => binding.collect_hints(sess),
            ast::ExprKind::Defer(defer) => defer.expr.collect_hints(sess),
            ast::ExprKind::Assign(assign) => {
                assign.lvalue.collect_hints(sess);
                assign.rvalue.collect_hints(sess);
            }
            ast::ExprKind::Cast(t) => t.expr.collect_hints(sess),
            ast::ExprKind::Builtin(b) => match b {
                ast::BuiltinKind::Import(_) | ast::BuiltinKind::LangItem(_) => (),
                ast::BuiltinKind::SizeOf(e)
                | ast::BuiltinKind::AlignOf(e)
                | ast::BuiltinKind::Run(e, _) => e.collect_hints(sess),
                ast::BuiltinKind::Panic(e) => e.collect_hints(sess),
            },
            ast::ExprKind::Function(f) => {
                f.sig.collect_hints(sess);
                f.body.collect_hints(sess);
            }
            ast::ExprKind::While(while_) => {
                while_.cond.collect_hints(sess);
                while_.block.collect_hints(sess);
            }
            ast::ExprKind::For(for_) => {
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
            ast::ExprKind::Break(term) | ast::ExprKind::Continue(term) => {
                term.deferred.collect_hints(sess);
            }
            ast::ExprKind::Return(ret) => {
                ret.expr.collect_hints(sess);
                ret.deferred.collect_hints(sess);
            }
            ast::ExprKind::If(if_) => {
                if_.cond.collect_hints(sess);
                if_.then.collect_hints(sess);
                if_.otherwise.collect_hints(sess);
            }
            ast::ExprKind::Block(block) => block.collect_hints(sess),
            ast::ExprKind::Binary(binary) => {
                binary.lhs.collect_hints(sess);
                binary.rhs.collect_hints(sess);
            }
            ast::ExprKind::Unary(unary) => {
                unary.lhs.collect_hints(sess);
            }
            ast::ExprKind::Subscript(sub) => {
                sub.expr.collect_hints(sess);
                sub.index.collect_hints(sess);
            }
            ast::ExprKind::Slice(slice) => {
                slice.expr.collect_hints(sess);
                slice.low.collect_hints(sess);
                slice.high.collect_hints(sess);
            }
            ast::ExprKind::Call(call) => {
                call.callee.collect_hints(sess);
                call.args.collect_hints(sess);
            }
            ast::ExprKind::MemberAccess(access) => access.expr.collect_hints(sess),
            ast::ExprKind::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(l) => l.collect_hints(sess),
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.collect_hints(sess);
                    expr.collect_hints(sess);
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                lit.elements.collect_hints(sess);
            }
            ast::ExprKind::StructLiteral(lit) => {
                lit.type_expr.collect_hints(sess);
                for field in &lit.fields {
                    field.expr.collect_hints(sess);
                }
            }
            ast::ExprKind::PointerType(e)
            | ast::ExprKind::MultiPointerType(e)
            | ast::ExprKind::SliceType(e) => e.inner.collect_hints(sess),
            ast::ExprKind::ArrayType(at) => at.inner.collect_hints(sess),
            ast::ExprKind::StructType(s) => {
                for f in &s.fields {
                    f.ty.collect_hints(sess);
                }
            }
            ast::ExprKind::FunctionType(sig) => {
                sig.collect_hints(sess);
            }
            ast::ExprKind::Ident(_)
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::ConstValue(_)
            | ast::ExprKind::Placeholder
            | ast::ExprKind::Error => (),
        }
    }
}
