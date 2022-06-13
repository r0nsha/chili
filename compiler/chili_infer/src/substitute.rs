use core::panic;
use std::collections::{HashMap, HashSet};

use chili_ast::{ast, ty::*};
use chili_error::{
    diagnostic::{Diagnostic, Label},
    Diagnostics,
};
use chili_span::Span;

use crate::{
    normalize::{Concrete, Normalize},
    ty_ctx::TyCtx,
};

pub fn substitute<'a>(
    diagnostics: &'a mut Diagnostics,
    tycx: &'a mut TyCtx,
    typed_ast: &'a ast::TypedAst,
) {
    let mut sess = Sess {
        diagnostics,
        tycx,
        erroneous_types: HashMap::new(),
        used_types: HashSet::new(),
    };

    // substitute used types - extracting erroneous types
    typed_ast.substitute(&mut sess);

    if sess.diagnostics.has_errors() {
        sess.emit_erroneous_types();
        return;
    }

    sess.concretize_types();
}

struct Sess<'a> {
    diagnostics: &'a mut Diagnostics,
    tycx: &'a mut TyCtx,

    // map of Ty -> Set of reduced expression spans that couldn't be inferred because of the key ty
    erroneous_types: HashMap<Ty, Vec<Span>>,

    used_types: HashSet<Ty>,
}

impl<'a> Sess<'a> {
    fn emit_erroneous_types(&mut self) {
        let diagnostics: Vec<Diagnostic> = self
            .erroneous_types
            .iter()
            .flat_map(|(&ty, spans)| {
                let ty_span = self.tycx.ty_span(ty);

                let ty_origin_label = ty_span.map(|span| {
                    Label::secondary(span, "because its type originates from this expression")
                });

                spans
                    .iter()
                    .filter(|&&span| ty_span.map_or(true, |ty_span| span != ty_span))
                    .map(|&span| {
                        Diagnostic::error()
                            .with_message("can't infer the expression's type")
                            .with_label(Label::primary(span, "can't infer type"))
                            .maybe_with_label(ty_origin_label.clone())
                            .with_note("try adding more type information")
                    })
                    .collect::<Vec<Diagnostic>>()
            })
            .collect();

        self.diagnostics.extend(diagnostics);
    }

    fn concretize_types(&mut self) {
        for ty in self.used_types.iter() {
            let concrete_type = ty.concrete(&self.tycx);
            self.tycx.bind_ty(*ty, concrete_type);
        }
    }
}

trait Substitute<'a> {
    fn substitute(&self, sess: &mut Sess<'a>);
}

impl<'a, T: Substitute<'a>> Substitute<'a> for Vec<T> {
    fn substitute(&self, sess: &mut Sess<'a>) {
        for element in self {
            element.substitute(sess);
        }
    }
}

impl<'a, T: Substitute<'a>> Substitute<'a> for Option<T> {
    fn substitute(&self, sess: &mut Sess<'a>) {
        if let Some(e) = self {
            e.substitute(sess);
        }
    }
}

impl<'a, T: Substitute<'a>> Substitute<'a> for Box<T> {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.as_ref().substitute(sess)
    }
}

impl<'a> Substitute<'a> for ast::TypedAst {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.bindings.iter().for_each(|b| b.substitute(sess));
    }
}

impl<'a> Substitute<'a> for ast::Binding {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty_expr.substitute(sess);
        self.expr.substitute(sess);
    }
}

impl<'a> Substitute<'a> for ast::Block {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.exprs.substitute(sess);
        self.deferred.substitute(sess);
    }
}

impl<'a> Substitute<'a> for ast::Function {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.sig.substitute(sess);
        self.body.substitute(sess);
    }
}

impl<'a> Substitute<'a> for ast::FunctionSig {
    fn substitute(&self, sess: &mut Sess<'a>) {
        for param in self.params.iter() {
            param.ty_expr.substitute(sess);
            param.ty.substitute(sess, param.pattern.span());
        }
        self.ret.substitute(sess);
        self.ty.substitute(sess, self.span);
    }
}

impl<'a> Substitute<'a> for ast::Cast {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.expr.substitute(sess);
        self.ty_expr.substitute(sess);
        self.target_ty.substitute(
            sess,
            self.ty_expr.as_ref().map_or(self.expr.span, |e| e.span),
        );
    }
}

impl<'a> Substitute<'a> for ast::Expr {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);

        match &self.kind {
            ast::ExprKind::Defer(_) => (),
            ast::ExprKind::Binding(binding) => binding.substitute(sess),
            ast::ExprKind::Assign(assign) => {
                assign.lvalue.substitute(sess);
                assign.rvalue.substitute(sess);
            }
            ast::ExprKind::Cast(info) => info.substitute(sess),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::BuiltinKind::SizeOf(expr)
                | ast::BuiltinKind::AlignOf(expr)
                | ast::BuiltinKind::Run(expr, _) => expr.substitute(sess),
                ast::BuiltinKind::Panic(expr) => expr.substitute(sess),
                ast::BuiltinKind::Import(_) | ast::BuiltinKind::LangItem(_) => (),
            },
            ast::ExprKind::Function(func) => func.substitute(sess),
            ast::ExprKind::While(while_) => {
                while_.cond.substitute(sess);
                while_.block.substitute(sess);
            }
            ast::ExprKind::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        start.substitute(sess);
                        end.substitute(sess);
                    }
                    ast::ForIter::Value(value) => {
                        value.substitute(sess);
                    }
                }

                for_.block.substitute(sess);
            }
            ast::ExprKind::Break(term) | ast::ExprKind::Continue(term) => {
                term.deferred.substitute(sess)
            }
            ast::ExprKind::Return(ret) => {
                ret.deferred.substitute(sess);
                ret.expr.substitute(sess);
            }
            ast::ExprKind::If(if_) => {
                if_.cond.substitute(sess);
                if_.then.substitute(sess);
                if_.otherwise.substitute(sess);
            }
            ast::ExprKind::Block(block) => {
                block.exprs.substitute(sess);
                block.deferred.substitute(sess);
            }
            ast::ExprKind::Binary(binary) => {
                binary.lhs.substitute(sess);
                binary.rhs.substitute(sess);
            }
            ast::ExprKind::Unary(unary) => unary.lhs.substitute(sess),
            ast::ExprKind::Subscript(sub) => {
                sub.expr.substitute(sess);
                sub.index.substitute(sess);
            }
            ast::ExprKind::Slice(slice) => {
                slice.expr.substitute(sess);
                slice.low.substitute(sess);
                slice.high.substitute(sess);
            }
            ast::ExprKind::Call(call) => {
                call.callee.substitute(sess);
                call.args.substitute(sess);
            }
            ast::ExprKind::MemberAccess(access) => access.expr.substitute(sess),
            ast::ExprKind::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(elements) => elements.substitute(sess),
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    len.substitute(sess);
                    expr.substitute(sess);
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                println!("{}", sess.tycx.ty_kind(self.ty));
                lit.elements.substitute(sess)
            }
            ast::ExprKind::StructLiteral(lit) => {
                lit.type_expr.substitute(sess);
                for f in lit.fields.iter() {
                    f.expr.substitute(sess);
                }
            }
            ast::ExprKind::PointerType(expr)
            | ast::ExprKind::MultiPointerType(expr)
            | ast::ExprKind::SliceType(expr) => expr.inner.substitute(sess),
            ast::ExprKind::ArrayType(at) => at.inner.substitute(sess),
            ast::ExprKind::StructType(struct_type, ..) => {
                for f in struct_type.fields.iter() {
                    f.ty.substitute(sess);
                }
            }
            ast::ExprKind::FunctionType(sig) => sig.substitute(sess),
            ast::ExprKind::Ident(_)
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::ConstValue(_)
            | ast::ExprKind::Placeholder => (),
            ast::ExprKind::Error => panic!("unexpected error node"),
        }
    }
}

trait SubstituteTy<'a> {
    fn substitute(&self, sess: &mut Sess<'a>, span: Span);
}

impl<'a> SubstituteTy<'a> for Ty {
    fn substitute(&self, sess: &mut Sess<'a>, span: Span) {
        sess.used_types.insert(*self);

        let ty = self.normalize(sess.tycx);

        // Check if any type variables are left after normalization
        // (normalization = reducing the type variable to its concrete type, recursively)
        let mut free_types: HashSet<Ty> = Default::default();
        extract_free_type_vars(&ty, &mut free_types);

        if free_types.is_empty() {
            sess.tycx.bind_ty(*self, ty);
        } else {
            for &ty in free_types.iter() {
                let span_set = sess.erroneous_types.entry(ty).or_default();

                if let Some(index) = span_set.iter().position(|s| {
                    s.start.index <= span.start.index && s.end.index >= span.end.index
                }) {
                    span_set.remove(index);
                }

                span_set.push(span);
            }
        }
    }
}

fn extract_free_type_vars(ty: &TyKind, free_types: &mut HashSet<Ty>) {
    match ty {
        TyKind::Var(var) => {
            free_types.insert(*var);
        }
        TyKind::Function(f) => {
            f.params
                .iter()
                .for_each(|p| extract_free_type_vars(p, free_types));

            extract_free_type_vars(&f.ret, free_types);

            if let Some(ty) = f.varargs.as_ref().and_then(|v| v.ty.as_ref()) {
                extract_free_type_vars(ty, free_types);
            }
        }
        TyKind::Pointer(ty, _)
        | TyKind::MultiPointer(ty, _)
        | TyKind::Array(ty, _)
        | TyKind::Slice(ty, _) => extract_free_type_vars(ty, free_types),
        TyKind::Tuple(tys) | TyKind::Infer(_, InferTy::PartialTuple(tys)) => tys
            .iter()
            .for_each(|t| extract_free_type_vars(t, free_types)),
        TyKind::Struct(StructTy { fields, .. }) => {
            fields
                .iter()
                .for_each(|f| extract_free_type_vars(&f.ty, free_types));
        }
        TyKind::Infer(_, InferTy::PartialStruct(fields)) => {
            fields
                .iter()
                .for_each(|(_, ty)| extract_free_type_vars(ty, free_types));
        }
        _ => (),
    }
}
