use super::{
    normalize::{Concrete, Normalize},
    ty_ctx::TyCtx,
};
use crate::ast::{self, ty::*};
use crate::error::{
    diagnostic::{Diagnostic, Label},
    Diagnostics,
};
use crate::span::Span;
use core::panic;
use std::collections::{HashMap, HashSet};

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

    sess.make_all_types_concrete();
}

struct Sess<'a> {
    diagnostics: &'a mut Diagnostics,
    tycx: &'a mut TyCtx,

    // map of Ty -> Set of reduced expression spans that couldn't be inferred because of the key ty
    erroneous_types: HashMap<TypeId, Vec<Span>>,

    used_types: HashSet<TypeId>,
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

    fn make_all_types_concrete(&mut self) {
        let tys: Vec<TypeId> = self
            .tycx
            .bindings
            .iter()
            .map(|(ty, _)| TypeId::from(ty))
            .collect();

        for ty in tys {
            let concrete_type = ty.concrete(&self.tycx);
            self.tycx.bind_ty(ty, concrete_type);
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
        self.bindings.iter().for_each(|(_, b)| b.substitute(sess));
    }
}

impl<'a> Substitute<'a> for ast::Binding {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.type_expr.substitute(sess);
        self.value.substitute(sess);
    }
}

impl<'a> Substitute<'a> for ast::Block {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.statements.substitute(sess);
    }
}

impl<'a> Substitute<'a> for ast::FunctionExpr {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.sig.substitute(sess);
        self.body.substitute(sess);
    }
}

impl<'a> Substitute<'a> for ast::FunctionSig {
    fn substitute(&self, sess: &mut Sess<'a>) {
        for param in self.params.iter() {
            param.type_expr.substitute(sess);
            param.ty.substitute(sess, param.pattern.span());
        }
        self.return_type.substitute(sess);
        self.ty.substitute(sess, self.span);
    }
}

impl<'a> Substitute<'a> for ast::Cast {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.expr.substitute(sess);
        self.target.substitute(sess);
        self.ty.substitute(
            sess,
            self.target.as_ref().map_or(self.expr.span(), |e| e.span()),
        );
    }
}

impl<'a> Substitute<'a> for ast::Ast {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty().substitute(sess, self.span());

        match self {
            ast::Ast::Binding(binding) => binding.substitute(sess),
            ast::Ast::Assignment(assignment) => {
                assignment.lvalue.substitute(sess);
                assignment.rvalue.substitute(sess);
            }
            ast::Ast::Cast(info) => info.substitute(sess),
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::SizeOf(expr)
                | ast::BuiltinKind::AlignOf(expr)
                | ast::BuiltinKind::Run(expr, _) => expr.substitute(sess),
                ast::BuiltinKind::Panic(expr) => expr.substitute(sess),
                ast::BuiltinKind::Import(_) => (),
            },
            ast::Ast::Function(func) => func.substitute(sess),
            ast::Ast::While(while_) => {
                while_.condition.substitute(sess);
                while_.block.substitute(sess);
            }
            ast::Ast::For(for_) => {
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
            ast::Ast::Break(_) | ast::Ast::Continue(_) => (),
            ast::Ast::Return(ret) => {
                ret.expr.substitute(sess);
            }
            ast::Ast::If(if_) => {
                if_.condition.substitute(sess);
                if_.then.substitute(sess);
                if_.otherwise.substitute(sess);
            }
            ast::Ast::Block(block) => {
                block.statements.substitute(sess);
            }
            ast::Ast::Binary(binary) => {
                binary.lhs.substitute(sess);
                binary.rhs.substitute(sess);
            }
            ast::Ast::Unary(unary) => unary.value.substitute(sess),
            ast::Ast::Subscript(sub) => {
                sub.expr.substitute(sess);
                sub.index.substitute(sess);
            }
            ast::Ast::Slice(slice) => {
                slice.expr.substitute(sess);
                slice.low.substitute(sess);
                slice.high.substitute(sess);
            }
            ast::Ast::Call(call) => {
                call.callee.substitute(sess);
                call.args.substitute(sess);
            }
            ast::Ast::MemberAccess(access) => access.expr.substitute(sess),
            ast::Ast::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(elements) => elements.substitute(sess),
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    len.substitute(sess);
                    expr.substitute(sess);
                }
            },
            ast::Ast::TupleLiteral(lit) => lit.elements.substitute(sess),
            ast::Ast::StructLiteral(lit) => {
                lit.type_expr.substitute(sess);
                for f in lit.fields.iter() {
                    f.expr.substitute(sess);
                }
            }
            ast::Ast::PointerType(expr)
            | ast::Ast::MultiPointerType(expr)
            | ast::Ast::SliceType(expr) => expr.inner.substitute(sess),
            ast::Ast::ArrayType(at) => at.inner.substitute(sess),
            ast::Ast::StructType(struct_type, ..) => {
                for f in struct_type.fields.iter() {
                    f.ty.substitute(sess);
                }
            }
            ast::Ast::FunctionType(sig) => sig.substitute(sess),
            ast::Ast::Ident(_)
            | ast::Ast::Literal(_)
            | ast::Ast::SelfType(_)
            | ast::Ast::Const(_)
            | ast::Ast::Placeholder(_) => (),
            ast::Ast::Error(_) => panic!("unexpected error node"),
        }
    }
}

trait SubstituteTy<'a> {
    fn substitute(&self, sess: &mut Sess<'a>, span: Span);
}

impl<'a> SubstituteTy<'a> for TypeId {
    fn substitute(&self, sess: &mut Sess<'a>, span: Span) {
        sess.used_types.insert(*self);

        let ty = self.normalize(sess.tycx);

        // Check if any type variables are left after normalization
        // (normalization = reducing the type variable to its concrete type, recursively)
        let mut free_types: HashSet<TypeId> = Default::default();
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

fn extract_free_type_vars(ty: &Type, free_types: &mut HashSet<TypeId>) {
    match ty {
        Type::Var(var) => {
            free_types.insert(*var);
        }
        Type::Function(f) => {
            f.params
                .iter()
                .for_each(|p| extract_free_type_vars(p, free_types));

            extract_free_type_vars(&f.ret, free_types);

            if let Some(ty) = f.varargs.as_ref().and_then(|v| v.ty.as_ref()) {
                extract_free_type_vars(ty, free_types);
            }
        }
        Type::Pointer(ty, _)
        | Type::MultiPointer(ty, _)
        | Type::Array(ty, _)
        | Type::Slice(ty, _) => extract_free_type_vars(ty, free_types),
        Type::Tuple(tys) | Type::Infer(_, InferTy::PartialTuple(tys)) => tys
            .iter()
            .for_each(|t| extract_free_type_vars(t, free_types)),
        Type::Struct(StructType { fields, .. }) => {
            fields
                .iter()
                .for_each(|f| extract_free_type_vars(&f.ty, free_types));
        }
        Type::Infer(_, InferTy::PartialStruct(fields)) => {
            fields
                .iter()
                .for_each(|(_, ty)| extract_free_type_vars(ty, free_types));
        }
        _ => (),
    }
}
