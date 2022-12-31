use super::{normalize::Normalize, type_ctx::TypeCtx};
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir,
    span::Span,
    types::*,
};
use std::collections::{HashMap, HashSet};

pub fn substitute_cache<'a>(cache: &'a hir::Cache, tcx: &'a mut TypeCtx) -> Result<(), Vec<Diagnostic>> {
    let mut sess = Sess {
        tcx,
        erroneous_types: HashMap::new(),
    };

    cache.substitute(&mut sess);
    sess.finish()
}

pub fn substitute_node<'a>(node: &'a hir::Node, tcx: &'a mut TypeCtx) -> Result<(), Vec<Diagnostic>> {
    let mut sess = Sess {
        tcx,
        erroneous_types: HashMap::new(),
    };

    node.substitute(&mut sess);
    sess.finish()
}

struct Sess<'a> {
    tcx: &'a mut TypeCtx,
    // map of Ty -> Set of reduced expression spans that couldn't be inferred because of the key ty
    erroneous_types: HashMap<TypeId, Vec<Span>>,
}

impl<'a> Sess<'a> {
    fn finish(self) -> Result<(), Vec<Diagnostic>> {
        if self.erroneous_types.is_empty() {
            Ok(())
        } else {
            Err(self.collect_diagnostics())
        }
    }

    fn collect_diagnostics(&self) -> Vec<Diagnostic> {
        self.erroneous_types
            .iter()
            .flat_map(|(&ty, spans)| {
                let ty_span = self.tcx.ty_span(ty);

                let ty_origin_label = ty_span.map(|span| Label::secondary(span, "incomplete type originates here"));

                spans
                    .iter()
                    .filter(|&&span| ty_span.map_or(true, |ty_span| span != ty_span))
                    .map(|&span| {
                        Diagnostic::error()
                            .with_message("can't infer the expression's type")
                            .with_label(Label::primary(span, "can't infer this type"))
                            .maybe_with_label(ty_origin_label.clone())
                            .with_note("try adding more type information")
                    })
                    .collect::<Vec<Diagnostic>>()
            })
            .collect()
    }
}

trait Substitute<'a> {
    fn substitute(&self, sess: &mut Sess<'a>);
}

impl<'a, T: Substitute<'a>> Substitute<'a> for Vec<T> {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.as_slice().substitute(sess);
    }
}

impl<'a, T: Substitute<'a>> Substitute<'a> for &[T] {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.iter().for_each(|v| v.substitute(sess));
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

impl<'a> Substitute<'a> for hir::Cache {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.bindings.iter().for_each(|(_, b)| b.substitute(sess));
        self.functions.iter().for_each(|(_, f)| f.substitute(sess));
    }
}

impl<'a> Substitute<'a> for hir::Node {
    fn substitute(&self, sess: &mut Sess<'a>) {
        match self {
            hir::Node::Const(x) => x.substitute(sess),
            hir::Node::Binding(x) => x.substitute(sess),
            hir::Node::Id(x) => x.substitute(sess),
            hir::Node::Assign(x) => x.substitute(sess),
            hir::Node::MemberAccess(x) => x.substitute(sess),
            hir::Node::Call(x) => x.substitute(sess),
            hir::Node::Cast(x) => x.substitute(sess),
            hir::Node::Sequence(x) => x.substitute(sess),
            hir::Node::Control(x) => x.substitute(sess),
            hir::Node::Builtin(x) => x.substitute(sess),
            hir::Node::Literal(x) => x.substitute(sess),
        }
    }
}

impl<'a> Substitute<'a> for hir::Const {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
    }
}

impl<'a> Substitute<'a> for hir::Binding {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Id {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
    }
}

impl<'a> Substitute<'a> for hir::Assign {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.lhs.substitute(sess);
        self.rhs.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::MemberAccess {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Call {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.callee.substitute(sess);
        self.args.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Control {
    fn substitute(&self, sess: &mut Sess<'a>) {
        match self {
            hir::Control::If(if_) => {
                if_.ty.substitute(sess, if_.span);
                if_.condition.substitute(sess);
                if_.then.substitute(sess);
                if_.otherwise.substitute(sess);
            }
            hir::Control::While(while_) => {
                while_.ty.substitute(sess, while_.span);
                while_.condition.substitute(sess);
                while_.body.substitute(sess);
            }
            hir::Control::Return(return_) => {
                return_.ty.substitute(sess, return_.span);
                return_.value.substitute(sess);
            }
            hir::Control::Break(term) | hir::Control::Continue(term) => term.ty.substitute(sess, term.span),
        }
    }
}

impl<'a> Substitute<'a> for hir::Builtin {
    fn substitute(&self, sess: &mut Sess<'a>) {
        match self {
            hir::Builtin::Add(x) => x.substitute(sess),
            hir::Builtin::Sub(x) => x.substitute(sess),
            hir::Builtin::Mul(x) => x.substitute(sess),
            hir::Builtin::Div(x) => x.substitute(sess),
            hir::Builtin::Rem(x) => x.substitute(sess),
            hir::Builtin::Shl(x) => x.substitute(sess),
            hir::Builtin::Shr(x) => x.substitute(sess),
            hir::Builtin::And(x) => x.substitute(sess),
            hir::Builtin::Or(x) => x.substitute(sess),
            hir::Builtin::Lt(x) => x.substitute(sess),
            hir::Builtin::Le(x) => x.substitute(sess),
            hir::Builtin::Gt(x) => x.substitute(sess),
            hir::Builtin::Ge(x) => x.substitute(sess),
            hir::Builtin::Eq(x) => x.substitute(sess),
            hir::Builtin::Ne(x) => x.substitute(sess),
            hir::Builtin::BitAnd(x) => x.substitute(sess),
            hir::Builtin::BitOr(x) => x.substitute(sess),
            hir::Builtin::BitXor(x) => x.substitute(sess),
            hir::Builtin::Not(x) => x.substitute(sess),
            hir::Builtin::Neg(x) => x.substitute(sess),
            hir::Builtin::Ref(x) => x.substitute(sess),
            hir::Builtin::Deref(x) => x.substitute(sess),
            hir::Builtin::Offset(x) => x.substitute(sess),
            hir::Builtin::Slice(x) => x.substitute(sess),
        }
    }
}

impl<'a> Substitute<'a> for hir::Binary {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.lhs.substitute(sess);
        self.rhs.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Unary {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Ref {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Offset {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
        self.index.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Slice {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
        self.low.substitute(sess);
        self.high.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Function {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        match &self.kind {
            hir::FunctionKind::Orphan { body, .. } => body.substitute(sess),
            hir::FunctionKind::Extern { .. } | hir::FunctionKind::Intrinsic(..) => (),
        }
    }
}

impl<'a> Substitute<'a> for hir::Literal {
    fn substitute(&self, sess: &mut Sess<'a>) {
        match self {
            hir::Literal::Struct(lit) => {
                lit.ty.substitute(sess, lit.span);
                for field in lit.fields.iter() {
                    field.ty.substitute(sess, field.span);
                    field.value.substitute(sess);
                }
            }
            hir::Literal::Tuple(lit) => {
                lit.ty.substitute(sess, lit.span);
                lit.elements.substitute(sess);
            }
            hir::Literal::Array(lit) => {
                lit.ty.substitute(sess, lit.span);
                lit.elements.substitute(sess);
            }
            hir::Literal::ArrayFill(lit) => {
                lit.ty.substitute(sess, lit.span);
                lit.value.substitute(sess);
            }
        }
    }
}

impl<'a> Substitute<'a> for hir::Sequence {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.statements.substitute(sess);
    }
}

impl<'a> Substitute<'a> for hir::Cast {
    fn substitute(&self, sess: &mut Sess<'a>) {
        self.ty.substitute(sess, self.span);
        self.value.substitute(sess);
    }
}

trait SubstituteTy<'a> {
    fn substitute(&self, sess: &mut Sess<'a>, span: Span);
}

impl<'a> SubstituteTy<'a> for TypeId {
    fn substitute(&self, sess: &mut Sess<'a>, span: Span) {
        let mut ty = self.normalize(sess.tcx);

        // Check if any type variables are left after normalization
        let mut free_types: HashSet<TypeId> = Default::default();
        extract_free_type_vars(&mut ty, &mut free_types, sess.tcx);

        if free_types.is_empty() {
            sess.tcx.bind_ty(*self, ty);
        } else {
            for &ty in free_types.iter() {
                let span_set = sess.erroneous_types.entry(ty).or_default();

                if let Some(index) = span_set
                    .iter()
                    .position(|s| s.start.index <= span.start.index && s.end.index >= span.end.index)
                {
                    span_set.remove(index);
                }

                span_set.push(span);
            }
        }
    }
}

fn extract_free_type_vars(ty: &mut Type, free_types: &mut HashSet<TypeId>, tcx: &mut TypeCtx) {
    match ty {
        Type::Var(var) => {
            free_types.insert(*var);
        }

        Type::Infer(id, InferType::AnyInt) => {
            let concrete = Type::int();
            tcx.bind_ty(*id, concrete.clone());
            *ty = concrete;
        }

        Type::Infer(id, InferType::AnyFloat) => {
            let concrete = Type::float();
            tcx.bind_ty(*id, concrete.clone());
            *ty = concrete;
        }

        Type::Function(f) => {
            f.params
                .iter_mut()
                .for_each(|p| extract_free_type_vars(&mut p.ty, free_types, tcx));

            extract_free_type_vars(&mut f.return_type, free_types, tcx);

            if let Some(ty) = f.varargs.as_mut().and_then(|v| v.ty.as_mut()) {
                extract_free_type_vars(ty, free_types, tcx);
            }
        }

        Type::Pointer(ty, _) | Type::Array(ty, _) | Type::Slice(ty) | Type::Str(ty) | Type::Type(ty) => {
            extract_free_type_vars(ty, free_types, tcx)
        }

        Type::Tuple(tys) => tys.iter_mut().for_each(|t| extract_free_type_vars(t, free_types, tcx)),

        Type::Struct(StructType { fields, .. }) => {
            fields
                .iter_mut()
                .for_each(|f| extract_free_type_vars(&mut f.ty, free_types, tcx));
        }

        Type::Never
        | Type::Unit
        | Type::Bool
        | Type::Int(_)
        | Type::Uint(_)
        | Type::Float(_)
        | Type::Module(_)
        | Type::AnyType => (),
    }
}
