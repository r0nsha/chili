use core::panic;
use std::collections::HashSet;

use chili_ast::{ast, ty::*};
use chili_span::Span;

use crate::{normalize::NormalizeTy, ty_ctx::TyCtx};

pub trait Substitute {
    fn substitute(&self, tycx: &mut TyCtx);
}

impl<T: Substitute> Substitute for Vec<T> {
    fn substitute(&self, tycx: &mut TyCtx) {
        for element in self {
            element.substitute(tycx);
        }
    }
}

impl<T: Substitute> Substitute for Option<T> {
    fn substitute(&self, tycx: &mut TyCtx) {
        if let Some(e) = self {
            e.substitute(tycx);
        }
    }
}

impl<T: Substitute> Substitute for Box<T> {
    fn substitute(&self, tycx: &mut TyCtx) {
        self.as_ref().substitute(tycx)
    }
}

impl Substitute for ast::TypedAst {
    fn substitute(&self, tycx: &mut TyCtx) {
        for binding in self.bindings.values() {
            binding.substitute(tycx);
        }
    }
}

impl Substitute for ast::Binding {
    fn substitute(&self, tycx: &mut TyCtx) {
        self.ty_expr.substitute(tycx);
        self.expr.substitute(tycx);
    }
}

impl Substitute for ast::Block {
    fn substitute(&self, tycx: &mut TyCtx) {
        self.exprs.substitute(tycx);
        self.deferred.substitute(tycx);
    }
}

impl Substitute for ast::Fn {
    fn substitute(&self, tycx: &mut TyCtx) {
        self.sig.substitute(tycx);
        self.body.substitute(tycx);
    }
}

impl Substitute for ast::FnSig {
    fn substitute(&self, tycx: &mut TyCtx) {
        for param in self.params.iter() {
            param.ty_expr.substitute(tycx);
            param.ty.substitute(tycx, param.pattern.span());
        }
        self.ret.substitute(tycx);
        // TODO: replace unknown span
        self.ty.substitute(tycx, Span::unknown());
    }
}

impl Substitute for ast::Cast {
    fn substitute(&self, tycx: &mut TyCtx) {
        self.expr.substitute(tycx);
        self.ty_expr.substitute(tycx);
        // TODO: replace unknown span
        self.target_ty.substitute(
            tycx,
            self.ty_expr.as_ref().map_or(Span::unknown(), |e| e.span),
        );
    }
}

impl Substitute for ast::Expr {
    fn substitute(&self, tycx: &mut TyCtx) {
        match &self.kind {
            ast::ExprKind::Import(..) | ast::ExprKind::Defer(_) => (),
            ast::ExprKind::Foreign(bindings) => {
                bindings.substitute(tycx);
            }
            ast::ExprKind::Binding(binding) => {
                binding.substitute(tycx);
            }
            ast::ExprKind::Assign(assign) => {
                assign.lvalue.substitute(tycx);
                assign.rvalue.substitute(tycx);
            }
            ast::ExprKind::Cast(info) => info.substitute(tycx),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.substitute(tycx),
                ast::Builtin::Panic(expr) => {
                    expr.substitute(tycx);
                }
            },
            ast::ExprKind::Fn(func) => {
                func.substitute(tycx);
            }
            ast::ExprKind::While(while_) => {
                while_.cond.substitute(tycx);
                while_.block.substitute(tycx);
            }
            ast::ExprKind::For(for_) => {
                match &for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        start.substitute(tycx);
                        end.substitute(tycx);
                    }
                    ast::ForIter::Value(value) => {
                        value.substitute(tycx);
                    }
                }

                for_.block.substitute(tycx);
            }
            ast::ExprKind::Break(e) | ast::ExprKind::Continue(e) => {
                e.deferred.substitute(tycx);
            }
            ast::ExprKind::Return(ret) => {
                ret.deferred.substitute(tycx);
                ret.expr.substitute(tycx);
            }
            ast::ExprKind::If(if_) => {
                if_.cond.substitute(tycx);
                if_.then.substitute(tycx);
                if_.otherwise.substitute(tycx);
            }
            ast::ExprKind::Block(block) => {
                block.exprs.substitute(tycx);
                block.deferred.substitute(tycx);
            }
            ast::ExprKind::Binary(binary) => {
                binary.lhs.substitute(tycx);
                binary.rhs.substitute(tycx);
            }
            ast::ExprKind::Unary(unary) => {
                unary.lhs.substitute(tycx);
            }
            ast::ExprKind::Subscript(sub) => {
                sub.expr.substitute(tycx);
                sub.index.substitute(tycx);
            }
            ast::ExprKind::Slice(slice) => {
                slice.expr.substitute(tycx);
                slice.low.substitute(tycx);
                slice.high.substitute(tycx);
            }
            ast::ExprKind::FnCall(call) => {
                call.callee.substitute(tycx);
                call.args.substitute(tycx);
            }
            ast::ExprKind::MemberAccess(access) => {
                access.expr.substitute(tycx);
            }
            ast::ExprKind::ArrayLiteral(kind) => match kind {
                ast::ArrayLiteralKind::List(elements) => {
                    elements.substitute(tycx);
                }
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    len.substitute(tycx);
                    expr.substitute(tycx);
                }
            },
            ast::ExprKind::TupleLiteral(elements) => {
                elements.substitute(tycx);
            }
            ast::ExprKind::StructLiteral(lit) => {
                lit.type_expr.substitute(tycx);
                for f in lit.fields.iter() {
                    f.value.substitute(tycx);
                }
            }

            ast::ExprKind::PointerType(expr, ..)
            | ast::ExprKind::MultiPointerType(expr, ..)
            | ast::ExprKind::SliceType(expr, ..)
            | ast::ExprKind::ArrayType(expr, ..) => expr.substitute(tycx),

            ast::ExprKind::StructType(struct_type, ..) => {
                for f in struct_type.fields.iter() {
                    f.ty.substitute(tycx);
                }
            }

            ast::ExprKind::FnType(sig) => {
                sig.substitute(tycx);
            }

            ast::ExprKind::Ident(_)
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType => (),

            ast::ExprKind::Error => panic!("unexpected error node"),
        }

        self.ty.substitute(tycx, self.span);
    }
}

trait SubstituteTy {
    fn substitute(&self, tycx: &mut TyCtx, span: Span);
}
impl SubstituteTy for Ty {
    fn substitute(&self, tycx: &mut TyCtx, span: Span) {
        let ty = self.normalize(tycx);

        // Check if any type variables are left after normalization
        // (normalization = reducing the type variable to its concrete type, recursively)
        let mut free_tys: HashSet<Ty> = Default::default();
        extract_free_type_vars(&ty, &mut free_tys);

        if free_tys.is_empty() {
            tycx.bind(*self, ty);
        } else {
            println!("{:?}", free_tys);
        }
    }
}

fn extract_free_type_vars(ty: &TyKind, set: &mut HashSet<Ty>) {
    match ty {
        TyKind::Var(var) => {
            set.insert(*var);
        }
        TyKind::Fn(f) => {
            f.params.iter().for_each(|p| extract_free_type_vars(p, set));
            extract_free_type_vars(&f.ret, set);
        }
        TyKind::Pointer(ty, _)
        | TyKind::MultiPointer(ty, _)
        | TyKind::Array(ty, _)
        | TyKind::Slice(ty, _) => extract_free_type_vars(ty, set),
        TyKind::Tuple(tys) => tys.iter().for_each(|t| extract_free_type_vars(t, set)),
        TyKind::Struct(st) => {
            st.fields
                .iter()
                .for_each(|f| extract_free_type_vars(&f.ty, set));
        }
        _ => (),
    }
}
