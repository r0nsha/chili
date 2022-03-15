use crate::sess::{InferSess, TyBinding, TyVar};
use chili_ast::{ast, ty::*};

pub(crate) trait Substitute {
    fn substitute(&mut self, sess: &InferSess);
}

impl<T: Substitute> Substitute for Vec<T> {
    fn substitute(&mut self, sess: &InferSess) {
        for element in self {
            element.substitute(sess);
        }
    }
}

impl<T: Substitute> Substitute for Option<T> {
    fn substitute(&mut self, sess: &InferSess) {
        if let Some(e) = self {
            e.substitute(sess);
        }
    }
}

impl<T: Substitute> Substitute for Box<T> {
    fn substitute(&mut self, sess: &InferSess) {
        self.as_mut().substitute(sess)
    }
}

impl Substitute for ast::Ast {
    fn substitute(&mut self, sess: &InferSess) {
        for binding in self.bindings.iter_mut() {
            binding.substitute(sess);
        }
    }
}

impl Substitute for ast::Binding {
    fn substitute(&mut self, sess: &InferSess) {
        self.ty_expr.substitute(sess);
        self.expr.substitute(sess);
    }
}

impl Substitute for ast::Block {
    fn substitute(&mut self, sess: &InferSess) {
        self.exprs.substitute(sess);
        self.deferred.substitute(sess);
    }
}

impl Substitute for ast::Fn {
    fn substitute(&mut self, sess: &InferSess) {
        self.proto.substitute(sess);
        self.body.substitute(sess);
    }
}

impl Substitute for ast::Proto {
    fn substitute(&mut self, sess: &InferSess) {
        for param in self.params.iter_mut() {
            param.ty.substitute(sess);
        }
        self.ret.substitute(sess);
        self.ty = substitute_ty(&self.ty, sess);
    }
}

impl Substitute for ast::Cast {
    fn substitute(&mut self, sess: &InferSess) {
        self.expr.substitute(sess);
        self.type_expr.substitute(sess);
        self.target_ty = substitute_ty(&self.target_ty, sess);
    }
}

impl Substitute for ast::Expr {
    fn substitute(&mut self, sess: &InferSess) {
        match &mut self.kind {
            ast::ExprKind::Import(..) | ast::ExprKind::Defer(_) => (),
            ast::ExprKind::Foreign(bindings) => {
                bindings.substitute(sess);
            }
            ast::ExprKind::Binding(binding) => {
                binding.substitute(sess);
            }
            ast::ExprKind::Assign { lvalue, rvalue } => {
                lvalue.substitute(sess);
                rvalue.substitute(sess);
            }
            ast::ExprKind::Cast(info) => info.substitute(sess),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.substitute(sess),
                ast::Builtin::Panic(expr) => {
                    expr.substitute(sess);
                }
            },
            ast::ExprKind::Fn(func) => {
                func.substitute(sess);
            }
            ast::ExprKind::While { cond, expr } => {
                cond.substitute(sess);
                expr.substitute(sess);
            }
            ast::ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        start.substitute(sess);
                        end.substitute(sess);
                    }
                    ast::ForIter::Value(value) => {
                        value.substitute(sess);
                    }
                }

                for_.expr.substitute(sess);
            }
            ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
                deferred.substitute(sess);
            }
            ast::ExprKind::Return { expr, deferred } => {
                deferred.substitute(sess);
                expr.substitute(sess);
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.substitute(sess);
                then_expr.substitute(sess);
                else_expr.substitute(sess);
            }
            ast::ExprKind::Block(block) => {
                block.exprs.substitute(sess);
                block.deferred.substitute(sess);
            }
            ast::ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.substitute(sess);
                rhs.substitute(sess);
            }
            ast::ExprKind::Unary { op: _, lhs } => {
                lhs.substitute(sess);
            }
            ast::ExprKind::Subscript { expr, index } => {
                expr.substitute(sess);
                index.substitute(sess);
            }
            ast::ExprKind::Slice { expr, low, high } => {
                expr.substitute(sess);
                low.substitute(sess);
                high.substitute(sess);
            }
            ast::ExprKind::Call(call) => {
                call.callee.substitute(sess);
                for arg in call.args.iter_mut() {
                    arg.value.substitute(sess);
                }
            }
            ast::ExprKind::MemberAccess { expr, .. } => {
                expr.substitute(sess);
            }
            ast::ExprKind::ArrayLiteral(kind) => match kind {
                ast::ArrayLiteralKind::List(elements) => {
                    elements.substitute(sess);
                }
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    len.substitute(sess);
                    expr.substitute(sess);
                }
            },
            ast::ExprKind::TupleLiteral(elements) => {
                elements.substitute(sess);
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.substitute(sess);
                for f in fields {
                    f.value.substitute(sess);
                }
            }

            ast::ExprKind::PointerType(expr, ..)
            | ast::ExprKind::MultiPointerType(expr, ..)
            | ast::ExprKind::SliceType(expr, ..)
            | ast::ExprKind::ArrayType(expr, ..) => expr.substitute(sess),

            ast::ExprKind::StructType(struct_type, ..) => {
                for f in struct_type.fields.iter_mut() {
                    f.ty.substitute(sess);
                }
            }

            ast::ExprKind::FnType(proto) => {
                proto.substitute(sess);
            }

            ast::ExprKind::Id { .. }
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType
            | ast::ExprKind::Noop => (),
        }

        self.ty = substitute_ty(&self.ty, sess);
    }
}

pub(crate) fn substitute_ty(ty: &Ty, sess: &InferSess) -> Ty {
    match ty {
        Ty::Var(var) => find_type_or_default(TyVar(*var), sess, || {
            panic!(
                "couldn't figure out the type of {}, because it was unbound",
                TyVar(*var)
            )
        }),
        Ty::Fn(f) => Ty::Fn(FnTy {
            params: f
                .params
                .iter()
                .map(|p| FnTyParam {
                    symbol: p.symbol,
                    ty: substitute_ty(&p.ty, sess),
                })
                .collect(),
            ret: Box::new(substitute_ty(&f.ret, sess)),
            variadic: f.variadic,
            lib_name: f.lib_name,
        }),
        Ty::Pointer(ty, a) => Ty::Pointer(Box::new(substitute_ty(ty, sess)), *a),
        Ty::MultiPointer(ty, a) => Ty::MultiPointer(Box::new(substitute_ty(ty, sess)), *a),
        Ty::Array(ty, a) => Ty::Array(Box::new(substitute_ty(ty, sess)), *a),
        Ty::Slice(ty, a) => Ty::Slice(Box::new(substitute_ty(ty, sess)), *a),
        Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|ty| substitute_ty(&ty, sess)).collect()),
        Ty::Struct(st) => Ty::Struct(StructTy {
            name: st.name,
            qualified_name: st.qualified_name,
            binding_info_id: st.binding_info_id,
            fields: st
                .fields
                .iter()
                .map(|f| StructTyField {
                    symbol: f.symbol,
                    ty: substitute_ty(&f.ty, sess),
                    span: f.span,
                })
                .collect(),
            kind: st.kind,
        }),
        Ty::AnyInt(var) => find_type_or_default(TyVar(*var), sess, || Ty::Int(IntTy::default())),
        Ty::AnyFloat(var) => {
            find_type_or_default(TyVar(*var), sess, || Ty::Float(FloatTy::default()))
        }
        _ => ty.clone(),
    }
}

fn find_type_or_default<F: FnOnce() -> Ty>(var: TyVar, sess: &InferSess, default: F) -> Ty {
    match sess.find_type_binding(var) {
        TyBinding::Bound(ty) => substitute_ty(&ty, sess),
        TyBinding::Unbound => default(),
    }
}
