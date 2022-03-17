use crate::tyctx::{TyBinding, TyContext};
use chili_ast::{ast, ty::*};

pub(crate) trait Substitute {
    fn substitute(&mut self, ctx: &TyContext);
}

impl<T: Substitute> Substitute for Vec<T> {
    fn substitute(&mut self, ctx: &TyContext) {
        for element in self {
            element.substitute(ctx);
        }
    }
}

impl<T: Substitute> Substitute for Option<T> {
    fn substitute(&mut self, ctx: &TyContext) {
        if let Some(e) = self {
            e.substitute(ctx);
        }
    }
}

impl<T: Substitute> Substitute for Box<T> {
    fn substitute(&mut self, ctx: &TyContext) {
        self.as_mut().substitute(ctx)
    }
}

impl Substitute for ast::Ast {
    fn substitute(&mut self, ctx: &TyContext) {
        for binding in self.bindings.iter_mut() {
            binding.substitute(ctx);
        }
    }
}

impl Substitute for ast::Binding {
    fn substitute(&mut self, ctx: &TyContext) {
        self.ty_expr.substitute(ctx);
        self.expr.substitute(ctx);
    }
}

impl Substitute for ast::Block {
    fn substitute(&mut self, ctx: &TyContext) {
        self.exprs.substitute(ctx);
        self.deferred.substitute(ctx);
    }
}

impl Substitute for ast::Fn {
    fn substitute(&mut self, ctx: &TyContext) {
        self.proto.substitute(ctx);
        self.body.substitute(ctx);
    }
}

impl Substitute for ast::Proto {
    fn substitute(&mut self, ctx: &TyContext) {
        for param in self.params.iter_mut() {
            param.ty.substitute(ctx);
        }
        self.ret.substitute(ctx);
        self.ty = substitute_ty(&self.ty, ctx);
    }
}

impl Substitute for ast::Cast {
    fn substitute(&mut self, ctx: &TyContext) {
        self.expr.substitute(ctx);
        self.type_expr.substitute(ctx);
        self.target_ty = substitute_ty(&self.target_ty, ctx);
    }
}

impl Substitute for ast::Expr {
    fn substitute(&mut self, ctx: &TyContext) {
        match &mut self.kind {
            ast::ExprKind::Import(..) | ast::ExprKind::Defer(_) => (),
            ast::ExprKind::Foreign(bindings) => {
                bindings.substitute(ctx);
            }
            ast::ExprKind::Binding(binding) => {
                binding.substitute(ctx);
            }
            ast::ExprKind::Assign { lvalue, rvalue } => {
                lvalue.substitute(ctx);
                rvalue.substitute(ctx);
            }
            ast::ExprKind::Cast(info) => info.substitute(ctx),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.substitute(ctx),
                ast::Builtin::Panic(expr) => {
                    expr.substitute(ctx);
                }
            },
            ast::ExprKind::Fn(func) => {
                func.substitute(ctx);
            }
            ast::ExprKind::While { cond, expr } => {
                cond.substitute(ctx);
                expr.substitute(ctx);
            }
            ast::ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        start.substitute(ctx);
                        end.substitute(ctx);
                    }
                    ast::ForIter::Value(value) => {
                        value.substitute(ctx);
                    }
                }

                for_.expr.substitute(ctx);
            }
            ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
                deferred.substitute(ctx);
            }
            ast::ExprKind::Return { expr, deferred } => {
                deferred.substitute(ctx);
                expr.substitute(ctx);
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.substitute(ctx);
                then_expr.substitute(ctx);
                else_expr.substitute(ctx);
            }
            ast::ExprKind::Block(block) => {
                block.exprs.substitute(ctx);
                block.deferred.substitute(ctx);
            }
            ast::ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.substitute(ctx);
                rhs.substitute(ctx);
            }
            ast::ExprKind::Unary { op: _, lhs } => {
                lhs.substitute(ctx);
            }
            ast::ExprKind::Subscript { expr, index } => {
                expr.substitute(ctx);
                index.substitute(ctx);
            }
            ast::ExprKind::Slice { expr, low, high } => {
                expr.substitute(ctx);
                low.substitute(ctx);
                high.substitute(ctx);
            }
            ast::ExprKind::Call(call) => {
                call.callee.substitute(ctx);
                for arg in call.args.iter_mut() {
                    arg.value.substitute(ctx);
                }
            }
            ast::ExprKind::MemberAccess { expr, .. } => {
                expr.substitute(ctx);
            }
            ast::ExprKind::ArrayLiteral(kind) => match kind {
                ast::ArrayLiteralKind::List(elements) => {
                    elements.substitute(ctx);
                }
                ast::ArrayLiteralKind::Fill { expr, len } => {
                    len.substitute(ctx);
                    expr.substitute(ctx);
                }
            },
            ast::ExprKind::TupleLiteral(elements) => {
                elements.substitute(ctx);
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.substitute(ctx);
                for f in fields {
                    f.value.substitute(ctx);
                }
            }

            ast::ExprKind::PointerType(expr, ..)
            | ast::ExprKind::MultiPointerType(expr, ..)
            | ast::ExprKind::SliceType(expr, ..)
            | ast::ExprKind::ArrayType(expr, ..) => expr.substitute(ctx),

            ast::ExprKind::StructType(struct_type, ..) => {
                for f in struct_type.fields.iter_mut() {
                    f.ty.substitute(ctx);
                }
            }

            ast::ExprKind::FnType(proto) => {
                proto.substitute(ctx);
            }

            ast::ExprKind::Id { .. }
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType
            | ast::ExprKind::Noop => (),
        }

        self.ty = substitute_ty(&self.ty, ctx);
    }
}

pub(crate) fn substitute_ty(ty: &Ty, ctx: &TyContext) -> Ty {
    match ty {
        Ty::Var(var) => find_type_or_default(*var, ctx, || {
            panic!(
                "couldn't figure out the type of {}, because it was unbound",
                var
            )
        }),
        Ty::Fn(f) => Ty::Fn(FnTy {
            params: f
                .params
                .iter()
                .map(|p| FnTyParam {
                    symbol: p.symbol,
                    ty: substitute_ty(&p.ty, ctx),
                })
                .collect(),
            ret: Box::new(substitute_ty(&f.ret, ctx)),
            variadic: f.variadic,
            lib_name: f.lib_name,
        }),
        Ty::Pointer(ty, a) => Ty::Pointer(Box::new(substitute_ty(ty, ctx)), *a),
        Ty::MultiPointer(ty, a) => Ty::MultiPointer(Box::new(substitute_ty(ty, ctx)), *a),
        Ty::Array(ty, a) => Ty::Array(Box::new(substitute_ty(ty, ctx)), *a),
        Ty::Slice(ty, a) => Ty::Slice(Box::new(substitute_ty(ty, ctx)), *a),
        Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|ty| substitute_ty(&ty, ctx)).collect()),
        Ty::Struct(st) => Ty::Struct(StructTy {
            name: st.name,
            qualified_name: st.qualified_name,
            binding_info_id: st.binding_info_id,
            fields: st
                .fields
                .iter()
                .map(|f| StructTyField {
                    symbol: f.symbol,
                    ty: substitute_ty(&f.ty, ctx),
                    span: f.span,
                })
                .collect(),
            kind: st.kind,
        }),
        Ty::AnyInt(var) => find_type_or_default(*var, ctx, || Ty::Int(IntTy::default())),
        Ty::AnyFloat(var) => find_type_or_default(*var, ctx, || Ty::Float(FloatTy::default())),
        _ => ty.clone(),
    }
}

fn find_type_or_default<F: FnOnce() -> Ty>(var: TyVar, ctx: &TyContext, default: F) -> Ty {
    match ctx.find_type_binding(var) {
        TyBinding::Bound(ty) => substitute_ty(&ty, ctx),
        TyBinding::Unbound => default(),
    }
}
