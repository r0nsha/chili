// use crate::tycx::{TyBinding, TyContext};
// use chili_ast::{ast, ty::*};

// pub(crate) trait Substitute {
//     fn substitute(&mut self, tycx: &TyContext);
// }

// impl<T: Substitute> Substitute for Vec<T> {
//     fn substitute(&mut self, tycx: &TyContext) {
//         for element in self {
//             element.substitute(tycx);
//         }
//     }
// }

// impl<T: Substitute> Substitute for Option<T> {
//     fn substitute(&mut self, tycx: &TyContext) {
//         if let Some(e) = self {
//             e.substitute(tycx);
//         }
//     }
// }

// impl<T: Substitute> Substitute for Box<T> {
//     fn substitute(&mut self, tycx: &TyContext) {
//         self.as_mut().substitute(tycx)
//     }
// }

// impl Substitute for ast::Ast {
//     fn substitute(&mut self, tycx: &TyContext) {
//         for binding in self.bindings.iter_mut() {
//             binding.substitute(tycx);
//         }
//     }
// }

// impl Substitute for ast::Binding {
//     fn substitute(&mut self, tycx: &TyContext) {
//         self.ty_expr.substitute(tycx);
//         self.expr.substitute(tycx);
//     }
// }

// impl Substitute for ast::Block {
//     fn substitute(&mut self, tycx: &TyContext) {
//         self.exprs.substitute(tycx);
//         self.deferred.substitute(tycx);
//     }
// }

// impl Substitute for ast::Fn {
//     fn substitute(&mut self, tycx: &TyContext) {
//         self.proto.substitute(tycx);
//         self.body.substitute(tycx);
//     }
// }

// impl Substitute for ast::Proto {
//     fn substitute(&mut self, tycx: &TyContext) {
//         for param in self.params.iter_mut() {
//             param.ty.substitute(tycx);
//         }
//         self.ret.substitute(tycx);
//         self.ty = substitute_ty(&self.ty, tycx);
//     }
// }

// impl Substitute for ast::Cast {
//     fn substitute(&mut self, tycx: &TyContext) {
//         self.expr.substitute(tycx);
//         self.type_expr.substitute(tycx);
//         self.target_ty = substitute_ty(&self.target_ty, tycx);
//     }
// }

// impl Substitute for ast::Expr {
//     fn substitute(&mut self, tycx: &TyContext) {
//         match &mut self.kind {
//             ast::ExprKind::Import(..) | ast::ExprKind::Defer(_) => (),
//             ast::ExprKind::Foreign(bindings) => {
//                 bindings.substitute(tycx);
//             }
//             ast::ExprKind::Binding(binding) => {
//                 binding.substitute(tycx);
//             }
//             ast::ExprKind::Assign { lvalue, rvalue } => {
//                 lvalue.substitute(tycx);
//                 rvalue.substitute(tycx);
//             }
//             ast::ExprKind::Cast(info) => info.substitute(tycx),
//             ast::ExprKind::Builtin(builtin) => match builtin {
//                 ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.substitute(tycx),
//                 ast::Builtin::Panic(expr) => {
//                     expr.substitute(tycx);
//                 }
//             },
//             ast::ExprKind::Fn(func) => {
//                 func.substitute(tycx);
//             }
//             ast::ExprKind::While { cond, expr } => {
//                 cond.substitute(tycx);
//                 expr.substitute(tycx);
//             }
//             ast::ExprKind::For(for_) => {
//                 match &mut for_.iterator {
//                     ast::ForIter::Range(start, end) => {
//                         start.substitute(tycx);
//                         end.substitute(tycx);
//                     }
//                     ast::ForIter::Value(value) => {
//                         value.substitute(tycx);
//                     }
//                 }

//                 for_.expr.substitute(tycx);
//             }
//             ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
//                 deferred.substitute(tycx);
//             }
//             ast::ExprKind::Return { expr, deferred } => {
//                 deferred.substitute(tycx);
//                 expr.substitute(tycx);
//             }
//             ast::ExprKind::If {
//                 cond,
//                 then_expr,
//                 else_expr,
//             } => {
//                 cond.substitute(tycx);
//                 then_expr.substitute(tycx);
//                 else_expr.substitute(tycx);
//             }
//             ast::ExprKind::Block(block) => {
//                 block.exprs.substitute(tycx);
//                 block.deferred.substitute(tycx);
//             }
//             ast::ExprKind::Binary { lhs, op: _, rhs } => {
//                 lhs.substitute(tycx);
//                 rhs.substitute(tycx);
//             }
//             ast::ExprKind::Unary { op: _, lhs } => {
//                 lhs.substitute(tycx);
//             }
//             ast::ExprKind::Subscript { expr, index } => {
//                 expr.substitute(tycx);
//                 index.substitute(tycx);
//             }
//             ast::ExprKind::Slice { expr, low, high } => {
//                 expr.substitute(tycx);
//                 low.substitute(tycx);
//                 high.substitute(tycx);
//             }
//             ast::ExprKind::Call(call) => {
//                 call.callee.substitute(tycx);
//                 for arg in call.args.iter_mut() {
//                     arg.value.substitute(tycx);
//                 }
//             }
//             ast::ExprKind::MemberAccess { expr, .. } => {
//                 expr.substitute(tycx);
//             }
//             ast::ExprKind::ArrayLiteral(kind) => match kind {
//                 ast::ArrayLiteralKind::List(elements) => {
//                     elements.substitute(tycx);
//                 }
//                 ast::ArrayLiteralKind::Fill { expr, len } => {
//                     len.substitute(tycx);
//                     expr.substitute(tycx);
//                 }
//             },
//             ast::ExprKind::TupleLiteral(elements) => {
//                 elements.substitute(tycx);
//             }
//             ast::ExprKind::StructLiteral { type_expr, fields } => {
//                 type_expr.substitute(tycx);
//                 for f in fields {
//                     f.value.substitute(tycx);
//                 }
//             }

//             ast::ExprKind::PointerType(expr, ..)
//             | ast::ExprKind::MultiPointerType(expr, ..)
//             | ast::ExprKind::SliceType(expr, ..)
//             | ast::ExprKind::ArrayType(expr, ..) => expr.substitute(tycx),

//             ast::ExprKind::StructType(struct_type, ..) => {
//                 for f in struct_type.fields.iter_mut() {
//                     f.ty.substitute(tycx);
//                 }
//             }

//             ast::ExprKind::FnType(proto) => {
//                 proto.substitute(tycx);
//             }

//             ast::ExprKind::Id { .. }
//             | ast::ExprKind::Literal(_)
//             | ast::ExprKind::SelfType
//             | ast::ExprKind::NeverType
//             | ast::ExprKind::UnitType
//             | ast::ExprKind::PlaceholderType
//             | ast::ExprKind::Noop => (),
//         }

//         self.ty = substitute_ty(&self.ty, tycx);
//     }
// }

// pub(crate) fn substitute_ty(ty: &Ty, tycx: &TyContext) -> Ty {
//     match ty {
//         Ty::Var(var) => find_type_or_default(*var, tycx, || {
//             panic!(
//                 "couldn't figure out the type of {}, because it was unbound",
//                 var
//             )
//         }),
//         Ty::Fn(f) => Ty::Fn(FnTy {
//             params: f
//                 .params
//                 .iter()
//                 .map(|p| FnTyParam {
//                     symbol: p.symbol,
//                     ty: substitute_ty(&p.ty, tycx),
//                 })
//                 .collect(),
//             ret: Box::new(substitute_ty(&f.ret, tycx)),
//             variadic: f.variadic,
//             lib_name: f.lib_name,
//         }),
//         Ty::Pointer(ty, a) => Ty::Pointer(Box::new(substitute_ty(ty, tycx)), *a),
//         Ty::MultiPointer(ty, a) => Ty::MultiPointer(Box::new(substitute_ty(ty, tycx)), *a),
//         Ty::Array(ty, a) => Ty::Array(Box::new(substitute_ty(ty, tycx)), *a),
//         Ty::Slice(ty, a) => Ty::Slice(Box::new(substitute_ty(ty, tycx)), *a),
//         Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|ty| substitute_ty(&ty, tycx)).collect()),
//         Ty::Struct(st) => Ty::Struct(StructTy {
//             name: st.name,
//             qualified_name: st.qualified_name,
//             binding_info_id: st.binding_info_id,
//             fields: st
//                 .fields
//                 .iter()
//                 .map(|f| StructTyField {
//                     symbol: f.symbol,
//                     ty: substitute_ty(&f.ty, tycx),
//                     span: f.span,
//                 })
//                 .collect(),
//             kind: st.kind,
//         }),
//         Ty::AnyInt(var) => find_type_or_default(*var, tycx, || Ty::Int(IntTy::default())),
//         Ty::AnyFloat(var) => find_type_or_default(*var, tycx, || Ty::Float(FloatTy::default())),
//         _ => ty.clone(),
//     }
// }

// fn find_type_or_default<F: FnOnce() -> Ty>(var: TyId, tycx: &TyContext, default: F) -> Ty {
//     match tycx.find_type_binding(var) {
//         TyBinding::Bound(ty) => substitute_ty(&ty, tycx),
//         TyBinding::Unbound => default(),
//     }
// }
