// use crate::{builtin::get_ty_for_builtin_type, tycx::TyCtx};
// use chili_ast::{
//     ast,
//     workspace::{BindingInfo, BindingInfoFlags},
// };

// pub(crate) trait Annotate {
//     fn annotate(&mut self, tycx: &mut TyCtx);
// }

// impl<T: Annotate> Annotate for Box<T> {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.as_mut().annotate(tycx)
//     }
// }

// impl<T: Annotate> Annotate for Option<T> {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         if let Some(s) = self {
//             s.annotate(tycx)
//         }
//     }
// }

// impl<T: Annotate> Annotate for Vec<T> {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.iter_mut().for_each(|e| e.annotate(tycx))
//     }
// }

// impl Annotate for BindingInfo {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.ty = if self.flags.contains(BindingInfoFlags::BUILTIN_TYPE) {
//             get_ty_for_builtin_type(self.symbol, tycx)
//         } else {
//             tycx.new_variable()
//         };
//     }
// }

// impl Annotate for ast::Ast {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.bindings.annotate(tycx);
//     }
// }

// impl Annotate for ast::Binding {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.ty_expr.annotate(tycx);
//         self.expr.annotate(tycx);
//     }
// }

// impl Annotate for ast::Fn {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.proto.annotate(tycx);
//         self.body.annotate(tycx);
//     }
// }

// impl Annotate for ast::Proto {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.params.iter_mut().for_each(|p| p.ty.annotate(tycx));

//         self.ret.annotate(tycx);
//     }
// }

// impl Annotate for ast::Block {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.exprs.annotate(tycx);
//         self.deferred.annotate(tycx);
//     }
// }

// impl Annotate for ast::Expr {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.ty = tycx.new_variable();

//         match &mut self.kind {
//             ast::ExprKind::Foreign(bindings) => bindings.annotate(tycx),
//             ast::ExprKind::Binding(binding) => binding.annotate(tycx),
//             ast::ExprKind::Defer(deferred) => deferred.annotate(tycx),
//             ast::ExprKind::Assign { lvalue, rvalue } => {
//                 lvalue.annotate(tycx);
//                 rvalue.annotate(tycx);
//             }
//             ast::ExprKind::Cast(cast) => cast.annotate(tycx),
//             ast::ExprKind::Builtin(builtin) => match builtin {
//                 ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.annotate(tycx),
//                 ast::Builtin::Panic(expr) => expr.annotate(tycx),
//             },
//             ast::ExprKind::Fn(f) => f.annotate(tycx),
//             ast::ExprKind::While { cond, block } => {
//                 cond.annotate(tycx);
//                 block.annotate(tycx);
//             }
//             ast::ExprKind::For(for_) => {
//                 match &mut for_.iterator {
//                     ast::ForIter::Range(start, end) => {
//                         start.annotate(tycx);
//                         end.annotate(tycx);
//                     }
//                     ast::ForIter::Value(value) => {
//                         value.annotate(tycx);
//                     }
//                 };
//                 for_.block.annotate(tycx);
//             }
//             ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
//                 deferred.annotate(tycx)
//             }
//             ast::ExprKind::Return { expr, deferred } => {
//                 expr.annotate(tycx);
//                 deferred.annotate(tycx);
//             }
//             ast::ExprKind::If {
//                 cond,
//                 then_expr,
//                 else_expr,
//             } => {
//                 cond.annotate(tycx);
//                 then_expr.annotate(tycx);
//                 else_expr.annotate(tycx);
//             }
//             ast::ExprKind::Block(block) => block.annotate(tycx),
//             ast::ExprKind::Binary { lhs, op: _, rhs } => {
//                 lhs.annotate(tycx);
//                 rhs.annotate(tycx);
//             }
//             ast::ExprKind::Unary { op: _, lhs } => lhs.annotate(tycx),
//             ast::ExprKind::Subscript { expr, index } => {
//                 expr.annotate(tycx);
//                 index.annotate(tycx);
//             }
//             ast::ExprKind::Slice { expr, low, high } => {
//                 expr.annotate(tycx);
//                 low.annotate(tycx);
//                 high.annotate(tycx);
//             }
//             ast::ExprKind::Call(call) => call.annotate(tycx),
//             ast::ExprKind::MemberAccess { expr, member: _ } => expr.annotate(tycx),
//             ast::ExprKind::Id { .. } => (),
//             ast::ExprKind::ArrayLiteral(lit) => match lit {
//                 ast::ArrayLiteralKind::List(elements) => {
//                     elements.annotate(tycx);
//                 }
//                 ast::ArrayLiteralKind::Fill { len, expr } => {
//                     len.annotate(tycx);
//                     expr.annotate(tycx);
//                 }
//             },
//             ast::ExprKind::TupleLiteral(elements) => elements.annotate(tycx),
//             ast::ExprKind::StructLiteral { type_expr, fields } => {
//                 type_expr.annotate(tycx);

//                 fields.iter_mut().for_each(|f| f.value.annotate(tycx));
//             }
//             ast::ExprKind::PointerType(inner, _)
//             | ast::ExprKind::MultiPointerType(inner, _)
//             | ast::ExprKind::SliceType(inner, _) => inner.annotate(tycx),
//             ast::ExprKind::ArrayType(inner, size) => {
//                 inner.annotate(tycx);
//                 size.annotate(tycx);
//             }

//             ast::ExprKind::StructType(st) => {
//                 st.fields.iter_mut().for_each(|f| f.ty.annotate(tycx));
//             }
//             ast::ExprKind::FnType(proto) => {
//                 proto.annotate(tycx);
//             }
//             ast::ExprKind::Import(_)
//             | ast::ExprKind::Literal(_)
//             | ast::ExprKind::SelfType
//             | ast::ExprKind::NeverType
//             | ast::ExprKind::UnitType
//             | ast::ExprKind::PlaceholderType
//             | ast::ExprKind::Noop => (),
//         }
//     }
// }

// impl Annotate for ast::Cast {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.expr.annotate(tycx);
//         self.ty_expr.annotate(tycx);
//     }
// }

// impl Annotate for ast::Call {
//     fn annotate(&mut self, tycx: &mut TyCtx) {
//         self.callee.annotate(tycx);
//         self.args.iter_mut().for_each(|a| a.expr.annotate(tycx));
//     }
// }
