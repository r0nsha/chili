use crate::codegen::{Codegen, CodegenState};
use chili_ast::ast::{Expr, UnaryOp};
use chili_span::Span;
use chili_ast::ty::*;
use inkwell::{values::BasicValueEnum, IntPredicate};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_unary(
        &mut self,
        state: &mut CodegenState<'ctx>,
        op: &UnaryOp,
        lhs: &Box<Expr>,
        span: Span,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        match op {
            UnaryOp::Ref(_) => self.gen_expr(state, &lhs, false),
            UnaryOp::Deref => {
                let ptr = self.gen_expr(state, lhs, true);
                self.gen_runtime_check_null_pointer_deref(
                    state,
                    ptr.into_pointer_value(),
                    span,
                );
                if deref {
                    self.build_load(ptr)
                } else {
                    ptr
                }
            }
            UnaryOp::Neg => match &lhs.ty {
                TyKind::Int(_) => self
                    .builder
                    .build_int_neg(
                        self.gen_expr(state, lhs, true).into_int_value(),
                        "sneg",
                    )
                    .into(),
                TyKind::Float(_) => self
                    .builder
                    .build_float_neg(
                        self.gen_expr(state, lhs, true).into_float_value(),
                        "fneg",
                    )
                    .into(),
                _ => unreachable!("{}", lhs.ty),
            },
            UnaryOp::Plus => self.gen_expr(state, lhs, true),
            UnaryOp::Not => match &lhs.ty {
                TyKind::Pointer(_, _) => {
                    let value = self.gen_expr(state, lhs, true);

                    self.builder
                        .build_is_null(value.into_pointer_value(), "ptr_is_nil")
                        .into()
                }
                TyKind::Bool => self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        self.gen_expr(state, lhs, true).into_int_value(),
                        self.context.custom_width_int_type(1).const_zero(),
                        "bnot",
                    )
                    .into(),
                _ => unreachable!(),
            },
            UnaryOp::BitwiseNot => {
                let value = self.gen_expr(state, lhs, true).into_int_value();
                self.builder.build_not(value, "bwnot").into()
            }
        }
    }
}
