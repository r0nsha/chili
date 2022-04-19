use crate::codegen::{Codegen, CodegenState};
use chili_ast::{ast, ty::*};
use chili_infer::normalize::NormalizeTy;
use chili_span::Span;
use inkwell::{values::BasicValueEnum, IntPredicate};

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_unary(
        &mut self,
        state: &mut CodegenState<'ctx>,
        unary: &ast::Unary,
        span: Span,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        let ty = unary.lhs.ty.normalize(self.tycx);
        match unary.op {
            ast::UnaryOp::Ref(_) => self.gen_expr(state, &unary.lhs, false),
            ast::UnaryOp::Deref => {
                let ptr = self.gen_expr(state, &unary.lhs, true);
                self.gen_runtime_check_null_pointer_deref(state, ptr.into_pointer_value(), span);
                if deref {
                    self.build_load(ptr)
                } else {
                    ptr
                }
            }
            ast::UnaryOp::Neg => match ty {
                TyKind::Int(_) => self
                    .builder
                    .build_int_neg(
                        self.gen_expr(state, &unary.lhs, true).into_int_value(),
                        "sneg",
                    )
                    .into(),
                TyKind::Float(_) => self
                    .builder
                    .build_float_neg(
                        self.gen_expr(state, &unary.lhs, true).into_float_value(),
                        "fneg",
                    )
                    .into(),
                _ => unreachable!("{}", &unary.lhs.ty),
            },
            ast::UnaryOp::Plus => self.gen_expr(state, &unary.lhs, true),
            ast::UnaryOp::Not => match ty {
                TyKind::Pointer(_, _) => {
                    let value = self.gen_expr(state, &unary.lhs, true);

                    self.builder
                        .build_is_null(value.into_pointer_value(), "ptr_is_nil")
                        .into()
                }
                TyKind::Bool => self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        self.gen_expr(state, &unary.lhs, true).into_int_value(),
                        self.context.custom_width_int_type(1).const_zero(),
                        "bnot",
                    )
                    .into(),
                _ => unreachable!(),
            },
            ast::UnaryOp::BitwiseNot => {
                let value = self.gen_expr(state, &unary.lhs, true).into_int_value();
                self.builder.build_not(value, "bwnot").into()
            }
        }
    }
}
