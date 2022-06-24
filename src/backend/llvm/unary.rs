use super::codegen::{Codegen, CodegenState};
use crate::ast::{self, ty::*};
use crate::infer::normalize::Normalize;
use inkwell::{values::BasicValueEnum, IntPredicate};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub fn gen_unary(
        &mut self,
        state: &mut CodegenState<'ctx>,
        unary: &ast::Unary,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        let ty = unary.value.ty().normalize(self.tycx);
        match unary.op {
            ast::UnaryOp::Ref(_) => self.gen_expr(state, &unary.value, false),
            ast::UnaryOp::Deref => {
                let ptr = self.gen_expr(state, &unary.value, true);
                self.gen_runtime_check_null_pointer_deref(
                    state,
                    ptr.into_pointer_value(),
                    unary.span,
                );
                if deref {
                    self.build_load(ptr)
                } else {
                    ptr
                }
            }
            ast::UnaryOp::Neg => match ty {
                Type::Int(_) => self
                    .builder
                    .build_int_neg(
                        self.gen_expr(state, &unary.value, true).into_int_value(),
                        "sneg",
                    )
                    .into(),
                Type::Float(_) => self
                    .builder
                    .build_float_neg(
                        self.gen_expr(state, &unary.value, true).into_float_value(),
                        "fneg",
                    )
                    .into(),
                _ => unreachable!("{}", &unary.value.ty()),
            },
            ast::UnaryOp::Plus => self.gen_expr(state, &unary.value, true),
            ast::UnaryOp::Not => {
                let value = self.gen_expr(state, &unary.value, true).into_int_value();
                match ty {
                    Type::Int(_) | Type::Uint(_) => self.builder.build_not(value, "not").into(),
                    Type::Bool => self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            value,
                            self.context.custom_width_int_type(1).const_zero(),
                            "bnot",
                        )
                        .into(),
                    _ => unreachable!(),
                }
            }
        }
    }
}
