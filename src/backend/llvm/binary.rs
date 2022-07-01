use super::codegen::{FunctionState, Generator};
use crate::{ast, infer::normalize::Normalize, span::Span, types::*};
use inkwell::{
    types::IntType,
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue},
    FloatPredicate, IntPredicate,
};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn gen_add(
        &mut self,
        state: &mut FunctionState<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        ty: Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        match &ty {
            Type::Int(_) | Type::Uint(_) => {
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();

                if self.workspace.build_options.optimization_level.is_release() {
                    self.builder.build_int_add(lhs, rhs, "iadd").into()
                } else {
                    let overflow_fn = self.get_overflow_fn(ast::BinaryOp::Add, ty, lhs.get_type());
                    let result =
                        self.gen_call_overflow_fn(state, overflow_fn, lhs, rhs, span, "add");
                    result.into()
                }
            }
            Type::Float(_) => self
                .builder
                .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "fadd")
                .into(),
            _ => panic!("unexpected type `{}`", ty),
        }
    }

    pub(super) fn gen_sub(
        &mut self,
        state: &mut FunctionState<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        ty: Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        match &ty {
            Type::Int(_) | Type::Uint(_) => {
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();

                if self.workspace.build_options.optimization_level.is_release() {
                    self.builder.build_int_sub(lhs, rhs, "isub").into()
                } else {
                    let overflow_fn = self.get_overflow_fn(ast::BinaryOp::Sub, ty, lhs.get_type());
                    let result =
                        self.gen_call_overflow_fn(state, overflow_fn, lhs, rhs, span, "subtract");
                    result.into()
                }
            }
            Type::Float(_) => self
                .builder
                .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "fsub")
                .into(),
            _ => panic!("unexpected type `{}`", ty),
        }
    }

    pub(super) fn gen_mul(
        &mut self,
        state: &mut FunctionState<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        ty: Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        match &ty {
            Type::Int(_) | Type::Uint(_) => {
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();

                if self.workspace.build_options.optimization_level.is_release() {
                    self.builder.build_int_mul(lhs, rhs, "imul").into()
                } else {
                    let overflow_fn = self.get_overflow_fn(ast::BinaryOp::Mul, ty, lhs.get_type());
                    let result =
                        self.gen_call_overflow_fn(state, overflow_fn, lhs, rhs, span, "multiply");
                    result.into()
                }
            }
            Type::Float(_) => self
                .builder
                .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "fmul")
                .into(),
            _ => panic!("unexpected type `{}`", ty),
        }
    }

    pub(super) fn gen_div(
        &mut self,
        state: &mut FunctionState<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        ty: Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        match &ty {
            Type::Int(_) => {
                self.gen_runtime_check_division_by_zero(state, rhs.into_int_value(), span);
                self.builder
                    .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "idiv")
                    .into()
            }
            Type::Uint(_) => {
                self.gen_runtime_check_division_by_zero(state, rhs.into_int_value(), span);
                self.builder
                    .build_int_unsigned_div(lhs.into_int_value(), rhs.into_int_value(), "udiv")
                    .into()
            }
            Type::Float(_) => self
                .builder
                .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "fdiv")
                .into(),
            _ => panic!("unexpected type `{}`", ty),
        }
    }

    pub(super) fn gen_rem(
        &mut self,
        state: &mut FunctionState<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        ty: Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        match &ty {
            Type::Int(_) => {
                self.gen_runtime_check_division_by_zero(state, rhs.into_int_value(), span);
                self.builder
                    .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "irem")
                    .into()
            }
            Type::Uint(_) => {
                self.gen_runtime_check_division_by_zero(state, rhs.into_int_value(), span);
                self.builder
                    .build_int_unsigned_rem(lhs.into_int_value(), rhs.into_int_value(), "urem")
                    .into()
            }
            Type::Float(_) => self
                .builder
                .build_float_rem(lhs.into_float_value(), rhs.into_float_value(), "frem")
                .into(),
            _ => panic!("unexpected type `{}`", ty),
        }
    }

    pub(super) fn gen_and(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_and(lhs.into_int_value(), rhs.into_int_value(), "iand")
            .into()
    }

    pub(super) fn gen_or(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_or(lhs.into_int_value(), rhs.into_int_value(), "ior")
            .into()
    }

    pub(super) fn gen_shl(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "ishl")
            .into()
    }

    pub(super) fn gen_shr(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        ty: Type,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_right_shift(
                lhs.into_int_value(),
                rhs.into_int_value(),
                ty.is_signed_int(),
                "ishr",
            )
            .into()
    }

    pub(super) fn gen_xor(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "ixor")
            .into()
    }

    pub(super) fn get_overflow_fn(
        &mut self,
        op: ast::BinaryOp,
        ty: Type,
        operand_type: IntType<'ctx>,
    ) -> FunctionValue<'ctx> {
        let overflow_fn_return_type = self.context.struct_type(
            &[operand_type.into(), self.context.bool_type().into()],
            false,
        );

        let overflow_fn_type =
            overflow_fn_return_type.fn_type(&[operand_type.into(), operand_type.into()], false);

        let llvm_op = format!(
            "{}{}",
            if ty.is_signed_int() { "s" } else { "u" },
            match op {
                ast::BinaryOp::Add => "add",
                ast::BinaryOp::Sub => "sub",
                ast::BinaryOp::Mul => "mul",
                _ => panic!(),
            }
        );

        let llvm_type_name = format!("i{}", operand_type.get_bit_width());

        self.get_or_add_function(
            format!("llvm.{}.with.overflow.{}", llvm_op, llvm_type_name),
            overflow_fn_type,
            None,
        )
    }

    pub(super) fn gen_call_overflow_fn(
        &mut self,
        state: &mut FunctionState<'ctx>,
        f: FunctionValue<'ctx>,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        span: Span,
        op: &str,
    ) -> IntValue<'ctx> {
        let call_value = self.builder.build_call(f, &[lhs.into(), rhs.into()], "");

        let return_value = call_value.try_as_basic_value().left().unwrap();

        let result = self.gen_struct_access(return_value, 0, None);

        let overflow_bit = self.gen_struct_access(return_value, 1, None);

        self.gen_runtime_check_overflow(state, overflow_bit.into_int_value(), span, op);

        result.into_int_value()
    }
}

trait IntoIntPredicate {
    fn into_int_predicate(self, is_signed: bool) -> IntPredicate;
}

impl IntoIntPredicate for ast::BinaryOp {
    fn into_int_predicate(self, is_signed: bool) -> IntPredicate {
        match self {
            ast::BinaryOp::Eq => IntPredicate::EQ,
            ast::BinaryOp::Ne => IntPredicate::NE,
            ast::BinaryOp::Lt => {
                if is_signed {
                    IntPredicate::SLT
                } else {
                    IntPredicate::ULT
                }
            }
            ast::BinaryOp::Le => {
                if is_signed {
                    IntPredicate::SLE
                } else {
                    IntPredicate::ULE
                }
            }
            ast::BinaryOp::Gt => {
                if is_signed {
                    IntPredicate::SGT
                } else {
                    IntPredicate::UGT
                }
            }
            ast::BinaryOp::Ge => {
                if is_signed {
                    IntPredicate::SGE
                } else {
                    IntPredicate::UGE
                }
            }
            _ => panic!("got {}", self),
        }
    }
}

trait IntoFloatPredicate {
    fn into_float_predicate(self) -> FloatPredicate;
}

impl IntoFloatPredicate for ast::BinaryOp {
    fn into_float_predicate(self) -> FloatPredicate {
        match self {
            ast::BinaryOp::Eq => FloatPredicate::OEQ,
            ast::BinaryOp::Ne => FloatPredicate::ONE,
            ast::BinaryOp::Lt => FloatPredicate::OLT,
            ast::BinaryOp::Le => FloatPredicate::OLE,
            ast::BinaryOp::Gt => FloatPredicate::OGT,
            ast::BinaryOp::Ge => FloatPredicate::OGE,
            _ => panic!("got {}", self),
        }
    }
}
