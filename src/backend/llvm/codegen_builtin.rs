use super::{
    codegen::{Codegen, FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{
    ast, hir,
    infer::{display::DisplayTy, normalize::Normalize},
    span::Span,
    types::*,
};
use inkwell::{
    types::IntType,
    values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode, IntValue},
    FloatPredicate, IntPredicate,
};

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Builtin {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            hir::Builtin::Add(binary) => {
                let (lhs, rhs, ty) = gen_binary(binary, generator, state);
                generator.gen_add(state, lhs, rhs, ty, binary.span)
            }
            hir::Builtin::Sub(binary) => {
                let (lhs, rhs, ty) = gen_binary(binary, generator, state);
                generator.gen_sub(state, lhs, rhs, ty, binary.span)
            }
            hir::Builtin::Mul(binary) => {
                let (lhs, rhs, ty) = gen_binary(binary, generator, state);
                generator.gen_mul(state, lhs, rhs, ty, binary.span)
            }
            hir::Builtin::Div(binary) => {
                let (lhs, rhs, ty) = gen_binary(binary, generator, state);
                generator.gen_div(state, lhs, rhs, ty, binary.span)
            }
            hir::Builtin::Rem(binary) => {
                let (lhs, rhs, ty) = gen_binary(binary, generator, state);
                generator.gen_rem(state, lhs, rhs, ty, binary.span)
            }
            hir::Builtin::Shl(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_shl(lhs, rhs)
            }
            hir::Builtin::Shr(binary) => {
                let (lhs, rhs, ty) = gen_binary(binary, generator, state);
                generator.gen_shr(lhs, rhs, ty)
            }
            hir::Builtin::And(binary) => {
                // let (lhs, rhs, _) = gen_binary(binary, generator, state);
                // generator.gen_and(lhs, rhs)
                generator.gen_conditional(
                    state,
                    |generator, state| binary.lhs.codegen(generator, state).into_int_value(),
                    |generator, state: &mut FunctionState<'ctx>| binary.rhs.codegen(generator, state),
                    Some(|generator: &mut Generator<'g, 'ctx>, _: &mut FunctionState<'ctx>| {
                        generator.context.bool_type().const_int(0, false).into()
                    }),
                )
            }
            hir::Builtin::Or(binary) => generator.gen_conditional(
                state,
                |generator, state| binary.lhs.codegen(generator, state).into_int_value(),
                |generator, _| generator.context.bool_type().const_int(1, false).into(),
                Some(|generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
                    binary.rhs.codegen(generator, state)
                }),
            ),
            hir::Builtin::Lt(binary) => gen_cmp(
                binary,
                IntPredicate::SLT,
                IntPredicate::ULT,
                FloatPredicate::OLT,
                generator,
                state,
            ),
            hir::Builtin::Le(binary) => gen_cmp(
                binary,
                IntPredicate::SLE,
                IntPredicate::ULE,
                FloatPredicate::OLE,
                generator,
                state,
            ),
            hir::Builtin::Gt(binary) => gen_cmp(
                binary,
                IntPredicate::SGT,
                IntPredicate::UGT,
                FloatPredicate::OGT,
                generator,
                state,
            ),
            hir::Builtin::Ge(binary) => gen_cmp(
                binary,
                IntPredicate::SGE,
                IntPredicate::UGE,
                FloatPredicate::OGE,
                generator,
                state,
            ),
            hir::Builtin::Eq(binary) => gen_cmp(
                binary,
                IntPredicate::EQ,
                IntPredicate::EQ,
                FloatPredicate::OEQ,
                generator,
                state,
            ),
            hir::Builtin::Ne(binary) => gen_cmp(
                binary,
                IntPredicate::NE,
                IntPredicate::NE,
                FloatPredicate::ONE,
                generator,
                state,
            ),
            hir::Builtin::BitAnd(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_and(lhs, rhs)
            }
            hir::Builtin::BitOr(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_or(lhs, rhs)
            }
            hir::Builtin::BitXor(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_xor(lhs, rhs)
            }
            hir::Builtin::Not(unary) => {
                let value = unary.value.codegen(generator, state).into_int_value();

                match unary.ty.normalize(generator.tcx) {
                    Type::Int(_) | Type::Uint(_) => generator.builder.build_not(value, "not").into(),
                    Type::Bool => {
                        let false_value = generator.context.custom_width_int_type(1).const_zero();

                        generator
                            .builder
                            .build_int_compare(IntPredicate::EQ, value, false_value, "bnot")
                            .into()
                    }
                    _ => unreachable!(),
                }
            }
            hir::Builtin::Neg(unary) => match unary.ty.normalize(generator.tcx) {
                Type::Int(_) => {
                    let value = unary.value.codegen(generator, state).into_int_value();
                    generator.builder.build_int_neg(value, "ineg").into()
                }
                Type::Float(_) => {
                    let value = unary.value.codegen(generator, state).into_float_value();
                    generator.builder.build_float_neg(value, "fneg").into()
                }
                _ => unreachable!("{}", &unary.value.ty().display(&generator.tcx)),
            },
            hir::Builtin::Deref(unary) => {
                let value = unary.value.codegen(generator, state);

                match value.as_instruction_value() {
                    Some(inst) if inst.get_opcode() == InstructionOpcode::Load => value,
                    _ => {
                        let ptr = value.into_pointer_value();
                        generator.gen_runtime_check_null_pointer_deref(state, ptr, unary.span);
                        generator.builder.build_load(ptr, "deref")
                    }
                }
            }
            hir::Builtin::Ref(ref_) => ref_.codegen(generator, state),
            hir::Builtin::Offset(offset) => offset.codegen(generator, state),
            hir::Builtin::Slice(slice) => slice.codegen(generator, state),
        }
    }
}

fn gen_binary<'g, 'ctx>(
    binary: &hir::Binary,
    generator: &mut Generator<'g, 'ctx>,
    state: &mut FunctionState<'ctx>,
) -> (BasicValueEnum<'ctx>, BasicValueEnum<'ctx>, Type) {
    let ty = binary.lhs.ty().normalize(generator.tcx);

    let lhs = binary.lhs.codegen(generator, state);
    let rhs = binary.rhs.codegen(generator, state);

    (lhs, rhs, ty)
}

fn gen_cmp<'g, 'ctx>(
    binary: &hir::Binary,
    int_predicate: IntPredicate,
    uint_predicate: IntPredicate,
    float_predicate: FloatPredicate,
    generator: &mut Generator<'g, 'ctx>,
    state: &mut FunctionState<'ctx>,
) -> BasicValueEnum<'ctx> {
    let (lhs, rhs, ty) = gen_binary(binary, generator, state);

    match ty {
        Type::Int(_) | Type::Uint(_) => generator
            .builder
            .build_int_compare(
                if ty.is_signed_int() {
                    int_predicate
                } else {
                    uint_predicate
                },
                lhs.into_int_value(),
                rhs.into_int_value(),
                "",
            )
            .into(),
        Type::Float(_) => generator
            .builder
            .build_float_compare(float_predicate, lhs.into_float_value(), rhs.into_float_value(), "")
            .into(),
        Type::Pointer(..) => {
            let lhs = generator
                .builder
                .build_ptr_to_int(lhs.into_pointer_value(), generator.ptr_sized_int_type, "");

            let rhs = generator
                .builder
                .build_ptr_to_int(rhs.into_pointer_value(), generator.ptr_sized_int_type, "");

            generator.builder.build_int_compare(int_predicate, lhs, rhs, "").into()
        }
        ty => panic!("unexpected type: {}", ty),
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Slice {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        let value_type = self.value.ty().normalize(generator.tcx);

        let sliced_value = match &value_type {
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(..) => generator.gep_slice_data(value).as_basic_value_enum(),
                _ => value,
            },
            Type::Array(..) => value,
            _ => unreachable!("got {}", value_type),
        };

        let low = self.low.codegen(generator, state).into_int_value();
        let high = self.high.codegen(generator, state).into_int_value();

        generator.gen_runtime_check_slice_end_before_start(state, low, high, self.value.span());

        let len = match &value_type {
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(..) => Some(generator.gep_slice_len(value)),
                _ => None,
            },
            Type::Array(_, size) => Some(generator.ptr_sized_int_type.const_int(*size as _, false)),
            _ => unreachable!("got {}", value_type),
        };

        if let Some(len) = len {
            generator.gen_runtime_check_slice_range_out_of_bounds(state, low, high, len, self.value.span());
        }

        let slice_llvm_type = self.ty.llvm_type(generator);
        let slice_ptr = generator.build_alloca(state, slice_llvm_type);

        generator.build_slice(slice_ptr, sliced_value, low, high, value_type.element_type().unwrap());

        generator.builder.build_load(slice_ptr, "")
    }
}

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
                    self.builder.build_int_add(lhs, rhs, "add").into()
                } else {
                    let overflow_fn = self.get_overflow_fn(ast::BinaryOp::Add, ty, lhs.get_type());
                    let result = self.gen_call_overflow_fn(state, overflow_fn, lhs, rhs, span, "add");
                    result.into()
                }
            }
            Type::Float(_) => self
                .builder
                .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "fadd")
                .into(),
            Type::Pointer(..) => unsafe {
                self.builder
                    .build_gep(lhs.into_pointer_value(), &[rhs.into_int_value()], "padd")
                    .into()
            },
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
                    self.builder.build_int_sub(lhs, rhs, "sub").into()
                } else {
                    let overflow_fn = self.get_overflow_fn(ast::BinaryOp::Sub, ty, lhs.get_type());
                    let result = self.gen_call_overflow_fn(state, overflow_fn, lhs, rhs, span, "subtract");
                    result.into()
                }
            }
            Type::Float(_) => self
                .builder
                .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "fsub")
                .into(),
            Type::Pointer(..) => unsafe {
                self.builder
                    .build_gep(lhs.into_pointer_value(), &[rhs.into_int_value()], "psub")
                    .into()
            },
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
                    let result = self.gen_call_overflow_fn(state, overflow_fn, lhs, rhs, span, "multiply");
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

    pub(super) fn gen_and(&mut self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_and(lhs.into_int_value(), rhs.into_int_value(), "iand")
            .into()
    }

    pub(super) fn gen_or(&mut self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_or(lhs.into_int_value(), rhs.into_int_value(), "ior")
            .into()
    }

    pub(super) fn gen_shl(&mut self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
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
            .build_right_shift(lhs.into_int_value(), rhs.into_int_value(), ty.is_signed_int(), "ishr")
            .into()
    }

    pub(super) fn gen_xor(&mut self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
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
        let overflow_fn_return_type = self
            .context
            .struct_type(&[operand_type.into(), self.context.bool_type().into()], false);

        let overflow_fn_type = overflow_fn_return_type.fn_type(&[operand_type.into(), operand_type.into()], false);

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

        let result = self.gep_struct(return_value, 0, "result");
        let overflow_bit = self.gep_struct(return_value, 1, "overflow_bit");

        self.gen_runtime_check_overflow(state, overflow_bit.into_int_value(), span, op);

        result.into_int_value()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Ref {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);

        match value.as_instruction_value() {
            Some(inst) => {
                match inst.get_opcode() {
                    InstructionOpcode::Load => inst.get_operand(0).unwrap().left().unwrap(),
                    // Note (Ron): Alloca is matched for DST's.
                    // This could be wrong, but I haven't encountered any issues with this yet
                    // InstructionOpcode::Alloca => value,
                    _ => panic!("{:?}", value),
                }
            }
            None => panic!("{:?}", value),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Offset {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        let index = self.index.codegen(generator, state).into_int_value();

        let ty = self.value.ty().normalize(generator.tcx);

        let len = match &ty {
            Type::Array(_, size) => Some(index.get_type().const_int(*size as _, false)),
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Array(_, size) => Some(index.get_type().const_int(*size as _, false)),
                Type::Slice(..) => Some(generator.gep_slice_len(value)),
                _ => None,
            },
            ty => unreachable!("got {}", ty),
        };

        if let Some(len) = len {
            generator.gen_runtime_check_index_out_of_bounds(state, index, len, self.span);
        }

        let ptr = match &ty {
            Type::Array(..) => value.into_pointer_value(),
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Array(..) => value.into_pointer_value(),
                Type::Slice(..) => generator.gep_slice_data(value),
                _ => value.into_pointer_value(),
            },
            ty => unreachable!("{}", ty),
        };

        let ptr_offset = match &ty {
            Type::Array(..) => unsafe {
                generator
                    .builder
                    .build_in_bounds_gep(ptr, &[index.get_type().const_zero(), index], "offset")
            },
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Array(..) => unsafe {
                    generator
                        .builder
                        .build_in_bounds_gep(ptr, &[index.get_type().const_zero(), index], "offset")
                },
                Type::Slice(..) => unsafe { generator.builder.build_in_bounds_gep(ptr, &[index], "offset") },
                _ => unsafe { generator.builder.build_in_bounds_gep(ptr, &[index], "offset") },
            },
            ty => unreachable!("{}", ty),
        };

        generator.builder.build_load(ptr_offset, "")
    }
}
