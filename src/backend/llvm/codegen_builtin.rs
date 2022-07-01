use super::{
    abi::{align_of, size_of},
    codegen::{Codegen, FunctionState, Generator},
    traits::IsALoadInst,
    ty::IntoLlvmType,
};
use crate::{
    ast::{
        self,
        pattern::{NamePattern, Pattern, UnpackPattern, UnpackPatternKind},
        FunctionId, Intrinsic,
    },
    common::{
        build_options,
        builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN},
        scopes::Scopes,
        target::TargetMetrics,
    },
    hir::{self, const_value::ConstValue},
    infer::{normalize::Normalize, ty_ctx::TyCtx},
    span::Span,
    types::*,
    workspace::{BindingId, BindingInfo, ModuleId, ModuleInfo, Workspace},
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::{PassManager, PassManagerBuilder},
    types::{BasicType, BasicTypeEnum, IntType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use std::collections::HashMap;
use ustr::{ustr, Ustr, UstrMap};

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Builtin {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        // let ty = binary.lhs.ty().normalize(self.tycx);

        // let lhs = self.gen_expr(state, &binary.lhs, true);
        // let rhs = self.gen_expr(state, &binary.rhs, true);

        // let (lhs, rhs) = if lhs.is_pointer_value() {
        //     (
        //         self.builder
        //             .build_ptr_to_int(lhs.into_pointer_value(), self.ptr_sized_int_type, "")
        //             .as_basic_value_enum(),
        //         self.builder
        //             .build_ptr_to_int(rhs.into_pointer_value(), self.ptr_sized_int_type, "")
        //             .as_basic_value_enum(),
        //     )
        // } else {
        //     (lhs, rhs)
        // };

        // match binary.op {
        //     ast::BinaryOp::Add => self.gen_add(state, lhs, rhs, ty, binary.span),
        //     ast::BinaryOp::Sub => self.gen_sub(state, lhs, rhs, ty, binary.span),
        //     ast::BinaryOp::Mul => self.gen_mul(state, lhs, rhs, ty, binary.span),
        //     ast::BinaryOp::Div => self.gen_div(state, lhs, rhs, ty, binary.span),
        //     ast::BinaryOp::Rem => self.gen_rem(state, lhs, rhs, ty, binary.span),
        //     ast::BinaryOp::Eq
        //     | ast::BinaryOp::Ne
        //     | ast::BinaryOp::Lt
        //     | ast::BinaryOp::Le
        //     | ast::BinaryOp::Gt
        //     | ast::BinaryOp::Ge => {
        //         if ty.is_float() {
        //             self.builder
        //                 .build_float_compare(
        //                     binary.op.into_float_predicate(),
        //                     lhs.into_float_value(),
        //                     rhs.into_float_value(),
        //                     "",
        //                 )
        //                 .into()
        //         } else {
        //             self.builder
        //                 .build_int_compare(
        //                     binary.op.into_int_predicate(ty.is_signed_int()),
        //                     lhs.into_int_value(),
        //                     rhs.into_int_value(),
        //                     "",
        //                 )
        //                 .into()
        //         }
        //     }
        //     ast::BinaryOp::And | ast::BinaryOp::BitAnd => self.gen_and(lhs, rhs),
        //     ast::BinaryOp::Or | ast::BinaryOp::BitOr => self.gen_or(lhs, rhs),
        //     ast::BinaryOp::Shl => self.gen_shl(lhs, rhs),
        //     ast::BinaryOp::Shr => self.gen_shr(lhs, rhs, ty),
        //     ast::BinaryOp::BitXor => self.gen_xor(lhs, rhs),
        // }

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
            hir::Builtin::And(binary) | hir::Builtin::BitAnd(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_and(lhs, rhs)
            }
            hir::Builtin::Or(binary) | hir::Builtin::BitOr(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_or(lhs, rhs)
            }
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
            hir::Builtin::BitXor(binary) => {
                let (lhs, rhs, _) = gen_binary(binary, generator, state);
                generator.gen_xor(lhs, rhs)
            }
            hir::Builtin::Not(_) => todo!(),
            hir::Builtin::Neg(_) => todo!(),
            hir::Builtin::Deref(_) => todo!(),
            hir::Builtin::Ref(_) => todo!(),
            hir::Builtin::Offset(_) => todo!(),
            hir::Builtin::Slice(slice) => slice.codegen(generator, state),
        }
    }
}

fn gen_binary<'g, 'ctx>(
    binary: &hir::Binary,
    generator: &mut Generator<'g, 'ctx>,
    state: &mut FunctionState<'ctx>,
) -> (BasicValueEnum<'ctx>, BasicValueEnum<'ctx>, Type) {
    let ty = binary.lhs.ty().normalize(generator.tycx);

    let lhs = binary.lhs.codegen(generator, state);
    let rhs = binary.rhs.codegen(generator, state);

    if lhs.is_pointer_value() {
        (
            generator
                .builder
                .build_ptr_to_int(lhs.into_pointer_value(), generator.ptr_sized_int_type, "")
                .as_basic_value_enum(),
            generator
                .builder
                .build_ptr_to_int(rhs.into_pointer_value(), generator.ptr_sized_int_type, "")
                .as_basic_value_enum(),
            ty,
        )
    } else {
        (lhs, rhs, ty)
    }
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

    if ty.is_float() {
        generator
            .builder
            .build_float_compare(
                float_predicate,
                lhs.into_float_value(),
                rhs.into_float_value(),
                "",
            )
            .into()
    } else {
        generator
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
            .into()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Slice {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        let value_type = self.value.ty().normalize(generator.tycx);

        let sliced_value = match value_type {
            Type::Slice(..) => generator.gep_slice_data(value).as_basic_value_enum(),
            Type::MultiPointer(..) | Type::Array(..) => value,
            _ => unreachable!("got {}", value_type),
        };

        let low = self.low.codegen(generator, state).into_int_value();
        let high = self.high.codegen(generator, state).into_int_value();

        generator.gen_runtime_check_slice_end_before_start(state, low, high, self.value.span());

        let len = match value_type {
            Type::Slice(..) => Some(generator.gep_slice_len(value)),
            Type::Array(_, size) => Some(generator.ptr_sized_int_type.const_int(size as _, false)),
            Type::MultiPointer(..) => None,
            _ => unreachable!("got {}", value_type),
        };

        if let Some(len) = len {
            generator.gen_runtime_check_slice_range_out_of_bounds(
                state,
                low,
                high,
                len,
                self.value.span(),
            );
        }

        let slice_llvm_ty = value_type.llvm_type(generator);
        let slice_ptr = generator.build_alloca(state, slice_llvm_ty);

        generator.build_slice(
            slice_ptr,
            sliced_value,
            low,
            high,
            value_type.element_type().unwrap(),
        );

        slice_ptr.into()
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
