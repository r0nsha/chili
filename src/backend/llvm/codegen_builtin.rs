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
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
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
            hir::Builtin::Add(_) => todo!(),
            hir::Builtin::Sub(_) => todo!(),
            hir::Builtin::Mul(_) => todo!(),
            hir::Builtin::Div(_) => todo!(),
            hir::Builtin::Rem(_) => todo!(),
            hir::Builtin::Shl(_) => todo!(),
            hir::Builtin::Shr(_) => todo!(),
            hir::Builtin::And(_) => todo!(),
            hir::Builtin::Or(_) => todo!(),
            hir::Builtin::Lt(_) => todo!(),
            hir::Builtin::Le(_) => todo!(),
            hir::Builtin::Gt(_) => todo!(),
            hir::Builtin::Ge(_) => todo!(),
            hir::Builtin::Eq(_) => todo!(),
            hir::Builtin::Ne(_) => todo!(),
            hir::Builtin::BitAnd(_) => todo!(),
            hir::Builtin::BitOr(_) => todo!(),
            hir::Builtin::BitXor(_) => todo!(),
            hir::Builtin::Not(_) => todo!(),
            hir::Builtin::Neg(_) => todo!(),
            hir::Builtin::Deref(_) => todo!(),
            hir::Builtin::Ref(_) => todo!(),
            hir::Builtin::Offset(_) => todo!(),
            hir::Builtin::Slice(slice) => slice.codegen(generator, state),
        }
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
