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
            hir::Builtin::Slice(slice) => {
                let slice_value = slice.value.codegen(generator, state);
                let ty = slice.value.ty().normalize(generator.tycx);

                let data = match ty {
                    Type::Slice(..) => generator.gep_slice_data(slice_value),
                    Type::MultiPointer(..) | Type::Array(..) => slice_value.into_pointer_value(),
                    _ => unreachable!("got {}", ty),
                };

                let low = slice.low.codegen(generator, state).into_int_value();
                let high = slice.high.codegen(generator, state).into_int_value();

                generator.gen_runtime_check_slice_end_before_start(
                    state,
                    low,
                    high,
                    slice.value.span(),
                );

                let len = match ty {
                    Type::Slice(..) => Some(generator.gep_slice_len(slice_value)),
                    Type::Array(_, size) => {
                        Some(generator.ptr_sized_int_type.const_int(size as _, false))
                    }
                    Type::MultiPointer(..) => None,
                    _ => unreachable!("got {}", ty),
                };

                if let Some(len) = len {
                    generator.gen_runtime_check_slice_range_out_of_bounds(
                        state,
                        low,
                        high,
                        len,
                        slice.value.span(),
                    );
                }

                let slice_llvm_ty = slice.value.ty().llvm_type(generator);
                let slice_ptr = generator.build_alloca(state, slice_llvm_ty);

                generator.build_slice(
                    slice_ptr,
                    data.into(),
                    low,
                    high,
                    ty.element_type().unwrap(),
                );

                slice_ptr.into()
            }
        }
    }
}
