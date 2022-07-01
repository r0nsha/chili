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

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Control {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match self {
            hir::Control::If(x) => x.codegen(generator, state),
            hir::Control::While(x) => x.codegen(generator, state),
            hir::Control::Return(x) => x.codegen(generator, state),
            hir::Control::Break(_) => todo!(),
            hir::Control::Continue(_) => todo!(),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::If {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let condition = self.condition.codegen(generator, state).into_int_value();

        let then = |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
            self.then.codegen(generator, state)
        };

        let otherwise = if let Some(otherwise) = &self.otherwise {
            Some(
                |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
                    otherwise.codegen(generator, state)
                },
            )
        } else {
            None
        };

        generator.gen_conditional(state, condition, then, otherwise)
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::While {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Return {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
