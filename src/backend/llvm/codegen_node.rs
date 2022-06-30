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

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Node {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        match self {
            hir::Node::Const(x) => x.codegen(generator, state),
            hir::Node::Binding(x) => x.codegen(generator, state),
            hir::Node::Id(x) => x.codegen(generator, state),
            hir::Node::Assignment(x) => x.codegen(generator, state),
            hir::Node::MemberAccess(x) => x.codegen(generator, state),
            hir::Node::Call(x) => x.codegen(generator, state),
            hir::Node::Cast(x) => x.codegen(generator, state),
            hir::Node::Sequence(x) => x.codegen(generator, state),
            hir::Node::Control(x) => x.codegen(generator, state),
            hir::Node::Builtin(x) => x.codegen(generator, state),
            hir::Node::Literal(x) => x.codegen(generator, state),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Const {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Binding {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        // binding globally/locally based on binding_info.scope_level()
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Id {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Assignment {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::MemberAccess {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Call {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Cast {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Sequence {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
