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
        state: &mut FunctionState,
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
            hir::Builtin::Slice(_) => todo!(),
        }
    }
}
