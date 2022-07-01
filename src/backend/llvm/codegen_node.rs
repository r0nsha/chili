use super::{
    abi::{align_of, size_of},
    codegen::{Codegen, Decl, FunctionState, Generator},
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
    workspace::{BindingId, BindingInfo, ModuleId, ModuleInfo, ScopeLevel, Workspace},
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::{PassManager, PassManagerBuilder},
    types::{BasicType, BasicTypeEnum, IntType},
    values::{
        BasicValue, BasicValueEnum, CallableValue, FunctionValue, GlobalValue, InstructionOpcode,
        PointerValue,
    },
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::collections::HashMap;
use ustr::{ustr, Ustr, UstrMap};

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Node {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
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
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ty = self.ty.normalize(generator.tycx);
        generator.gen_const_value(Some(state), &self.value, &ty)
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Binding {
    fn codegen_global(&self, generator: &mut Generator<'g, 'ctx>) -> super::codegen::Decl<'ctx> {
        if let Some(decl) = generator.global_decls.get(&self.id) {
            *decl
        } else {
            // forward declare the global value, i.e: `let answer = 42`
            // the global value will is initialized by the entry point function
            let binding_info = generator.workspace.binding_infos.get(self.id).unwrap();

            let ty = binding_info.ty.normalize(generator.tycx);
            let llvm_type = ty.llvm_type(generator);

            let value = if let Some(const_value) = self.value.as_const_value() {
                generator.gen_const_value(None, const_value, &ty)
            } else {
                llvm_type.const_zero()
            };

            let global_value = generator.add_global_uninit(self.id, llvm_type, Linkage::Private);
            global_value.set_initializer(&value);

            generator.insert_global_decl(self.id, Decl::Global(global_value))
        }
    }

    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        generator.local_with_alloca(state, self.id, value);
        generator.unit_value()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Id {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let decl_ptr = match state.scopes.get(self.id) {
            Some((_, decl)) => decl.into_pointer_value(),
            None => generator
                .gen_top_level_binding(self.id)
                .into_pointer_value(),
        };

        generator.build_load(decl_ptr)
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Assignment {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let lhs = self.lhs.codegen(generator, state);

        let lhs = match lhs.as_instruction_value() {
            Some(inst) if inst.get_opcode() == InstructionOpcode::Load => inst
                .get_operand(0)
                .unwrap()
                .left()
                .unwrap()
                .into_pointer_value(),
            _ => lhs.into_pointer_value(),
        };

        let rhs = self.rhs.codegen(generator, state);

        let lhs = generator.builder.build_pointer_cast(
            lhs,
            rhs.get_type().ptr_type(AddressSpace::Generic),
            "",
        );

        generator.builder.build_store(lhs, rhs);
        generator.unit_value()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::MemberAccess {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);

        match value.as_instruction_value().map(|inst| inst.get_opcode()) {
            Some(InstructionOpcode::Load) => generator.gep_at_index(value, self.member_index, ""),
            _ => generator
                .builder
                .build_extract_value(value.into_struct_value(), self.member_index, "")
                .unwrap(),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Call {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let callee_ty = self.callee.ty().normalize(generator.tycx).into_function();

        let mut args = vec![];

        for (index, arg) in self.args.iter().enumerate() {
            let value = arg.codegen(generator, state);
            args.push((value, index));
        }

        args.sort_by_key(|&(_, i)| i);

        let args: Vec<BasicValueEnum> = args.iter().map(|(a, _)| *a).collect();

        let callee_ptr = self.callee.codegen(generator, state).into_pointer_value();

        // println!("callee: {:#?}", callee_ptr.get_type());

        // for arg in args.iter() {
        //     println!("arg: {:#?}", arg);
        // }

        let callable_value: CallableValue = callee_ptr.try_into().unwrap();

        generator.gen_function_call(
            state,
            callable_value,
            &callee_ty,
            args,
            &self.ty.normalize(generator.tycx),
        )
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Cast {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Sequence {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let mut yielded_value = generator.unit_value();

        state.push_scope();

        for (i, statement) in self.statements.iter().enumerate() {
            let value = statement.codegen(generator, state);
            if i == self.statements.len() - 1 {
                yielded_value = value;
            }
        }

        state.pop_scope();

        yielded_value
    }
}
