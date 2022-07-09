use super::{
    codegen::{Codegen, Decl, FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{
    hir::{self, const_value::ConstValue},
    infer::normalize::Normalize,
    types::*,
};
use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{BasicValue, BasicValueEnum, CallableValue, InstructionOpcode},
    AddressSpace,
};

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
        let ty = self.ty.normalize(generator.tcx);
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

            let ty = binding_info.ty.normalize(generator.tcx);
            let llvm_type = ty.llvm_type(generator);

            match self.value.as_const_value() {
                Some(ConstValue::Function(function)) => {
                    let function = generator.gen_function(function.id, None);
                    generator.insert_global_decl(self.id, Decl::Function(function))
                }
                Some(const_value) => {
                    let global_value =
                        generator.add_global_uninit(self.id, llvm_type, Linkage::Private);

                    let value = generator.gen_const_value(None, const_value, &ty);
                    global_value.set_initializer(&value);

                    generator.insert_global_decl(self.id, Decl::Global(global_value))
                }
                None => {
                    let global_value =
                        generator.add_global_uninit(self.id, llvm_type, Linkage::Private);

                    global_value.set_initializer(&llvm_type.const_zero());

                    generator.insert_global_decl(self.id, Decl::Global(global_value))
                }
            }
        }
    }

    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        generator.local_with_alloca(state, self.id, value);
        // let ptr = generator.local_or_load_addr(state, self.id, value);
        // state.scopes.insert(self.id, Decl::Local(ptr));
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
            Some(InstructionOpcode::Load) => generator.gep_struct(value, self.member_index, ""),
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
        let callee_ty = self.callee.ty().normalize(generator.tcx).into_function();

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
            &self.ty.normalize(generator.tcx),
        )
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Cast {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);

        let from_ty = &self.value.ty().normalize(generator.tcx);
        let target_ty = &self.ty.normalize(generator.tcx);

        if from_ty == target_ty {
            return value;
        }

        const INST_NAME: &str = "cast";

        let cast_type = target_ty.llvm_type(generator);

        match (from_ty, target_ty) {
            (Type::Bool, Type::Int(_)) | (Type::Bool, Type::Uint(_)) => generator
                .builder
                .build_int_z_extend(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),
            (Type::Int(_) | Type::Uint(_), Type::Int(_) | Type::Uint(_)) => generator
                .builder
                .build_int_cast(value.into_int_value(), cast_type.into_int_type(), INST_NAME)
                .into(),

            (Type::Int(_), Type::Float(_)) => generator
                .builder
                .build_signed_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Uint(_), Type::Float(_)) => generator
                .builder
                .build_unsigned_int_to_float(
                    value.into_int_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Float(_), Type::Int(_)) => generator
                .builder
                .build_float_to_signed_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (Type::Float(_), Type::Uint(_)) => generator
                .builder
                .build_float_to_unsigned_int(
                    value.into_float_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),
            (Type::Float(_), Type::Float(_)) => generator
                .builder
                .build_float_cast(
                    value.into_float_value(),
                    cast_type.into_float_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Pointer(..), Type::Pointer(..)) => generator
                .builder
                .build_pointer_cast(
                    value.into_pointer_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            // pointer <=> int | uint
            (Type::Pointer(..), Type::Int(..) | Type::Uint(..)) => generator
                .builder
                .build_ptr_to_int(
                    value.into_pointer_value(),
                    cast_type.into_int_type(),
                    INST_NAME,
                )
                .into(),

            // int | uint <=> pointer
            (Type::Int(..) | Type::Uint(..), Type::Pointer(..)) => generator
                .builder
                .build_int_to_ptr(
                    value.into_int_value(),
                    cast_type.into_pointer_type(),
                    INST_NAME,
                )
                .into(),

            (Type::Pointer(t, _), Type::Slice(t_slice, ..)) => match t.as_ref() {
                Type::Array(_, size) => {
                    let slice_ty = generator.slice_type(t_slice);
                    let ptr = generator.build_alloca(state, slice_ty);

                    generator.build_slice(
                        ptr,
                        value,
                        generator.ptr_sized_int_type.const_zero(),
                        generator.ptr_sized_int_type.const_int(*size as u64, false),
                        t_slice.as_ref(),
                    );

                    generator.build_load(ptr.into())
                }
                _ => unreachable!(),
            },

            _ => unreachable!("can't cast {} to {}", from_ty, target_ty),
        }
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
