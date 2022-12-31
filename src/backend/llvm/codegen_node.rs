use super::{
    codegen::{Codegen, Decl, FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{
    hir::{self, const_value::ConstValue},
    infer::{display::DisplayType, normalize::Normalize},
    types::*,
    workspace::BindingInfoKind,
};
use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{BasicValue, BasicValueEnum, CallableValue, InstructionOpcode},
    AddressSpace,
};

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Node {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        if state.current_block.get_terminator().is_some() {
            return generator.const_unit();
        }

        match self {
            hir::Node::Const(x) => x.codegen(generator, state),
            hir::Node::Binding(x) => x.codegen(generator, state),
            hir::Node::Id(x) => x.codegen(generator, state),
            hir::Node::Assign(x) => x.codegen(generator, state),
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
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let ty = self.ty.normalize(generator.tcx);
        generator.gen_const_value(Some(state), &self.value, &ty)
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Binding {
    fn codegen_global(&self, generator: &mut Generator<'g, 'ctx>) -> super::codegen::Decl<'ctx> {
        if let Some(decl) = generator.global_decls.get(&self.id) {
            *decl
        } else {
            let binding_info = generator.workspace.binding_infos.get(self.id).unwrap();

            let ty = binding_info.ty.normalize(generator.tcx);
            let llvm_type = ty.llvm_type(generator);

            let decl = match &binding_info.kind {
                BindingInfoKind::LetStatic => {
                    // statically initialize this binding
                    let global_value = generator.add_global(self.id, llvm_type, Linkage::Private);
                    global_value.set_initializer(&llvm_type.const_zero());

                    generator.initialize_static(global_value, &self.value);

                    Decl::Global(global_value)
                }
                _ => match self.value.as_const_value() {
                    Some(ConstValue::Function(function)) => Decl::Function(generator.gen_function(function.id, None)),
                    Some(const_value) => {
                        let global_value = generator.add_global(self.id, llvm_type, Linkage::Private);

                        global_value.set_initializer(&generator.gen_const_value(None, const_value, &ty));

                        Decl::Global(global_value)
                    }
                    None => {
                        panic!("expected top level binding to be statically initialized")
                    }
                },
            };

            generator.insert_global_decl(self.id, decl)
        }
    }

    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let binding_info = generator.workspace.binding_infos.get(self.id).unwrap();

        let ty = binding_info.ty.normalize(generator.tcx);
        let llvm_type = ty.llvm_type(generator);

        match &binding_info.kind {
            BindingInfoKind::LetStatic => {
                // statically initialize this binding
                let global_value = generator.add_global(self.id, llvm_type, Linkage::Private);
                global_value.set_initializer(&llvm_type.const_zero());

                generator.insert_global_decl(self.id, Decl::Global(global_value));

                generator.initialize_static(global_value, &self.value);
            }
            _ => {
                let value = self.value.codegen(generator, state);
                generator.gen_local(state, self.id, value);
            }
        }

        generator.const_unit()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Id {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let decl_ptr = match state.scopes.get(self.id) {
            Some((_, decl)) => decl.as_pointer_value(),
            None => generator.gen_top_level_binding(self.id).as_pointer_value(),
        };

        generator.build_load(
            decl_ptr,
            &format!("load_{}", &generator.workspace.binding_infos.get(self.id).unwrap().name),
        )
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Assign {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let lhs = self.lhs.codegen(generator, state);

        let lhs = match lhs.as_instruction_value() {
            Some(inst) if inst.get_opcode() == InstructionOpcode::Load => {
                inst.get_operand(0).unwrap().left().unwrap().into_pointer_value()
            }
            _ => lhs.into_pointer_value(),
        };

        let rhs = self.rhs.codegen(generator, state);

        let lhs = generator
            .builder
            .build_pointer_cast(lhs, rhs.get_type().ptr_type(AddressSpace::Generic), "");

        generator.builder.build_store(lhs, rhs);
        generator.const_unit()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::MemberAccess {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        generator.gep_struct(
            value,
            self.member_index,
            &self.member_name,
            self.value.ty().normalize(generator.tcx).is_fat_pointer(),
        )
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Call {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
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
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);

        let from_type = &self.value.ty().normalize(generator.tcx);
        let target_type = &self.ty.normalize(generator.tcx);

        if from_type == target_type {
            return value;
        }

        const INST_NAME: &str = "cast";

        let cast_type = target_type.llvm_type(generator);

        match (from_type, target_type) {
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
                .build_signed_int_to_float(value.into_int_value(), cast_type.into_float_type(), INST_NAME)
                .into(),

            (Type::Uint(_), Type::Float(_)) => generator
                .builder
                .build_unsigned_int_to_float(value.into_int_value(), cast_type.into_float_type(), INST_NAME)
                .into(),

            (Type::Float(_), Type::Int(_)) => generator
                .builder
                .build_float_to_signed_int(value.into_float_value(), cast_type.into_int_type(), INST_NAME)
                .into(),
            (Type::Float(_), Type::Uint(_)) => generator
                .builder
                .build_float_to_unsigned_int(value.into_float_value(), cast_type.into_int_type(), INST_NAME)
                .into(),
            (Type::Float(_), Type::Float(_)) => generator
                .builder
                .build_float_cast(value.into_float_value(), cast_type.into_float_type(), INST_NAME)
                .into(),

            (Type::Pointer(_, _), Type::Pointer(_, _)) => generator
                .builder
                .build_pointer_cast(value.into_pointer_value(), cast_type.into_pointer_type(), INST_NAME)
                .into(),

            // pointer <=> int | uint
            (Type::Pointer(..), Type::Int(..) | Type::Uint(..)) => generator
                .builder
                .build_ptr_to_int(value.into_pointer_value(), cast_type.into_int_type(), INST_NAME)
                .into(),

            // int | uint <=> pointer
            (Type::Int(..) | Type::Uint(..), Type::Pointer(..)) => generator
                .builder
                .build_int_to_ptr(value.into_int_value(), cast_type.into_pointer_type(), INST_NAME)
                .into(),

            _ => unreachable!(
                "can't cast {} to {}",
                from_type.display(generator.tcx),
                target_type.display(generator.tcx)
            ),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Sequence {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let mut yielded_value = generator.const_unit();

        if self.is_scope {
            state.push_scope();
        }

        for (i, statement) in self.statements.iter().enumerate() {
            let value = statement.codegen(generator, state);
            if i == self.statements.len() - 1 {
                yielded_value = value;
            }
        }

        if self.is_scope {
            state.pop_scope();
        }

        yielded_value
    }
}
