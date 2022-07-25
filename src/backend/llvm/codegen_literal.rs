use super::{
    abi::size_of,
    codegen::{Codegen, FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{hir, infer::normalize::Normalize, types::*};
use inkwell::{types::BasicType, values::BasicValueEnum, AddressSpace, IntPredicate};

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Literal {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            hir::Literal::Struct(x) => x.codegen(generator, state),
            hir::Literal::Tuple(x) => x.codegen(generator, state),
            hir::Literal::Array(x) => x.codegen(generator, state),
            hir::Literal::ArrayFill(x) => x.codegen(generator, state),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::StructLiteral {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let ty = &self.ty.normalize(generator.tcx);
        let struct_ty = ty.as_struct();
        let llvm_type = ty.llvm_type(generator);

        match struct_ty.kind {
            StructTypeKind::Struct | StructTypeKind::PackedStruct => {
                let struct_ptr = generator.build_alloca(state, llvm_type);

                for field in self.fields.iter() {
                    let field_index = struct_ty.find_field_position(field.name).unwrap();

                    let field_ptr = generator
                        .builder
                        .build_struct_gep(struct_ptr, field_index as u32, "")
                        .unwrap();

                    let value = field.value.codegen(generator, state);

                    generator.build_store(field_ptr, value);
                }

                generator.build_load(struct_ptr)
            }
            StructTypeKind::Union => {
                let value = self.fields.first().as_ref().unwrap().value.codegen(generator, state);

                let field_ptr = generator.build_alloca(state, value.get_type());

                generator.build_store(field_ptr, value);

                let struct_ptr =
                    generator
                        .builder
                        .build_pointer_cast(field_ptr, llvm_type.ptr_type(AddressSpace::Generic), "");

                generator.build_load(struct_ptr)
            }
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::TupleLiteral {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let ty = self.ty.normalize(generator.tcx);

        let values: Vec<BasicValueEnum> = self
            .elements
            .iter()
            .map(|element| element.codegen(generator, state))
            .collect();

        let llvm_type = ty.llvm_type(generator);
        let tuple_ptr = generator.build_alloca(state, llvm_type);

        for (i, value) in values.iter().enumerate() {
            let ptr = generator.builder.build_struct_gep(tuple_ptr, i as u32, "").unwrap();

            generator.build_store(ptr, *value);
        }

        generator.build_load(tuple_ptr)
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::ArrayLiteral {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let ty = self.ty.normalize(generator.tcx);

        let elements: Vec<BasicValueEnum> = self
            .elements
            .iter()
            .map(|element| element.codegen(generator, state))
            .collect();

        let size = match &ty {
            Type::Array(_, size) => *size,
            _ => unreachable!("got ty `{}`", ty),
        };

        let llvm_type = ty.llvm_type(generator);
        let array_ptr = generator.build_alloca(state, llvm_type);

        for index in 0..size {
            let access = unsafe {
                generator.builder.build_in_bounds_gep(
                    array_ptr,
                    &[
                        generator.context.i32_type().const_zero(),
                        generator.context.i32_type().const_int(index as u64, true),
                    ],
                    "",
                )
            };

            generator.build_store(access, elements[index]);
        }

        array_ptr.into()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::ArrayFillLiteral {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let ty = self.ty.normalize(generator.tcx);

        let value = self.value.codegen(generator, state);

        let array_type = ty.llvm_type(generator);
        let element_type = array_type.into_array_type().get_element_type();
        let array_ptr = generator.build_alloca(state, array_type);

        let src_ptr = generator.build_alloca_or_load_addr(state, value);

        let loop_head = generator.append_basic_block(state, "array_fill_head");
        let loop_body = generator.append_basic_block(state, "array_fill_body");
        let loop_exit = generator.append_basic_block(state, "array_fill_exit");

        let start = generator.ptr_sized_int_type.const_zero();
        let end = generator
            .ptr_sized_int_type
            .const_int(array_type.into_array_type().len() as _, false);

        let it = generator.build_alloca(state, start.get_type().into());
        generator.build_store(it, start.into());

        generator.builder.build_unconditional_branch(loop_head);
        generator.start_block(state, loop_head);

        let it_value = generator.build_load(it.into()).into_int_value();

        let condition = generator
            .builder
            .build_int_compare(IntPredicate::SLT, it_value, end, "");

        generator
            .builder
            .build_conditional_branch(condition, loop_body, loop_exit);

        generator.start_block(state, loop_body);

        let dst_ptr = unsafe {
            generator
                .builder
                .build_in_bounds_gep(array_ptr, &[generator.ptr_sized_int_type.const_zero(), it_value], "")
        };

        let sz = size_of(element_type, generator.target_metrics.word_size);
        generator.build_copy_nonoverlapping(src_ptr, dst_ptr, generator.ptr_sized_int_type.const_int(sz as _, false));

        let next_it = generator
            .builder
            .build_int_add(it_value, it_value.get_type().const_int(1, false), "");

        generator.build_store(it, next_it.into());

        generator.builder.build_unconditional_branch(loop_head);

        generator.start_block(state, loop_exit);

        array_ptr.into()
    }
}
