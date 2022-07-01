use super::{
    codegen::{Codegen, FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{hir, infer::normalize::Normalize, types::*};
use inkwell::{types::BasicType, values::BasicValueEnum, AddressSpace};

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Literal {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match self {
            hir::Literal::Struct(x) => x.codegen(generator, state),
            hir::Literal::Tuple(x) => x.codegen(generator, state),
            hir::Literal::Array(x) => x.codegen(generator, state),
            hir::Literal::ArrayFill(x) => x.codegen(generator, state),
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::StructLiteral {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ty = &self.ty.normalize(generator.tycx);
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

                struct_ptr.into()
            }
            StructTypeKind::Union => {
                let value = self
                    .fields
                    .first()
                    .as_ref()
                    .unwrap()
                    .value
                    .codegen(generator, state);

                let field_ptr = generator.build_alloca(state, value.get_type());

                generator.build_store(field_ptr, value);

                let struct_ptr = generator.builder.build_pointer_cast(
                    field_ptr,
                    llvm_type.ptr_type(AddressSpace::Generic),
                    "",
                );

                struct_ptr.into()
            }
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::TupleLiteral {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ty = self.ty.normalize(generator.tycx);

        let values: Vec<BasicValueEnum> = self
            .elements
            .iter()
            .map(|element| element.codegen(generator, state))
            .collect();

        let llvm_type = ty.llvm_type(generator);
        let tuple = generator.build_alloca(state, llvm_type);

        for (i, value) in values.iter().enumerate() {
            let ptr = generator
                .builder
                .build_struct_gep(tuple, i as u32, "")
                .unwrap();

            generator.build_store(ptr, *value);
        }

        tuple.into()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::ArrayLiteral {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::ArrayFillLiteral {
    fn codegen(
        &self,
        generator: &mut Generator<'g, 'ctx>,
        state: &mut FunctionState<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
