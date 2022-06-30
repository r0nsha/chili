use super::{
    codegen::{FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{ast, types::*};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, PointerValue},
    AddressSpace,
};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub fn gen_struct_literal(
        &mut self,
        state: &mut FunctionState<'ctx>,
        ty: &Type,
        fields: &[ast::StructLiteralField],
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        todo!();

        // let struct_ty = ty.as_struct();
        // let struct_llvm_type = ty.llvm_type(self);

        // let struct_ptr = if struct_ty.is_union() {
        //     let value = self.gen_expr(state, &fields[0].expr, true);
        //     let field_ptr = self.build_alloca(state, value.get_type());

        //     self.build_store(field_ptr, value);

        //     let struct_ptr = self.builder.build_pointer_cast(
        //         field_ptr,
        //         struct_llvm_type.ptr_type(AddressSpace::Generic),
        //         "",
        //     );

        //     struct_ptr
        // } else {
        //     let struct_ptr = self.build_alloca(state, struct_llvm_type);

        //     for field in fields {
        //         let field_index = struct_ty.find_field_position(field.name).unwrap();

        //         let field_ptr = self
        //             .builder
        //             .build_struct_gep(struct_ptr, field_index as u32, "")
        //             .unwrap();

        //         let value = self.gen_expr(state, &field.expr, true);

        //         self.build_store(field_ptr, value);
        //     }

        //     struct_ptr
        // };

        // if deref {
        //     self.build_load(struct_ptr.into())
        // } else {
        //     struct_ptr.into()
        // }
    }

    pub fn gen_struct(
        &mut self,
        state: &mut FunctionState<'ctx>,
        llvm_type: BasicTypeEnum<'ctx>,
        values: &[BasicValueEnum<'ctx>],
    ) -> PointerValue<'ctx> {
        let ptr = self.build_alloca(state, llvm_type.into());

        for (i, value) in values.iter().enumerate() {
            let field_ptr = self.builder.build_struct_gep(ptr, i as _, "").unwrap();
            self.build_store(field_ptr, *value);
        }

        ptr
    }
}
