use super::{
    codegen::{Codegen, FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{hir, types::*};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, PointerValue},
    AddressSpace,
};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn gen_struct(
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
