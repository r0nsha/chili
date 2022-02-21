use crate::codegen::{Codegen, CodegenState};
use chilic_ast::ast::{LiteralKind, StructLiteralField};
use chilic_ty::*;
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, PointerValue},
    AddressSpace,
};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_struct_literal_named(
        &mut self,
        state: &mut CodegenState<'ctx>,
        ty: &Ty,
        fields: &Vec<StructLiteralField>,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        let struct_ty = ty.into_struct();

        let struct_ptr = if struct_ty.is_union() {
            let struct_llvm_type = self.llvm_type(ty);

            let value = self.gen_expr(state, &fields[0].value, true);
            let field_ptr = self.build_alloca(state, value.get_type());

            self.build_store(field_ptr, value);

            let struct_ptr = self.builder.build_pointer_cast(
                field_ptr,
                struct_llvm_type.ptr_type(AddressSpace::Generic),
                "",
            );

            struct_ptr
        } else {
            let struct_llvm_ty = self.llvm_type(ty);
            let struct_ptr = self.build_alloca(state, struct_llvm_ty);

            for field in fields {
                let field_index = struct_ty
                    .fields
                    .iter()
                    .position(|f| f.symbol == field.symbol)
                    .unwrap();

                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        struct_ptr,
                        field_index as u32,
                        &format!(
                            "set_{}",
                            struct_ty.fields[field_index].symbol
                        ),
                    )
                    .unwrap();

                let value = self.gen_expr(state, &field.value, true);

                self.build_store(field_ptr, value);
            }

            struct_ptr
        };

        if deref {
            self.build_load(struct_ptr.into())
        } else {
            struct_ptr.into()
        }
    }

    pub(super) fn gen_struct(
        &mut self,
        state: &mut CodegenState<'ctx>,
        llvm_type: BasicTypeEnum<'ctx>,
        values: &[BasicValueEnum<'ctx>],
    ) -> PointerValue<'ctx> {
        let ptr = self.build_alloca(state, llvm_type.into());

        for (i, value) in values.iter().enumerate() {
            let field_ptr =
                self.builder.build_struct_gep(ptr, i as _, "").unwrap();
            self.build_store(field_ptr, *value);
        }

        ptr
    }

    pub(super) fn gen_literal_value(
        &mut self,
        value: &LiteralKind,
        ty: &Ty,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        match value {
            LiteralKind::Unit => self.gen_unit(),

            LiteralKind::Nil => self.gen_nil(ty),

            LiteralKind::Bool(v) => self
                .context
                .bool_type()
                .const_int(if *v { 1 } else { 0 }, false)
                .into(),

            LiteralKind::Int(v) => match ty {
                Ty::Int(_) | Ty::UInt(_) => self
                    .llvm_type(ty)
                    .into_int_type()
                    .const_int(*v as u64, ty.is_int())
                    .into(),

                Ty::Float(_) => self
                    .llvm_type(ty)
                    .into_float_type()
                    .const_float(*v as f64)
                    .into(),

                _ => unreachable!(),
            },

            LiteralKind::Float(v) => {
                self.llvm_type(ty).into_float_type().const_float(*v).into()
            }

            LiteralKind::Str(v) => self.gen_global_str("", v.as_str(), deref),

            LiteralKind::Char(v) => self
                .llvm_type(ty)
                .into_int_type()
                .const_int(*v as u64, false)
                .into(),
        }
    }
}
