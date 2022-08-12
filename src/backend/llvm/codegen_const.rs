use super::{
    codegen::{FunctionState, Generator},
    ty::IntoLlvmType,
};
use crate::{hir::const_value::ConstValue, infer::normalize::Normalize, types::*};
use inkwell::{
    module::Linkage,
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue, StructValue},
};
use ustr::Ustr;

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn const_str_slice(&mut self, name: &str, value: impl Into<Ustr>) -> PointerValue<'ctx> {
        let value = value.into();
        let cached_str = self.static_strs.get(&value).map(|v| *v);

        let ptr = cached_str.unwrap_or_else(|| self.builder.build_global_string_ptr(&value, name).as_pointer_value());
        let len = self.ptr_sized_int_type.const_int(value.len() as u64, false);

        let slice = self.const_slice(ptr, len);
        let global = self.module.add_global(slice.get_type(), None, "");

        global.set_initializer(&slice);

        global.as_pointer_value()
    }

    #[inline]
    pub(super) fn const_slice(&self, ptr: PointerValue<'ctx>, len: IntValue<'ctx>) -> StructValue<'ctx> {
        self.const_struct(&[ptr.as_basic_value_enum(), len.as_basic_value_enum()])
    }

    #[inline]
    pub(super) fn const_unit(&self) -> BasicValueEnum<'ctx> {
        self.context.const_struct(&[], false).into()
    }

    #[inline]
    pub(super) fn const_struct(&self, values: &[BasicValueEnum<'ctx>]) -> StructValue<'ctx> {
        self.context.const_struct(&values, false)
    }

    #[inline]
    pub(super) fn const_bool(&self, b: bool) -> IntValue<'ctx> {
        self.context.bool_type().const_int(if b { 1 } else { 0 }, false)
    }

    pub(super) fn gen_const_value(
        &mut self,
        state: Option<&FunctionState<'ctx>>,
        const_value: &ConstValue,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        match const_value {
            ConstValue::Unit(_) | ConstValue::Type(_) => self.const_unit(),
            ConstValue::Bool(v) => self.const_bool(*v).into(),
            ConstValue::Int(v) => {
                if ty.is_any_integer() {
                    ty.llvm_type(self)
                        .into_int_type()
                        .const_int(*v as u64, ty.is_signed_int())
                        .into()
                } else {
                    ty.llvm_type(self).into_float_type().const_float(*v as f64).into()
                }
            }
            ConstValue::Uint(v) => {
                if ty.is_any_integer() {
                    ty.llvm_type(self)
                        .into_int_type()
                        .const_int(*v, ty.is_signed_int())
                        .into()
                } else {
                    ty.llvm_type(self).into_float_type().const_float(*v as f64).into()
                }
            }
            ConstValue::Float(v) => ty.llvm_type(self).into_float_type().const_float(*v as f64).into(),
            ConstValue::Str(v) => self.const_str_slice("", *v).into(),
            ConstValue::Array(array) => {
                let el_ty = array.element_type.normalize(self.tcx);

                let values: Vec<BasicValueEnum> = array
                    .values
                    .iter()
                    .map(|value| self.gen_const_value(state, value, &el_ty))
                    .collect();

                el_ty.llvm_type(self).const_array(&values).into()
            }
            ConstValue::Tuple(elements) => {
                let values = elements
                    .iter()
                    .map(|element| self.gen_const_value(state, &element.value, &element.ty.normalize(self.tcx)))
                    .collect::<Vec<BasicValueEnum>>();

                self.const_struct(&values).into()
            }
            ConstValue::Struct(fields) => {
                let values = fields
                    .values()
                    .map(|element| self.gen_const_value(state, &element.value, &element.ty.normalize(self.tcx)))
                    .collect::<Vec<BasicValueEnum>>();

                // self.context.const_struct(&values, false).into();
                ty.llvm_type(self).into_struct_type().const_named_struct(&values).into()
            }
            ConstValue::Function(function) => {
                let prev_block = if let Some(state) = state {
                    Some(state.current_block)
                } else {
                    self.builder.get_insert_block()
                };

                let function_value = self.gen_function(function.id, state.cloned());

                if let Some(previous_block) = prev_block {
                    self.builder.position_at_end(previous_block);
                }

                function_value.as_global_value().as_pointer_value().into()
            }
            ConstValue::ExternVariable(variable) => {
                if let Some(lib) = variable.lib.as_ref().or(variable.dylib.as_ref()) {
                    self.extern_libraries.insert(lib.clone());
                }

                let global_value = self.extern_variables.get(&variable.name).cloned().unwrap_or_else(|| {
                    let llvm_type = variable.ty.llvm_type(self);

                    let global_value = self.module.add_global(llvm_type, None, &variable.name);
                    global_value.set_linkage(Linkage::External);

                    self.extern_variables.insert(variable.name, global_value);

                    global_value
                });

                let ptr = global_value.as_pointer_value();

                self.build_load(ptr)
            }
        }
    }
}
