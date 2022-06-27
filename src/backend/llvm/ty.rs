use super::{
    abi::{self, AbiFunction, AbiType},
    codegen::Codegen,
};
use crate::infer::normalize::Normalize;
use crate::types::{size::SizeOf, *};
use inkwell::{
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType},
    AddressSpace,
};
use std::cmp::Ordering;

pub trait IntoLlvmType<'cg, 'ctx> {
    fn llvm_type(&self, cg: &mut Codegen<'cg, 'ctx>) -> BasicTypeEnum<'ctx>;
}

impl<'cg, 'ctx> IntoLlvmType<'cg, 'ctx> for TypeId {
    fn llvm_type(&self, cg: &mut Codegen<'cg, 'ctx>) -> BasicTypeEnum<'ctx> {
        let kind = self.normalize(cg.tycx);
        kind.llvm_type(cg)
    }
}

impl<'cg, 'ctx> IntoLlvmType<'cg, 'ctx> for Type {
    fn llvm_type(&self, cg: &mut Codegen<'cg, 'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            Type::Bool => cg.context.bool_type().into(),
            Type::Int(inner) => match inner {
                IntType::I8 => cg.context.i8_type().into(),
                IntType::I16 => cg.context.i16_type().into(),
                IntType::I32 => cg.context.i32_type().into(),
                IntType::I64 => cg.context.i64_type().into(),
                IntType::Int => cg.ptr_sized_int_type.into(),
            },
            Type::Uint(inner) => match inner {
                UintType::U8 => cg.context.i8_type().into(),
                UintType::U16 => cg.context.i16_type().into(),
                UintType::U32 => cg.context.i32_type().into(),
                UintType::U64 => cg.context.i64_type().into(),
                UintType::Uint => cg.ptr_sized_int_type.into(),
            },
            Type::Float(inner) => match inner {
                FloatType::F16 => cg.context.f16_type().into(),
                FloatType::F32 => cg.context.f32_type().into(),
                FloatType::F64 => cg.context.f64_type().into(),
                FloatType::Float => if cg.target_metrics.word_size == 8 {
                    cg.context.f64_type()
                } else {
                    cg.context.f32_type()
                }
                .into(),
            },
            Type::Pointer(inner, _) | Type::MultiPointer(inner, ..) => {
                let ty = inner.llvm_type(cg);
                ty.ptr_type(AddressSpace::Generic).into()
            }
            Type::Type(_) | Type::Unit | Type::Never | Type::Module { .. } => cg.unit_type(),
            Type::Function(func) => cg
                .abi_compliant_fn_type(func)
                .ptr_type(AddressSpace::Generic)
                .into(),
            Type::Array(inner, size) => inner.llvm_type(cg).array_type(*size as u32).into(),
            Type::Slice(inner, ..) => cg.slice_type(inner),
            Type::Tuple(tys) => cg
                .context
                .struct_type(
                    &tys.iter()
                        .map(|ty| ty.llvm_type(cg))
                        .collect::<Vec<BasicTypeEnum>>(),
                    false,
                )
                .into(),
            Type::Struct(struct_ty) => {
                let struct_type = if struct_ty.name.is_empty() {
                    cg.create_anonymous_struct_type(struct_ty)
                } else {
                    cg.get_or_create_named_struct_type(struct_ty)
                };

                struct_type.into()
            }
            _ => {
                panic!("bug: type `{}` in llvm codegen", self)
            }
        }
    }
}

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub fn unit_type(&self) -> BasicTypeEnum<'ctx> {
        self.context.struct_type(&[], false).into()
    }

    pub fn raw_pointer_type(&self) -> PointerType<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::Generic)
    }

    pub fn slice_type(&mut self, element_ty: &Type) -> BasicTypeEnum<'ctx> {
        self.context
            .struct_type(
                &[
                    element_ty
                        .llvm_type(self)
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                    self.ptr_sized_int_type.into(),
                ],
                false,
            )
            .into()
    }

    pub fn fn_type(&mut self, f: &FunctionType) -> inkwell::types::FunctionType<'ctx> {
        let params: Vec<BasicMetadataTypeEnum> = f
            .params
            .iter()
            .map(|p| p.ty.llvm_type(self).into())
            .collect();

        let ret = f.return_type.llvm_type(self);

        ret.fn_type(&params, f.varargs.is_some())
    }

    pub fn get_abi_compliant_fn(&mut self, f: &FunctionType) -> AbiFunction<'ctx> {
        let fn_type = self.fn_type(f);

        abi::get_abi_compliant_fn(self.context, &self.target_metrics, fn_type)
    }

    pub fn abi_compliant_fn_type(
        &mut self,
        f: &FunctionType,
    ) -> inkwell::types::FunctionType<'ctx> {
        let abi_compliant_fn_ty = self.get_abi_compliant_fn(f);
        self.abi_fn_to_type(&abi_compliant_fn_ty)
    }

    pub fn abi_fn_to_type(
        &mut self,
        abi_fn: &AbiFunction<'ctx>,
    ) -> inkwell::types::FunctionType<'ctx> {
        let mut offset = 0;

        let ret = match abi_fn.ret.kind {
            AbiType::Direct => match abi_fn.ret.cast_to {
                Some(cast_to) => cast_to,
                None => abi_fn.ret.ty,
            }
            .as_any_type_enum(),
            AbiType::Indirect => {
                offset += 1;
                self.context.void_type().into()
            }
            AbiType::Ignore => self.context.void_type().into(),
        };

        let mut params: Vec<BasicMetadataTypeEnum> = vec![];
        if offset == 1 {
            params.push(abi_fn.ret.ty.ptr_type(AddressSpace::Generic).into());
        }

        for param in abi_fn.params.iter() {
            let ty = match &param.kind {
                AbiType::Direct => match param.cast_to {
                    Some(cast_to) => cast_to,
                    None => param.ty,
                },
                AbiType::Indirect => param.ty.ptr_type(AddressSpace::Generic).into(),
                AbiType::Ignore => unimplemented!("ignore '{:?}'", param.ty),
            };

            params.push(ty.into());
        }

        if ret.is_void_type() {
            ret.into_void_type().fn_type(&params, abi_fn.variadic)
        } else {
            let ret: BasicTypeEnum = ret.try_into().unwrap();
            ret.fn_type(&params, abi_fn.variadic)
        }
    }

    pub fn get_or_create_named_struct_type(
        &mut self,
        struct_ty: &StructType,
    ) -> inkwell::types::StructType<'ctx> {
        match self.types.get(&struct_ty.binding_id) {
            Some(t) => t.into_struct_type(),
            None => self.create_named_struct_type(struct_ty),
        }
    }

    pub fn create_named_struct_type(
        &mut self,
        struct_ty: &StructType,
    ) -> inkwell::types::StructType<'ctx> {
        let struct_type = self.context.opaque_struct_type(&struct_ty.name);

        self.types.insert(struct_ty.binding_id, struct_type.into());

        let fields = self.create_struct_type_fields(struct_ty);
        struct_type.set_body(&fields, struct_ty.is_packed_struct());
        struct_type
    }

    pub fn create_anonymous_struct_type(
        &mut self,
        struct_ty: &StructType,
    ) -> inkwell::types::StructType<'ctx> {
        let fields = self.create_struct_type_fields(struct_ty);
        self.context
            .struct_type(&fields, struct_ty.is_packed_struct())
    }

    fn create_struct_type_fields(&mut self, struct_ty: &StructType) -> Vec<BasicTypeEnum<'ctx>> {
        if struct_ty.fields.is_empty() {
            vec![]
        } else if struct_ty.is_union() {
            let largest_field = struct_ty
                .fields
                .iter()
                .max_by(|f1, f2| {
                    let s1 = f1.ty.size_of(self.target_metrics.word_size);
                    let s2 = f2.ty.size_of(self.target_metrics.word_size);
                    if s1 > s2 {
                        Ordering::Greater
                    } else if s1 == s2 {
                        Ordering::Equal
                    } else {
                        Ordering::Less
                    }
                })
                .unwrap();

            let field_ty = largest_field.ty.llvm_type(self);

            vec![field_ty]
        } else {
            struct_ty
                .fields
                .iter()
                .map(|f| f.ty.llvm_type(self))
                .collect::<Vec<BasicTypeEnum>>()
        }
    }
}
