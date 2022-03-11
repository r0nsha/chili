use std::cmp::Ordering;

use chili_ast::ty::{size::SizeOf, *};

use inkwell::{
    types::{
        AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        PointerType,
    },
    AddressSpace,
};

use super::{
    abi::{self, AbiFn, AbiTyKind},
    codegen::Codegen,
};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn llvm_type(&mut self, ty: &Ty) -> BasicTypeEnum<'ctx> {
        match ty {
            Ty::Bool => self.context.bool_type().into(),
            Ty::Int(inner) => match inner {
                IntTy::I8 => self.context.i8_type().into(),
                IntTy::I16 => self.context.i16_type().into(),
                IntTy::I32 => self.context.i32_type().into(),
                IntTy::I64 => self.context.i64_type().into(),
                IntTy::Isize => self.ptr_sized_int_type.into(),
            },
            Ty::UInt(inner) => match inner {
                UIntTy::U8 => self.context.i8_type().into(),
                UIntTy::U16 => self.context.i16_type().into(),
                UIntTy::U32 => self.context.i32_type().into(),
                UIntTy::U64 => self.context.i64_type().into(),
                UIntTy::Usize => self.ptr_sized_int_type.into(),
            },
            Ty::Float(inner) => match inner {
                FloatTy::F16 => self.context.f16_type().into(),
                FloatTy::F32 => self.context.f32_type().into(),
                FloatTy::F64 => self.context.f64_type().into(),
                FloatTy::Fsize => if self.target_metrics.word_size == 8 {
                    self.context.f64_type()
                } else {
                    self.context.f32_type()
                }
                .into(),
            },
            Ty::Pointer(inner, _) | Ty::MultiPointer(inner, ..) => {
                let ty = self.llvm_type(&inner);
                ty.ptr_type(AddressSpace::Generic).into()
            }
            Ty::Type(_) | Ty::Unit | Ty::Never | Ty::Module { .. } => {
                self.unit_type()
            }
            Ty::Fn(func) => {
                self.fn_type(func).ptr_type(AddressSpace::Generic).into()
            }
            Ty::Array(inner, size) => {
                self.llvm_type(inner).array_type(*size as u32).into()
            }
            Ty::Slice(inner, ..) => self.slice_type(inner),
            Ty::Tuple(tys) => self
                .context
                .struct_type(
                    &tys.iter()
                        .map(|ty| self.llvm_type(ty))
                        .collect::<Vec<BasicTypeEnum>>(),
                    false,
                )
                .into(),
            Ty::Struct(struct_ty) => {
                let struct_type = if struct_ty.name.is_empty() {
                    self.create_anonymous_struct_type(struct_ty)
                } else {
                    self.get_or_create_named_struct_type(struct_ty)
                };

                struct_type.into()
            }
            _ => {
                panic!("bug: type `{}` in llvm codegen", ty)
            }
        }
    }

    pub(super) fn unit_type(&self) -> BasicTypeEnum<'ctx> {
        self.context.struct_type(&[], false).into()
    }

    pub(super) fn raw_pointer_type(&self) -> PointerType<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::Generic)
    }

    pub(super) fn slice_type(
        &mut self,
        element_ty: &Ty,
    ) -> BasicTypeEnum<'ctx> {
        self.context
            .struct_type(
                &[
                    self.llvm_type(element_ty)
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                    self.ptr_sized_int_type.into(),
                ],
                false,
            )
            .into()
    }

    pub(super) fn fn_type(&mut self, func: &FnTy) -> FunctionType<'ctx> {
        let params: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.ty).into())
            .collect();
        let ret = self.llvm_type(&func.ret);
        let fn_ty = ret.fn_type(&params, func.variadic);

        // println!("before: {:#?}", fn_ty);
        let abi_compliant_fn_ty =
            abi::get_abi_compliant_fn(self.context, self.target_metrics, fn_ty);

        self.fn_type_map
            .insert(func.clone(), abi_compliant_fn_ty.clone());

        // println!("after: {:#?}", self.abi_fn_to_type(&abi_compliant_fn_ty));
        self.abi_fn_to_type(&abi_compliant_fn_ty)
    }

    pub(super) fn abi_fn_to_type(
        &mut self,
        abi_fn: &AbiFn<'ctx>,
    ) -> FunctionType<'ctx> {
        let mut offset = 0;

        let ret = match abi_fn.ret.kind {
            AbiTyKind::Direct => match abi_fn.ret.cast_to {
                Some(cast_to) => cast_to,
                None => abi_fn.ret.ty,
            }
            .as_any_type_enum(),
            AbiTyKind::Indirect => {
                offset += 1;
                self.context.void_type().into()
            }
            AbiTyKind::Ignore => self.context.void_type().into(),
        };

        let mut params: Vec<BasicMetadataTypeEnum> = vec![];
        if offset == 1 {
            params.push(abi_fn.ret.ty.ptr_type(AddressSpace::Generic).into());
        }

        for param in abi_fn.params.iter() {
            let ty = match &param.kind {
                AbiTyKind::Direct => match param.cast_to {
                    Some(cast_to) => cast_to,
                    None => param.ty,
                },
                AbiTyKind::Indirect => {
                    param.ty.ptr_type(AddressSpace::Generic).into()
                }
                AbiTyKind::Ignore => unimplemented!("ignore '{:?}'", param.ty),
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

    pub(super) fn get_or_create_named_struct_type(
        &mut self,
        struct_ty: &StructTy,
    ) -> inkwell::types::StructType<'ctx> {
        match self.type_map.get(&struct_ty.qualified_name) {
            Some(t) => t.into_struct_type(),
            None => self.create_named_struct_type(struct_ty),
        }
    }

    pub(super) fn create_named_struct_type(
        &mut self,
        struct_ty: &StructTy,
    ) -> inkwell::types::StructType<'ctx> {
        let struct_type =
            self.context.opaque_struct_type(&struct_ty.qualified_name);

        self.type_map
            .insert(struct_ty.qualified_name, struct_type.into());

        let fields = self.create_struct_type_fields(struct_ty);

        struct_type.set_body(&fields, struct_ty.is_packed_struct());

        struct_type
    }

    pub(super) fn create_anonymous_struct_type(
        &mut self,
        struct_ty: &StructTy,
    ) -> inkwell::types::StructType<'ctx> {
        let fields = self.create_struct_type_fields(struct_ty);
        self.context
            .struct_type(&fields, struct_ty.is_packed_struct())
    }

    fn create_struct_type_fields(
        &mut self,
        struct_ty: &StructTy,
    ) -> Vec<BasicTypeEnum<'ctx>> {
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

            let field_ty = self.llvm_type(&largest_field.ty);

            vec![field_ty]
        } else {
            struct_ty
                .fields
                .iter()
                .map(|f| self.llvm_type(&f.ty))
                .collect::<Vec<BasicTypeEnum>>()
        }
    }
}
