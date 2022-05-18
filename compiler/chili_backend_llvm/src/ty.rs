use std::cmp::Ordering;

use chili_ast::ty::{size::SizeOf, *};

use chili_infer::normalize::NormalizeTy;
use inkwell::{
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType},
    AddressSpace,
};

use super::{
    abi::{self, AbiFn, AbiTyKind},
    codegen::Codegen,
};

pub(crate) trait IntoLlvmType<'cg, 'ctx> {
    fn llvm_type(&self, cg: &mut Codegen<'cg, 'ctx>) -> BasicTypeEnum<'ctx>;
}

impl<'cg, 'ctx> IntoLlvmType<'cg, 'ctx> for Ty {
    fn llvm_type(&self, cg: &mut Codegen<'cg, 'ctx>) -> BasicTypeEnum<'ctx> {
        let kind = self.normalize(cg.tycx);
        kind.llvm_type(cg)
    }
}

impl<'cg, 'ctx> IntoLlvmType<'cg, 'ctx> for TyKind {
    fn llvm_type(&self, cg: &mut Codegen<'cg, 'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            TyKind::Bool => cg.context.bool_type().into(),
            TyKind::Infer(_, InferTy::AnyInt) => cg.ptr_sized_int_type.into(),
            TyKind::Infer(_, InferTy::AnyFloat) => if cg.target_metrics.word_size == 8 {
                cg.context.f64_type()
            } else {
                cg.context.f32_type()
            }
            .into(),
            TyKind::Int(inner) => match inner {
                IntTy::I8 => cg.context.i8_type().into(),
                IntTy::I16 => cg.context.i16_type().into(),
                IntTy::I32 => cg.context.i32_type().into(),
                IntTy::I64 => cg.context.i64_type().into(),
                IntTy::Int => cg.ptr_sized_int_type.into(),
            },
            TyKind::Uint(inner) => match inner {
                UintTy::U8 => cg.context.i8_type().into(),
                UintTy::U16 => cg.context.i16_type().into(),
                UintTy::U32 => cg.context.i32_type().into(),
                UintTy::U64 => cg.context.i64_type().into(),
                UintTy::Uint => cg.ptr_sized_int_type.into(),
            },
            TyKind::Float(inner) => match inner {
                FloatTy::F16 => cg.context.f16_type().into(),
                FloatTy::F32 => cg.context.f32_type().into(),
                FloatTy::F64 => cg.context.f64_type().into(),
                FloatTy::Float => if cg.target_metrics.word_size == 8 {
                    cg.context.f64_type()
                } else {
                    cg.context.f32_type()
                }
                .into(),
            },
            TyKind::Pointer(inner, _) | TyKind::MultiPointer(inner, ..) => {
                let ty = inner.llvm_type(cg);
                ty.ptr_type(AddressSpace::Generic).into()
            }
            TyKind::Type(_) | TyKind::Unit | TyKind::Never | TyKind::Module { .. } => {
                cg.unit_type()
            }
            TyKind::Fn(func) => cg.fn_type(func).ptr_type(AddressSpace::Generic).into(),
            TyKind::Array(inner, size) => inner.llvm_type(cg).array_type(*size as u32).into(),
            TyKind::Slice(inner, ..) => cg.slice_type(inner),
            TyKind::Tuple(tys) => cg
                .context
                .struct_type(
                    &tys.iter()
                        .map(|ty| ty.llvm_type(cg))
                        .collect::<Vec<BasicTypeEnum>>(),
                    false,
                )
                .into(),
            TyKind::Struct(struct_ty) => {
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

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn unit_type(&self) -> BasicTypeEnum<'ctx> {
        self.context.struct_type(&[], false).into()
    }

    pub(super) fn raw_pointer_type(&self) -> PointerType<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::Generic)
    }

    pub(super) fn slice_type(&mut self, element_ty: &TyKind) -> BasicTypeEnum<'ctx> {
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

    pub(super) fn fn_type(&mut self, func: &FnTy) -> FunctionType<'ctx> {
        let params: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| p.llvm_type(self).into())
            .collect();
        let ret = func.ret.llvm_type(self);
        let fn_ty = ret.fn_type(&params, func.variadic);

        // println!("before: {:#?}", fn_ty);
        let abi_compliant_fn_ty =
            abi::get_abi_compliant_fn(self.context, &self.target_metrics, fn_ty);

        self.fn_types
            .insert(func.clone(), abi_compliant_fn_ty.clone());

        // println!("after: {:#?}", self.abi_fn_to_type(&abi_compliant_fn_ty));
        self.abi_fn_to_type(&abi_compliant_fn_ty)
    }

    pub(super) fn abi_fn_to_type(&mut self, abi_fn: &AbiFn<'ctx>) -> FunctionType<'ctx> {
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
                AbiTyKind::Indirect => param.ty.ptr_type(AddressSpace::Generic).into(),
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
        match self.types.get(&struct_ty.binding_info_id) {
            Some(t) => t.into_struct_type(),
            None => self.create_named_struct_type(struct_ty),
        }
    }

    pub(super) fn create_named_struct_type(
        &mut self,
        struct_ty: &StructTy,
    ) -> inkwell::types::StructType<'ctx> {
        let struct_type = self.context.opaque_struct_type(&struct_ty.name);
        self.types
            .insert(struct_ty.binding_info_id, struct_type.into());
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

    fn create_struct_type_fields(&mut self, struct_ty: &StructTy) -> Vec<BasicTypeEnum<'ctx>> {
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
