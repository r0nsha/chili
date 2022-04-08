use crate::{
    abi::{align_of, size_of},
    codegen::{Codegen, CodegenDeclsMap, CodegenState},
};
use chili_ast::ast::FnSig;
use chili_ast::ty::*;
use common::mem::calculate_align;
use inkwell::{
    basic_block::BasicBlock,
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, InstructionOpcode, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};
use std::mem;
use ustr::{ustr, Ustr, UstrMap};

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    #[inline]
    pub(super) fn get_or_insert_new_module(&mut self, module: Ustr) -> &mut CodegenDeclsMap<'ctx> {
        self.module_decl_map
            .entry(module)
            .or_insert(UstrMap::default())
    }

    pub(super) fn gen_global_str(
        &mut self,
        name: &str,
        value: impl Into<Ustr>,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        let value = value.into();
        let cached_str = self.global_str_map.get(&value).map(|v| *v);
        let str_ptr = cached_str.unwrap_or_else(|| {
            let str_ptr = self
                .builder
                .build_global_string_ptr(&value, name)
                .as_pointer_value();

            let element_ty = Ty::UInt(UIntTy::U8);
            let ty = self.slice_type(&element_ty);

            let str_slice_ptr = self.module.add_global(ty, Some(AddressSpace::Generic), "");

            str_slice_ptr.set_initializer(&ty.const_zero());

            let str_slice_ptr = str_slice_ptr.as_pointer_value();

            self.gen_slice(
                str_slice_ptr,
                str_ptr.into(),
                self.ptr_sized_int_type.const_zero(),
                self.ptr_sized_int_type.const_int(value.len() as u64, false),
                &element_ty,
            );

            self.global_str_map.insert(value, str_slice_ptr);

            str_slice_ptr
        });

        if deref {
            self.build_load(str_ptr.into())
        } else {
            str_ptr.into()
        }
    }

    pub(super) fn gen_nil(&mut self, ty: TyKind) -> BasicValueEnum<'ctx> {
        let llvm_ty = self.llvm_type(ty);
        llvm_ty.into_pointer_type().const_null().into()
    }

    pub(super) fn gen_unit(&self) -> BasicValueEnum<'ctx> {
        self.context.const_struct(&[], false).into()
    }

    pub(super) fn gen_load_slice_data(&self, slice: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
        self.gen_struct_access(slice, 0, None).into_pointer_value()
    }

    pub(super) fn gen_load_slice_len(&self, slice: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        let value = self.gen_struct_access(slice, 1, None);
        self.build_load(value).into_int_value()
    }

    pub(super) fn gen_slice(
        &mut self,
        ptr: PointerValue<'ctx>,
        sliced_value: BasicValueEnum<'ctx>,
        low: IntValue<'ctx>,
        high: IntValue<'ctx>,
        element_ty: &Ty,
    ) {
        let data = self.builder.build_bitcast(
            sliced_value,
            self.llvm_type(element_ty).ptr_type(AddressSpace::Generic),
            "bitcast_slice_data",
        );

        let data = unsafe {
            self.builder
                .build_gep(data.into_pointer_value(), &[low], "slice_low_addr")
        };

        let data_ptr = self.builder.build_struct_gep(ptr, 0, "slice_data").unwrap();

        self.build_store(data_ptr, data.into());

        let slice_len = self.builder.build_int_sub(high, low, "get_slice_len");
        let slice_len = self.builder.build_int_cast(
            slice_len,
            self.ptr_sized_int_type,
            "cast_slice_len_to_int",
        );

        let len_ptr = self.builder.build_struct_gep(ptr, 1, "slice_len").unwrap();

        self.build_store(len_ptr, slice_len.into());
    }

    pub(super) fn current_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    pub(super) fn append_basic_block(
        &self,
        state: &CodegenState<'ctx>,
        name: &str,
    ) -> BasicBlock<'ctx> {
        self.context.append_basic_block(state.function, name)
    }

    pub(super) fn start_block(&self, state: &mut CodegenState<'ctx>, block: BasicBlock<'ctx>) {
        state.curr_block = block;
        self.builder.position_at_end(block);
    }

    #[allow(unused)]
    pub(super) fn print_current_state(&self, state: &CodegenState<'ctx>) {
        let current_block = self.current_block();
        println!(
            "function: {}\n\tblock: {}\n\tterminated: {}",
            state.function.get_name().to_str().unwrap(),
            current_block.get_name().to_str().unwrap(),
            current_block.get_terminator().is_some()
        );
    }

    pub(super) fn build_alloca(
        &self,
        state: &CodegenState<'ctx>,
        llvm_ty: BasicTypeEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        self.build_alloca_internal(state, llvm_ty, "")
    }

    pub(super) fn build_alloca_named(
        &self,
        state: &mut CodegenState<'ctx>,
        llvm_ty: BasicTypeEnum<'ctx>,
        symbol: Ustr,
    ) -> PointerValue<'ctx> {
        if !symbol.is_empty() {
            if let Some((decl, depth)) = state.env.get_with_depth(&symbol) {
                let is_same_depth = depth == state.env.depth();
                let ptr = decl.into_pointer_value();
                let is_same_type = ptr.get_type().get_element_type() == llvm_ty.as_any_type_enum();
                if is_same_depth && is_same_type {
                    return ptr;
                }
            }
        }

        self.build_alloca_internal(state, llvm_ty, &symbol)
    }

    fn build_alloca_internal(
        &self,
        state: &CodegenState<'ctx>,
        llvm_ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        self.builder.position_at_end(state.decl_block);

        let is_array = llvm_ty.is_pointer_type()
            && llvm_ty
                .into_pointer_type()
                .get_element_type()
                .is_array_type();

        let llvm_ty = if is_array {
            llvm_ty
                .into_pointer_type()
                .get_element_type()
                .try_into()
                .unwrap()
        } else {
            llvm_ty
        };

        let ptr = self.builder.build_alloca(llvm_ty, &name);

        let align = align_of(llvm_ty, self.target_metrics.word_size);
        ptr.as_instruction_value()
            .unwrap()
            .set_alignment(align as _)
            .unwrap();

        self.builder.position_at_end(state.curr_block);

        ptr
    }

    pub(super) fn build_load(&self, value: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        if value.is_pointer_value() {
            let ptr = value.into_pointer_value();
            let element_type = ptr.get_type().get_element_type();
            if element_type.is_function_type() || element_type.is_array_type() {
                value
            } else {
                self.builder.build_load(ptr, "")
            }
        } else {
            value
        }
    }

    pub(super) fn get_operand(&self, value: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let inst = value.as_instruction_value().unwrap();
        let operand = inst.get_operand(0).unwrap();
        operand.unwrap_left()
    }

    pub(super) fn build_store(&self, ptr: PointerValue<'ctx>, value: BasicValueEnum<'ctx>) {
        let ty = value.get_type();
        if ty.is_pointer_type() {
            let ptr_type = ty.into_pointer_type();
            if ptr_type.get_element_type().is_array_type() {
                let array_ty = ptr_type.get_element_type();
                let size = size_of(array_ty.try_into().unwrap(), self.target_metrics.word_size);
                let size_value = self.ptr_sized_int_type.const_int(size as _, true);
                self.build_copy_nonoverlapping(value.into_pointer_value(), ptr, size_value);
                return;
            }
        }
        self.builder.build_store(ptr, value);
    }

    pub(super) fn build_unreachable(&self) {
        if self.current_block().get_terminator().is_none() {
            self.builder.build_unreachable();
        }
    }

    pub(super) fn maybe_load_double_pointer(&self, ptr: PointerValue<'ctx>) -> PointerValue<'ctx> {
        if ptr.get_type().get_element_type().is_pointer_type() {
            self.build_load(ptr.into()).into_pointer_value()
        } else {
            ptr
        }
    }

    #[allow(unused)]
    pub(super) fn build_memset_zero(&mut self, ptr: PointerValue<'ctx>, ty: &Ty) {
        self.build_memset(ptr, ty, self.context.i8_type().const_zero())
    }

    #[allow(unused)]
    pub(super) fn build_memset(&mut self, ptr: PointerValue<'ctx>, ty: &Ty, value: IntValue<'ctx>) {
        let ty = self.llvm_type(ty);
        let bytes_to_set = ty.size_of().unwrap();
        self.builder
            .build_memset(
                ptr,
                self.target_metrics.word_size as u32,
                value,
                bytes_to_set,
            )
            .unwrap();
    }

    pub(super) fn build_copy_value_to_ptr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
        dst_type: BasicTypeEnum<'ctx>,
        align: u32,
    ) -> PointerValue<'ctx> {
        let dst_align = align_of(dst_type, self.target_metrics.word_size);
        let align = align.min(dst_align as _);

        let ptr = self.gen_local_with_alloca(state, ustr(""), value);

        ptr.as_instruction_value()
            .unwrap()
            .set_alignment(align)
            .unwrap();

        ptr
    }

    #[allow(unused)]
    pub(super) fn build_copy_overlapping(
        &self,
        src_ptr: PointerValue<'ctx>,
        dst_ptr: PointerValue<'ctx>,
        size: IntValue<'ctx>,
    ) {
        let t_rawptr = self.raw_pointer_type();

        let src_ptr = self.builder.build_pointer_cast(src_ptr, t_rawptr, "");
        let dst_ptr = self.builder.build_pointer_cast(dst_ptr, t_rawptr, "");
        let size = self
            .builder
            .build_int_cast(size, self.ptr_sized_int_type, "");

        self.builder
            .build_memmove(
                dst_ptr,
                self.target_metrics.word_size as u32,
                src_ptr,
                self.target_metrics.word_size as u32,
                size,
            )
            .unwrap();
    }

    pub(super) fn build_copy_nonoverlapping(
        &self,
        src_ptr: PointerValue<'ctx>,
        dst_ptr: PointerValue<'ctx>,
        size: IntValue<'ctx>,
    ) {
        let t_rawptr = self.raw_pointer_type();

        let src_ptr = self.builder.build_pointer_cast(src_ptr, t_rawptr, "");
        let dst_ptr = self.builder.build_pointer_cast(dst_ptr, t_rawptr, "");

        let size = self
            .builder
            .build_int_cast(size, self.ptr_sized_int_type, "");

        self.builder
            .build_memcpy(
                dst_ptr,
                self.target_metrics.word_size as u32,
                src_ptr,
                self.target_metrics.word_size as u32,
                size,
            )
            .unwrap();
    }

    pub(super) fn build_transmute(
        &self,
        state: &CodegenState<'ctx>,
        value: BasicValueEnum<'ctx>,
        dst_type: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        use inkwell::types::BasicTypeEnum::*;

        let src_type = value.get_type();

        if src_type == dst_type {
            return value;
        }

        let src_size = size_of(src_type, self.target_metrics.word_size);
        let dst_size = size_of(dst_type, self.target_metrics.word_size);

        let mut src_align = align_of(src_type, self.target_metrics.word_size);
        let dst_align = align_of(dst_type, self.target_metrics.word_size);

        // println!("src_type => {:#?}", src_type);
        // println!("dst_type => {:#?}", dst_type);

        if is_a_load_inst(value) {
            let inst_align = value
                .as_instruction_value()
                .unwrap()
                .get_alignment()
                .unwrap();
            src_align = src_align.min(inst_align as usize);
        }

        match (src_type, dst_type) {
            (_, IntType(i)) if i.get_bit_width() == 1 => self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    value.into_int_value(),
                    src_type.into_int_type().const_zero(),
                    "",
                )
                .into(),

            (IntType(i), _) if i.get_bit_width() == 1 => self
                .builder
                .build_int_z_extend_or_bit_cast(
                    value.into_int_value(),
                    dst_type.into_int_type(),
                    "",
                )
                .into(),

            (PointerType(_), PointerType(_)) => self
                .builder
                .build_pointer_cast(value.into_pointer_value(), dst_type.into_pointer_type(), "")
                .into(),

            (PointerType(_), IntType(_)) => self
                .builder
                .build_ptr_to_int(value.into_pointer_value(), dst_type.into_int_type(), "")
                .into(),

            (IntType(_), PointerType(_)) => self
                .builder
                .build_int_to_ptr(value.into_int_value(), dst_type.into_pointer_type(), "")
                .into(),

            (src_type, dst_type)
                if mem::discriminant(&src_type) == mem::discriminant(&dst_type)
                    && !src_type.is_aggregate_type() =>
            {
                self.builder.build_bitcast(value, dst_type, "")
            }

            _ => {
                src_align = src_align.max(dst_align as _) as _;

                if is_a_load_inst(value) && src_align < dst_align {
                    let inst = value.as_instruction_value().unwrap();
                    let value_ptr = inst.get_operand(0).unwrap();

                    if value_ptr.is_left() {
                        let value_ptr = value_ptr.unwrap_left();

                        if let Some(inst) = value_ptr.as_instruction_value() {
                            if inst.get_opcode() == InstructionOpcode::Alloca {
                                let value_inst = value_ptr
                                    .as_instruction_value()
                                    .expect(&format!("{:#?}", value_ptr));

                                src_align =
                                    value_inst.get_alignment().unwrap().max(dst_align as _) as _;

                                value_inst.set_alignment(src_align as _).unwrap();
                            }
                        }
                    }
                }

                let src_size = calculate_align(src_size, src_align);
                let dst_size = calculate_align(dst_size, dst_align);

                // println!("src_size: {}, dst_size: {}", src_size, dst_size);
                // println!("src_align: {}, dst_align: {}", src_align,
                // dst_align);

                if is_a_load_inst(value) && src_size >= dst_size && src_align >= dst_align {
                    let value_ptr = self.get_operand(value).into_pointer_value();

                    let ptr = self.builder.build_pointer_cast(
                        value_ptr,
                        dst_type.ptr_type(AddressSpace::Generic),
                        "",
                    );

                    self.build_load(ptr.into())
                } else {
                    let ptr = self.build_alloca(state, dst_type);

                    if let Some(inst) = ptr.as_instruction_value() {
                        let max_align = align_of(src_type, self.target_metrics.word_size)
                            .max(align_of(dst_type, self.target_metrics.word_size));
                        let max_align = max_align.max(4);
                        inst.set_alignment(max_align as _).unwrap();
                    }

                    let ptr = self.builder.build_pointer_cast(
                        ptr,
                        src_type.ptr_type(AddressSpace::Generic),
                        "",
                    );

                    self.build_store(ptr, value);

                    let ptr = self.builder.build_pointer_cast(
                        ptr,
                        dst_type.ptr_type(AddressSpace::Generic),
                        "",
                    );

                    self.build_load(ptr.into())
                }
            }
        }
    }

    pub(super) fn gen_struct_access(
        &self,
        agg_or_ptr: BasicValueEnum<'ctx>,
        index: u32,
        struct_ty: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        if agg_or_ptr.is_pointer_value() {
            let agg_or_ptr = self.maybe_load_double_pointer(agg_or_ptr.into_pointer_value());
            let el_type = agg_or_ptr.get_type().get_element_type();

            if el_type.is_struct_type() {
                self.builder
                    .build_struct_gep(agg_or_ptr, index, &format!("get_struct_ptr_{}", index))
                    .expect(&format!("{:#?}", agg_or_ptr))
                    .into()
            } else {
                let struct_ty = struct_ty.expect(&format!("{:#?}", agg_or_ptr));

                let ptr = self.builder.build_pointer_cast(
                    agg_or_ptr,
                    struct_ty.ptr_type(AddressSpace::Generic),
                    "",
                );

                self.builder
                    .build_struct_gep(ptr, index, &format!("get_struct_ptr_2_{}", index))
                    .expect(&format!("{:#?}", ptr))
                    .into()
            }
        } else {
            self.builder
                .build_extract_value(
                    agg_or_ptr.into_struct_value(),
                    index,
                    &format!("get_struct_{}", index),
                )
                .expect(&format!("{:#?}", agg_or_ptr))
        }
    }

    pub(super) fn gen_subscript(
        &mut self,
        agg: BasicValueEnum<'ctx>,
        agg_ty: &Ty,
        index: IntValue<'ctx>,
        deref: bool,
    ) -> BasicValueEnum<'ctx> {
        let ty = agg_ty.maybe_deref_once();

        let agg = match ty {
            Ty::Array(..) | Ty::MultiPointer(..) => {
                self.maybe_load_double_pointer(agg.into_pointer_value())
            }
            Ty::Slice(..) => {
                let value = if agg.is_pointer_value() {
                    self.maybe_load_double_pointer(agg.into_pointer_value())
                        .into()
                } else {
                    agg
                };

                let agg = self.gen_load_slice_data(value.into());
                self.build_load(agg.into()).into_pointer_value()
            }

            ty => unreachable!("{}", ty),
        };

        let access = unsafe {
            match ty {
                Ty::Array(..) => self.builder.build_gep(
                    agg,
                    &[index.get_type().const_zero(), index],
                    "array_subscript",
                ),
                Ty::MultiPointer(..) | Ty::Slice(..) => {
                    self.builder.build_gep(agg, &[index], "subscript")
                }
                ty => unreachable!("{}", ty),
            }
        };

        if deref {
            self.build_load(access.into())
        } else {
            access.into()
        }
    }
}

pub(super) fn is_a_load_inst<'ctx>(value: BasicValueEnum<'ctx>) -> bool {
    if let Some(inst) = value.as_instruction_value() {
        inst.is_a_load_inst()
    } else {
        false
    }
}

pub(super) trait IsAggregateType {
    fn is_aggregate_type(&self) -> bool;
}

impl<'ctx> IsAggregateType for AnyTypeEnum<'ctx> {
    fn is_aggregate_type(&self) -> bool {
        match self {
            AnyTypeEnum::ArrayType(_) | AnyTypeEnum::StructType(_) => true,
            _ => false,
        }
    }
}

impl<'ctx> IsAggregateType for BasicTypeEnum<'ctx> {
    fn is_aggregate_type(&self) -> bool {
        self.as_any_type_enum().is_aggregate_type()
    }
}

pub(crate) trait LlvmName {
    fn llvm_name(&self, module_name: impl AsRef<str>) -> String;
}

impl LlvmName for FnSig {
    fn llvm_name(&self, module_name: impl AsRef<str>) -> String {
        if self.lib_name.is_some() {
            self.name.to_string()
        } else {
            format!("{}.{}", module_name.as_ref(), self.name)
        }
    }
}
