use super::{
    abi::{AbiFunction, AbiType},
    codegen::{Codegen, CodegenState},
    traits::LlvmName,
    ty::IntoLlvmType,
    CallingConv,
};
use crate::ast::{
    ast::{self, FunctionId},
    ty::*,
    workspace::BindingId,
};
use crate::infer::normalize::Normalize;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    module::Linkage,
    types::AnyType,
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue, FunctionValue,
        PointerValue,
    },
};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub fn gen_function(
        &mut self,
        id: FunctionId,
        prev_state: Option<CodegenState<'ctx>>,
    ) -> FunctionValue<'ctx> {
        self.functions.get(&id).cloned().unwrap_or_else(|| {
            let function = self.typed_ast.functions.get(id).unwrap();
            self.gen_function_inner(function, prev_state)
        })
    }

    fn gen_function_inner(
        &mut self,
        function: &ast::Function,
        prev_state: Option<CodegenState<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let module_info = *self.workspace.get_module_info(function.module_id).unwrap();
        let function_type = function.ty.normalize(self.tycx).into_fn();

        match &function.kind {
            ast::FunctionKind::Orphan { sig, body } => {
                let prev_block = if let Some(ref prev_state) = prev_state {
                    Some(prev_state.curr_block)
                } else {
                    self.builder.get_insert_block()
                };

                let function_value = self.declare_fn_sig(
                    &function_type,
                    sig.llvm_name(module_info.name),
                    Some(Linkage::Private),
                );

                self.functions.insert(function.id, function_value);

                let decl_block = self.context.append_basic_block(function_value, "decls");
                let entry_block = self.context.append_basic_block(function_value, "entry");

                let abi_fn = self.get_abi_compliant_fn(&function_type);

                let return_ptr = if abi_fn.ret.kind.is_indirect() {
                    let return_ptr = function_value
                        .get_first_param()
                        .unwrap()
                        .into_pointer_value();
                    return_ptr.set_name(&format!("{}.result", sig.name));
                    Some(return_ptr)
                } else {
                    None
                };

                let mut state = CodegenState::new(
                    module_info,
                    function_value,
                    function_type,
                    return_ptr,
                    decl_block,
                    entry_block,
                );

                if let Some(prev_state) = prev_state {
                    state.scopes = prev_state.scopes;
                }

                self.start_block(&mut state, entry_block);

                state.push_scope();

                let mut params = function_value.get_params();

                if abi_fn.ret.kind.is_indirect() {
                    params.remove(0);
                }

                for (index, (&value, param)) in params.iter().zip(sig.params.iter()).enumerate() {
                    let value = if abi_fn.params[index].kind.is_indirect() {
                        self.build_load(value)
                    } else {
                        value
                    };

                    let param_ty = match param.ty.normalize(self.tycx) {
                        Type::Type(inner) => *inner,
                        t => t,
                    };

                    let llvm_param_ty = param_ty.llvm_type(self);

                    let value = self.build_transmute(&state, value, llvm_param_ty);

                    self.gen_binding_pattern_with_value(
                        &mut state,
                        &param.pattern,
                        &param_ty,
                        value,
                    );
                }

                let return_value = self.gen_block(&mut state, body.as_ref().unwrap(), true);

                if self.current_block().get_terminator().is_none() {
                    self.gen_return(&mut state, Some(return_value));
                }

                state.pop_scope();

                self.start_block(&mut state, decl_block);
                self.builder.build_unconditional_branch(entry_block);

                if let Some(prev_block) = prev_block {
                    self.builder.position_at_end(prev_block);
                }

                function_value
            }
            ast::FunctionKind::Extern { name, lib: _ } => {
                self.extern_functions.get(name).cloned().unwrap_or_else(|| {
                    let function_type = self.fn_type(&function_type);
                    let function = self.get_or_add_function(name, function_type, None);
                    self.extern_functions.insert(*name, function);
                    function
                })
            }
            ast::FunctionKind::Intrinsic(intrinsic) => {
                self.gen_intrinsic(intrinsic, &function_type)
            }
        }
    }

    pub fn declare_fn_sig(
        &mut self,
        ty: &FunctionType,
        name: impl AsRef<str>,
        linkage: Option<Linkage>,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.abi_compliant_fn_type(&ty);
        let abi_fn = self.get_abi_compliant_fn(&ty);

        let function = self.get_or_add_function(name, fn_type, linkage);

        self.add_fn_attributes(function, &abi_fn);

        function
    }

    fn add_fn_attributes(&self, function: FunctionValue<'ctx>, abi_fn: &AbiFunction<'ctx>) {
        for (index, param) in abi_fn.params.iter().enumerate() {
            if param.kind.is_ignore() {
                continue;
            }

            if let Some(attr) = param.attr {
                function.add_attribute(AttributeLoc::Param(index as u32), attr);
            }

            if let Some(align_attr) = param.align_attr {
                function.add_attribute(AttributeLoc::Param(index as u32), align_attr);
            }
        }

        if abi_fn.ret.kind.is_indirect() && abi_fn.ret.attr.is_some() {
            // TODO: maybe this should be AttributeLoc::Return
            function.add_attribute(AttributeLoc::Param(0), abi_fn.ret.attr.unwrap());
            function.add_attribute(
                AttributeLoc::Param(0),
                self.context
                    .create_enum_attribute(Attribute::get_named_enum_kind_id("noalias"), 0),
            );
        }

        function.set_call_conventions(CallingConv::C as _);
    }

    pub fn gen_fn_call_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        call: &ast::Call,
        result_ty: TypeId,
    ) -> BasicValueEnum<'ctx> {
        let callee_ty = call.callee.ty.normalize(self.tycx).into_fn();

        let mut args = vec![];

        for (index, arg) in call.args.iter().enumerate() {
            let value = self.gen_expr(state, arg, true);
            args.push((value, index));
        }

        args.sort_by_key(|&(_, i)| i);

        let args: Vec<BasicValueEnum> = args.iter().map(|(a, _)| *a).collect();

        let callee_ptr = self
            .gen_expr(state, &call.callee, false)
            .into_pointer_value();

        // println!("callee: {:#?}", callee_ptr.get_type());

        // for arg in args.iter() {
        //     println!("arg: {:#?}", arg);
        // }

        let callable_value: CallableValue = callee_ptr.try_into().unwrap();

        self.gen_fn_call(
            state,
            callable_value,
            &callee_ty,
            args,
            &result_ty.normalize(self.tycx),
        )
    }

    pub fn gen_fn_call(
        &mut self,
        state: &mut CodegenState<'ctx>,
        callee: impl Into<CallableValue<'ctx>>,
        callee_ty: &FunctionType,
        args: Vec<BasicValueEnum<'ctx>>,
        result_ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let abi_fn = self.get_abi_compliant_fn(callee_ty);

        let mut processed_args = vec![];

        let mut param_index = 0;
        for (index, param) in abi_fn.params.iter().enumerate() {
            let arg = args[index];

            let arg = match param.kind {
                AbiType::Direct => {
                    // println!("...direct");
                    let abi_ty = match param.cast_to {
                        Some(cast_to) => cast_to,
                        None => param.ty,
                    };

                    self.build_transmute(state, arg, abi_ty)
                }
                AbiType::Indirect => {
                    // println!("...indirect");
                    let arg_type = arg.get_type();

                    let is_array = arg_type.is_pointer_type()
                        && arg_type
                            .into_pointer_type()
                            .get_element_type()
                            .is_array_type();

                    let arg = if is_array {
                        let ptr = self.build_alloca(
                            state,
                            arg_type
                                .into_pointer_type()
                                .get_element_type()
                                .try_into()
                                .unwrap(),
                        );

                        self.build_store(ptr, arg);

                        ptr
                    } else if !callee_ty.kind.is_extern() {
                        self.gen_local_or_load_addr(state, BindingId::unknown(), arg)
                    } else {
                        self.build_copy_value_to_ptr(state, arg, arg_type, 16)
                    };

                    arg.into()
                }
                AbiType::Ignore => unimplemented!("ignore '{:?}'", param.ty),
            };

            processed_args.push(arg.into());
            param_index += 1;
        }

        if abi_fn.variadic {
            for i in param_index..args.len() {
                processed_args.push(args[i]);
            }
        }

        let processed_args: Vec<BasicMetadataValueEnum<'ctx>> =
            processed_args.iter().map(|&a| a.into()).collect();

        let value = if abi_fn.ret.kind.is_indirect() {
            let return_ptr = self.build_alloca(state, abi_fn.ret.ty);
            return_ptr.set_name("__call_result");
            self.gen_call_inner(callee, processed_args, Some(return_ptr));
            self.build_load(return_ptr.into())
        } else {
            let value = self.gen_call_inner(callee, processed_args, None);
            let value = self.build_transmute(
                state,
                value,
                match abi_fn.ret.cast_to {
                    Some(cast_to) => cast_to,
                    None => abi_fn.ret.ty,
                },
            );
            value
        };

        let result_ty = result_ty.llvm_type(self);
        let value = self.build_transmute(state, value, result_ty);

        if callee_ty.ret.is_never() {
            self.build_unreachable();
        }

        value
    }

    fn gen_call_inner(
        &mut self,
        callee: impl Into<CallableValue<'ctx>>,
        mut args: Vec<BasicMetadataValueEnum<'ctx>>,
        return_ptr: Option<PointerValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        if let Some(return_ptr) = return_ptr {
            args.insert(0, return_ptr.into());
        }

        // args.iter().for_each(|arg| println!("arg: {:#?}", arg));

        let ret = self.builder.build_call(callee, &args, "call");

        if let Some(return_ptr) = return_ptr {
            ret.add_attribute(
                AttributeLoc::Param(0),
                self.context.create_type_attribute(
                    Attribute::get_named_enum_kind_id("sret"),
                    return_ptr.get_type().as_any_type_enum(),
                ),
            );
        }

        match ret.try_as_basic_value().left() {
            Some(value) => value,
            None => self.gen_unit(),
        }
    }

    pub fn get_or_add_function(
        &self,
        name: impl AsRef<str>,
        function_type: inkwell::types::FunctionType<'ctx>,
        linkage: Option<Linkage>,
    ) -> FunctionValue<'ctx> {
        let name = name.as_ref();
        self.module
            .get_function(name)
            .unwrap_or_else(|| self.module.add_function(name, function_type, linkage))
    }
}
