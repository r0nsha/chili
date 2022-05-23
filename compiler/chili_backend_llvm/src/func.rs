use crate::{
    abi::{AbiFn, AbiTyKind},
    codegen::{Codegen, CodegenState},
    ty::IntoLlvmType,
    util::LlvmName,
    CallingConv,
};
use chili_ast::{
    ast,
    ty::*,
    workspace::{BindingInfoId, ModuleInfo},
};
use chili_infer::normalize::NormalizeTy;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    module::Linkage,
    types::{AnyType, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue, FunctionValue,
        PointerValue,
    },
};

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_fn(
        &mut self,
        module_info: ModuleInfo,
        func: &ast::Fn,
        prev_state: Option<CodegenState<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let prev_block = if let Some(ref prev_state) = prev_state {
            Some(prev_state.curr_block)
        } else {
            self.builder.get_insert_block()
        };

        let function = self
            .module
            .get_function(&func.sig.llvm_name(module_info.name))
            .unwrap_or_else(|| self.declare_fn_sig(module_info, &func.sig));

        let decl_block = self.context.append_basic_block(function, "decls");
        let entry_block = self.context.append_basic_block(function, "entry");

        let fn_ty = func.sig.ty.normalize(self.tycx).into_fn();
        let abi_fn = self.fn_types.get(&fn_ty).unwrap().clone();

        let return_ptr = if abi_fn.ret.kind.is_indirect() {
            let return_ptr = function.get_first_param().unwrap().into_pointer_value();
            return_ptr.set_name(&format!("{}.result", func.sig.name));
            Some(return_ptr)
        } else {
            None
        };

        let mut state = CodegenState::new(
            module_info,
            function,
            fn_ty,
            return_ptr,
            decl_block,
            entry_block,
        );

        if let Some(prev_state) = prev_state {
            state.scopes = prev_state.scopes;
        }

        self.start_block(&mut state, entry_block);

        state.push_scope();

        let mut params = function.get_params();

        if abi_fn.ret.kind.is_indirect() {
            params.remove(0);
        }

        for (index, (&value, param)) in params.iter().zip(func.sig.params.iter()).enumerate() {
            let value = if abi_fn.params[index].kind.is_indirect() {
                self.build_load(value)
            } else {
                value
            };

            let param_ty = match param.ty.normalize(self.tycx) {
                TyKind::Type(inner) => *inner,
                t => t,
            };

            let llvm_param_ty = param_ty.llvm_type(self);

            let value = self.build_transmute(&state, value, llvm_param_ty);

            self.gen_binding_pattern_with_value(&mut state, &param.pattern, param_ty, value);
        }

        let return_value: BasicValueEnum = self.gen_block(&mut state, &func.body, true);

        if self.current_block().get_terminator().is_none() {
            self.gen_return(&mut state, Some(return_value), &[]);
        }

        state.pop_scope();

        self.start_block(&mut state, decl_block);
        self.builder.build_unconditional_branch(entry_block);

        let function_value = self.verify_and_optimize_function(function, &func.sig.name);

        if let Some(prev_block) = prev_block {
            self.builder.position_at_end(prev_block);
        }

        function_value
    }

    pub(crate) fn verify_and_optimize_function(
        &mut self,
        function: FunctionValue<'ctx>,
        name: &str,
    ) -> FunctionValue<'ctx> {
        if function.verify(true) {
            self.fpm.run_on(&function);
            function
        } else {
            println!("\nThis is the generated function:");
            function.print_to_stderr();
            panic!("Invalid generated function `{}`", name);
        }
    }

    pub(super) fn declare_fn_sig(
        &mut self,
        module_info: ModuleInfo,
        sig: &ast::FnSig,
    ) -> FunctionValue<'ctx> {
        let fn_sig_ty = sig.ty.normalize(self.tycx).into_fn();
        let fn_type = self.fn_type(&fn_sig_ty);
        let abi_fn = self.fn_types.get(&fn_sig_ty).unwrap();

        let llvm_name = sig.llvm_name(module_info.name);

        let function = self.get_or_add_function(llvm_name, fn_type, Some(Linkage::External));

        self.add_fn_attributes(function, abi_fn);

        function
    }

    fn add_fn_attributes(&self, function: FunctionValue<'ctx>, abi_fn: &AbiFn<'ctx>) {
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

    pub(super) fn gen_fn_call_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        call: &ast::Call,
        result_ty: Ty,
    ) -> BasicValueEnum<'ctx> {
        let fn_ty = call.callee.ty.normalize(self.tycx).into_fn();
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

        let callee_ptr = self.maybe_load_double_pointer(callee_ptr);

        // println!("callee: {:#?}", callee_ptr.get_type());

        let callable_value: CallableValue = callee_ptr.try_into().unwrap();

        self.gen_fn_call(
            state,
            callable_value,
            &fn_ty,
            args,
            &result_ty.normalize(self.tycx),
        )
    }

    pub(super) fn gen_fn_call(
        &mut self,
        state: &mut CodegenState<'ctx>,
        callee: impl Into<CallableValue<'ctx>>,
        callee_ty: &FnTy,
        args: Vec<BasicValueEnum<'ctx>>,
        result_ty: &TyKind,
    ) -> BasicValueEnum<'ctx> {
        let abi_fn = self
            .fn_types
            .get(callee_ty)
            .unwrap_or_else(|| panic!("not found: {}", callee_ty))
            .clone();

        let mut processed_args = vec![];

        let mut param_index = 0;
        for (index, param) in abi_fn.params.iter().enumerate() {
            let arg = args[index];

            let arg = match param.kind {
                AbiTyKind::Direct => {
                    // println!("...direct");
                    let abi_ty = match param.cast_to {
                        Some(cast_to) => cast_to,
                        None => param.ty,
                    };
                    let arg = self.build_transmute(state, arg, abi_ty);
                    arg
                }
                AbiTyKind::Indirect => {
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
                    } else if callee_ty.lib_name.is_none() {
                        self.gen_local_or_load_addr(state, BindingInfoId::unknown(), arg)
                    } else {
                        self.build_copy_value_to_ptr(state, arg, arg_type, 16)
                    };

                    arg.into()
                }
                AbiTyKind::Ignore => unimplemented!("ignore '{:?}'", param.ty),
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

    pub(super) fn get_or_add_function(
        &self,
        name: impl AsRef<str>,
        fn_type: FunctionType<'ctx>,
        linkage: Option<Linkage>,
    ) -> FunctionValue<'ctx> {
        let name = name.as_ref();
        self.module
            .get_function(name)
            .unwrap_or(self.module.add_function(name, fn_type, linkage))
    }
}
