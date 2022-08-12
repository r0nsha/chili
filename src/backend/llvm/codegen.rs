use super::{
    abi::{align_of, size_of},
    traits::IsALoadInst,
};
use crate::{
    ast::ExternLibrary,
    common::{build_options, scopes::Scopes, target::TargetMetrics},
    hir,
    infer::{normalize::Normalize, type_ctx::TypeCtx},
    types::*,
    workspace::{BindingId, BindingInfo, ModuleId, ModuleInfo, Workspace},
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::{PassManager, PassManagerBuilder},
    types::{BasicTypeEnum, IntType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, InstructionOpcode, PointerValue},
    OptimizationLevel,
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};
use ustr::{Ustr, UstrMap};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Decl<'ctx> {
    Function(FunctionValue<'ctx>),
    Global(GlobalValue<'ctx>),
    Local(PointerValue<'ctx>),
}

impl<'ctx> Decl<'ctx> {
    pub(super) fn into_function_value(&self) -> FunctionValue<'ctx> {
        match self {
            Decl::Function(f) => *f,
            _ => panic!("can't get function value of {:?}", self),
        }
    }

    #[allow(unused)]
    pub(super) fn into_global_value(&self) -> GlobalValue<'ctx> {
        match self {
            Decl::Global(g) => *g,
            _ => panic!("can't get global value of {:?}", self),
        }
    }

    pub(super) fn into_pointer_value(&self) -> PointerValue<'ctx> {
        match self {
            Decl::Function(f) => f.as_global_value().as_pointer_value(),
            Decl::Global(g) => g.as_pointer_value(),
            Decl::Local(p) => *p,
        }
    }
}

pub(super) struct Generator<'g, 'ctx> {
    pub(super) workspace: &'g Workspace,
    pub(super) tcx: &'g TypeCtx,
    pub(super) cache: &'g hir::Cache,

    pub(super) target_metrics: TargetMetrics,

    pub(super) context: &'ctx Context,
    pub(super) module: &'g Module<'ctx>,
    pub(super) builder: &'g Builder<'ctx>,
    pub(super) ptr_sized_int_type: IntType<'ctx>,

    pub(super) global_decls: HashMap<BindingId, Decl<'ctx>>,
    pub(super) types: HashMap<BindingId, BasicTypeEnum<'ctx>>,
    pub(super) static_strs: UstrMap<PointerValue<'ctx>>,

    pub(super) functions: HashMap<hir::FunctionId, FunctionValue<'ctx>>,

    pub(super) extern_functions: UstrMap<FunctionValue<'ctx>>,
    pub(super) extern_variables: UstrMap<GlobalValue<'ctx>>,
    pub(super) extern_libraries: HashSet<ExternLibrary>,

    pub(super) intrinsics: HashMap<hir::Intrinsic, FunctionValue<'ctx>>,

    // This is an Option since it is only initialized after
    // creating the startup function's state
    pub(super) startup_function_state: Option<FunctionState<'ctx>>,
}

#[derive(Clone)]
pub(super) struct FunctionState<'ctx> {
    pub(super) module_info: ModuleInfo,
    pub(super) function: FunctionValue<'ctx>,
    pub(super) fn_type: FunctionType,
    pub(super) return_ptr: Option<PointerValue<'ctx>>,
    pub(super) loop_blocks: Vec<LoopBlock<'ctx>>,
    pub(super) decl_block: BasicBlock<'ctx>,
    pub(super) current_block: BasicBlock<'ctx>,
    pub(super) scopes: Scopes<BindingId, Decl<'ctx>>,
}

impl<'ctx> FunctionState<'ctx> {
    pub(super) fn new(
        module_info: ModuleInfo,
        function: FunctionValue<'ctx>,
        fn_type: FunctionType,
        return_ptr: Option<PointerValue<'ctx>>,
        decl_block: BasicBlock<'ctx>,
        entry_block: BasicBlock<'ctx>,
    ) -> Self {
        Self {
            module_info,
            function,
            fn_type,
            return_ptr,
            loop_blocks: vec![],
            decl_block,
            current_block: entry_block,
            scopes: Scopes::default(),
        }
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push_scope();
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop_scope();
    }
}

#[derive(Clone, Copy)]
pub(super) struct LoopBlock<'ctx> {
    pub(super) head: BasicBlock<'ctx>,
    pub(super) exit: BasicBlock<'ctx>,
}

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn start(&mut self) {
        self.gen_start_function();
    }

    pub(super) fn optimize(&mut self) {
        let pass_manager_builder = PassManagerBuilder::create();

        let optimization_level: OptimizationLevel = self.workspace.build_options.optimization_level.into();

        let size_level: u32 = match self.workspace.build_options.optimization_level {
            build_options::OptimizationLevel::Debug => 1,
            build_options::OptimizationLevel::Release => 2,
        };

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);
        pass_manager.run_on(&self.module);

        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);
        link_time_optimizations.run_on(&self.module);
    }

    pub(super) fn gen_top_level_binding(&mut self, id: BindingId) -> Decl<'ctx> {
        if let Some(decl) = self.global_decls.get(&id) {
            return *decl;
        } else if let Some(binding) = self.cache.bindings.get(&id) {
            binding.codegen_global(self)
        } else {
            panic!("{:#?}", self.workspace.binding_infos.get(id))
        }
    }

    pub(super) fn insert_global_decl(&mut self, id: BindingId, decl: Decl<'ctx>) -> Decl<'ctx> {
        self.global_decls.insert(id, decl);
        decl
    }

    pub(super) fn add_global(&mut self, id: BindingId, ty: BasicTypeEnum<'ctx>, linkage: Linkage) -> GlobalValue<'ctx> {
        let binding_info = self.workspace.binding_infos.get(id).unwrap();
        let global_value = self.module.add_global(ty, None, &binding_info.name);
        global_value.set_linkage(linkage);
        global_value
    }

    pub(super) fn find_binding_info_by_name(
        &mut self,
        module_name: impl Into<Ustr>,
        name: impl Into<Ustr>,
    ) -> &BindingInfo {
        let module_name: Ustr = module_name.into();
        let name: Ustr = name.into();

        let module_id = self
            .workspace
            .module_infos
            .iter()
            .position(|(_, m)| m.name == module_name)
            .map(ModuleId::from)
            .unwrap_or_else(|| panic!("couldn't find {}", module_name));

        self.workspace
            .binding_infos
            .iter()
            .map(|(_, b)| b)
            .find(|b| b.module_id == module_id && b.name == name)
            .unwrap_or_else(|| panic!("couldn't find {} in {}", name, module_name))
    }

    pub(super) fn find_decl_by_name(&mut self, module_name: impl Into<Ustr>, name: impl Into<Ustr>) -> Decl<'ctx> {
        let id = self.find_binding_info_by_name(module_name, name).id;
        self.gen_top_level_binding(id)
    }

    pub(super) fn gen_local(
        &mut self,
        state: &mut FunctionState<'ctx>,
        id: BindingId,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        debug_assert!(id != BindingId::unknown());

        let binding_info = self.workspace.binding_infos.get(id).unwrap();
        let ty = binding_info.ty.normalize(self.tcx);

        // Hack: This handles the weird alloca behavior of unsized types
        let value = match value.as_instruction_value() {
            Some(inst) if inst.get_opcode() == InstructionOpcode::Alloca && ty.is_fat_pointer() => {
                self.build_load(value.into_pointer_value())
            }
            _ => value,
        };

        let llvm_type = value.get_type();
        let ptr = self.build_alloca_named(state, llvm_type, id);

        self.build_store(ptr, value);

        ptr.set_name(&binding_info.name);

        let align = align_of(
            ptr.get_type().get_element_type().try_into().unwrap(),
            self.target_metrics.word_size,
        );

        ptr.as_instruction_value().unwrap().set_alignment(align as u32).unwrap();

        state.scopes.insert(id, Decl::Local(ptr));

        ptr
    }

    pub(super) fn gen_return(&mut self, state: &mut FunctionState<'ctx>, value: Option<BasicValueEnum<'ctx>>) {
        let abi_fn = self.get_abi_compliant_fn(&state.fn_type);

        if abi_fn.ret.kind.is_indirect() {
            let return_ptr = state.return_ptr.unwrap();

            match value {
                Some(value) => {
                    let size = size_of(abi_fn.ret.ty, self.target_metrics.word_size);
                    if value.is_a_load_inst() && size > self.target_metrics.word_size {
                        let ptr = self.build_alloca_or_load_addr(state, value);
                        self.build_copy_nonoverlapping(
                            ptr,
                            return_ptr,
                            self.ptr_sized_int_type.const_int(size as u64, true),
                        );
                    } else {
                        self.build_store(return_ptr, value);
                    }
                }
                _ => {
                    self.build_store(return_ptr, self.const_unit());
                }
            };

            self.builder.build_return(None);
        } else {
            let value = value.unwrap_or_else(|| self.const_unit());

            let value = self.build_transmute(state, value, state.function.get_type().get_return_type().unwrap());

            self.builder.build_return(Some(&value));
        }
    }
}

pub(super) trait Codegen<'g, 'ctx>
where
    Self: Debug,
{
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx>;

    fn codegen_global(&self, _generator: &mut Generator<'g, 'ctx>) -> Decl<'ctx> {
        unimplemented!("{:?}", self)
    }
}
