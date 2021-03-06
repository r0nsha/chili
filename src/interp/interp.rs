use super::{
    ffi::Ffi,
    lower::{Lower, LowerContext},
    vm::{
        display::dump_bytecode_to_file,
        instruction::{CompiledCode, Instruction},
        value::{ExternFunction, Function, FunctionAddress, FunctionValue, Value},
        Constants, Globals, VM,
    },
};
use crate::{
    common::{build_options::BuildOptions, scopes::Scopes},
    error::diagnostic::Diagnostic,
    hir,
    infer::type_ctx::TypeCtx,
    types::{FunctionType, FunctionTypeKind, Type},
    workspace::{BindingId, ModuleId, Workspace},
};
use std::collections::{HashMap, HashSet};
use ustr::{ustr, Ustr};

pub type InterpResult = Result<Value, Vec<Diagnostic>>;

pub struct Interp {
    pub globals: Globals,
    pub constants: Constants,

    pub functions: HashMap<hir::FunctionId, Function>,
    pub extern_functions: HashMap<hir::FunctionId, ExternFunction>,

    pub ffi: Ffi,
    pub build_options: BuildOptions,

    bindings_to_globals: HashMap<BindingId, usize>,
}

impl Interp {
    pub fn new(build_options: BuildOptions) -> Self {
        Self {
            globals: vec![],
            constants: vec![Value::unit()],
            functions: HashMap::new(),
            extern_functions: HashMap::new(),
            ffi: Ffi::new(),
            build_options,
            bindings_to_globals: HashMap::new(),
        }
    }

    pub fn create_session<'i>(
        &'i mut self,
        workspace: &'i Workspace,
        tcx: &'i TypeCtx,
        cache: &'i hir::Cache,
    ) -> InterpSess<'i> {
        InterpSess {
            interp: self,
            workspace,
            tcx,
            cache,
            diagnostics: vec![],
            env_stack: vec![],
            // labels: vec![],
            statically_initialized_globals: vec![],
            lowered_functions: HashSet::new(),
        }
    }

    pub fn get_function(&self, id: hir::FunctionId) -> Option<FunctionValue> {
        self.functions
            .get(&id)
            .map(FunctionValue::Orphan)
            .or_else(|| self.extern_functions.get(&id).map(FunctionValue::Extern))
    }
}

pub struct InterpSess<'i> {
    pub interp: &'i mut Interp,
    pub workspace: &'i Workspace,
    pub tcx: &'i TypeCtx,
    pub cache: &'i hir::Cache,

    pub diagnostics: Vec<Diagnostic>,
    pub env_stack: Vec<(ModuleId, Env)>,

    // pub labels: Vec<Label>,

    // Globals that are going to be statically initialized when the VM starts
    pub statically_initialized_globals: Vec<CompiledCode>,

    // Functions currently lowered, cached to prevent infinite recursion in recursive functions
    pub lowered_functions: HashSet<hir::FunctionId>,
}

// labels are used for patching call instruction after lowering
// pub struct Label {
//     instruction: *mut Instruction,
// }

pub type Env = Scopes<BindingId, i16>;

impl<'i> InterpSess<'i> {
    pub fn eval(&'i mut self, node: &hir::Node, module_id: ModuleId) -> InterpResult {
        let mut start_code = CompiledCode::new();

        self.env_stack.push((module_id, Env::default()));

        // lower expression tree into instructions
        node.lower(self, &mut start_code, LowerContext { take_ptr: false });

        if self.diagnostics.is_empty() {
            start_code.push(Instruction::Halt);

            let start_code = self.insert_init_instructions(start_code);

            self.env_stack.pop();

            if self.workspace.build_options.emit_bytecode {
                dump_bytecode_to_file(&self.interp, &start_code);
            }

            let mut vm = self.create_vm();

            let start_func = Function {
                id: hir::FunctionId::unknown(),
                name: ustr("__vm_start"),
                ty: FunctionType {
                    params: vec![],
                    return_type: Box::new(Type::Unit),
                    varargs: None,
                    kind: FunctionTypeKind::Orphan,
                },
                code: start_code,
            };

            let result = vm.run_func(start_func);

            Ok(result)
        } else {
            Err(self.diagnostics.clone())
        }
    }

    // pushes initialization instructions such as global evaluation to the start
    fn insert_init_instructions(&mut self, mut code: CompiledCode) -> CompiledCode {
        let mut init_instructions: Vec<Instruction> = vec![];

        for (i, global_eval_code) in self.statically_initialized_globals.iter().enumerate() {
            let const_slot = self.interp.constants.len();

            let id = hir::FunctionId::from(usize::MAX - i);
            let name = ustr(&format!("global_init_{}", i));

            self.interp.functions.insert(
                id,
                Function {
                    id,
                    name,
                    ty: FunctionType {
                        params: vec![],
                        return_type: Box::new(Type::Unit),
                        varargs: None,
                        kind: FunctionTypeKind::Orphan,
                    },
                    code: global_eval_code.clone(),
                },
            );

            self.interp
                .constants
                .push(Value::Function(FunctionAddress { id, name }));

            init_instructions.push(Instruction::PushConst(const_slot as u32));
            init_instructions.push(Instruction::Call(0));
        }

        code.instructions = init_instructions
            .into_iter()
            .chain(code.instructions)
            .collect();

        code
    }

    pub fn create_vm(&'i mut self) -> VM<'i> {
        VM::new(self.interp)
    }

    pub fn push_const(&mut self, code: &mut CompiledCode, value: Value) -> usize {
        let slot = self.interp.constants.len();
        self.interp.constants.push(value);
        code.push(Instruction::PushConst(slot as u32));
        slot
    }

    pub fn push_const_unit(&mut self, code: &mut CompiledCode) {
        // to avoid redundancy, when pushing a unit value,
        // we just use the first value in the constants vec
        code.push(Instruction::PushConst(0));
    }

    pub fn insert_global(&mut self, id: BindingId, value: Value) -> usize {
        if let Some(&slot) = self.interp.bindings_to_globals.get(&id) {
            self.interp.globals[slot] = value;
            slot
        } else {
            let slot = self.interp.globals.len();
            self.interp.globals.push(value);
            self.interp.bindings_to_globals.insert(id, slot);
            slot
        }
    }

    pub fn get_global(&self, id: BindingId) -> Option<usize> {
        self.interp.bindings_to_globals.get(&id).cloned()
    }

    #[allow(unused)]
    pub fn module_id(&self) -> ModuleId {
        self.env_stack.last().unwrap().0
    }

    pub fn env(&self) -> &Env {
        &self.env_stack.last().unwrap().1
    }

    pub fn env_mut(&mut self) -> &mut Env {
        &mut self.env_stack.last_mut().unwrap().1
    }

    #[allow(unused)]
    pub fn find_binding(&self, module_id: ModuleId, name: Ustr) -> BindingId {
        self.workspace
            .binding_infos
            .iter()
            .position(|(_, info)| info.module_id == module_id && info.name == name)
            .map(BindingId::from)
            .unwrap_or_else(|| {
                panic!(
                    "couldn't find member `{}` in module `{}`",
                    self.workspace.module_infos.get(module_id).unwrap().name,
                    name
                )
            })
    }

    pub fn add_local(&mut self, code: &mut CompiledCode, id: BindingId) {
        self.env_mut().insert(id, code.locals as i16);
        code.locals += 1;
    }
}
