use crate::{
    ffi::Ffi,
    lower::{Lower, LowerContext},
    vm::display::dump_bytecode_to_file,
    vm::instruction::{CompiledCode, Instruction},
    vm::value::{Func, Value},
    vm::{Constants, Globals, VM},
};
use chili_ast::{
    ast,
    ty::TyKind,
    workspace::{BindingInfoId, ModuleId, Workspace},
};
use chili_infer::ty_ctx::TyCtx;
use common::scopes::Scopes;
use std::collections::HashMap;
use ustr::{ustr, Ustr};

pub type InterpResult = Result<Value, InterpErr>;

#[derive(Debug)]
pub enum InterpErr {}

pub struct Interp {
    pub(crate) globals: Globals,
    pub(crate) constants: Constants,
    pub(crate) ffi: Ffi,

    bindings_to_globals: HashMap<BindingInfoId, usize>,
}

impl Interp {
    pub fn new() -> Self {
        Self {
            globals: vec![],
            constants: vec![Value::unit()],
            ffi: Ffi::new(),
            bindings_to_globals: HashMap::new(),
        }
    }

    pub fn create_session<'i>(
        &'i mut self,
        workspace: &'i Workspace,
        tycx: &'i TyCtx,
        typed_ast: &'i ast::TypedAst,
    ) -> InterpSess<'i> {
        InterpSess {
            interp: self,
            workspace,
            tycx,
            typed_ast,
            env_stack: vec![],
            evaluated_globals: vec![],
        }
    }
}

pub struct InterpSess<'i> {
    pub(crate) interp: &'i mut Interp,
    pub(crate) workspace: &'i Workspace,
    pub(crate) tycx: &'i TyCtx,
    pub(crate) typed_ast: &'i ast::TypedAst,
    pub(crate) env_stack: Vec<(ModuleId, Env)>,

    // globals to be evaluated when the VM starts
    pub(crate) evaluated_globals: Vec<(usize, CompiledCode)>,
}

pub type Env = Scopes<BindingInfoId, i16>;

impl<'i> InterpSess<'i> {
    pub fn eval(&'i mut self, expr: &ast::Expr, module_id: ModuleId) -> InterpResult {
        let verbose = self.workspace.build_options.verbose;
        let mut code = CompiledCode::new();

        self.env_stack.push((module_id, Env::default()));

        // lower expression tree into instructions
        expr.lower(self, &mut code, LowerContext { take_ptr: false });
        code.push(Instruction::Halt);

        let code = self.insert_init_instructions(code);

        self.env_stack.pop();

        if verbose {
            dump_bytecode_to_file(&self.interp.globals, &self.interp.constants, &code);
        }

        let mut vm = self.create_vm();

        let result = vm.run_code(code);

        Ok(result)
    }

    // pushes initialization instructions such as global evaluation to the start
    fn insert_init_instructions(&mut self, mut code: CompiledCode) -> CompiledCode {
        let mut init_instructions: Vec<Instruction> = vec![];

        for (global_index, global_code) in self.evaluated_globals.clone() {
            let const_slot = self.interp.constants.len();
            self.interp.constants.push(Value::Func(Func {
                name: ustr(&format!("global_init_{}", global_index)),
                arg_types: vec![],
                return_type: TyKind::Unit,
                code: global_code,
            }));
            init_instructions.push(Instruction::PushConst(const_slot as u32));
            init_instructions.push(Instruction::Call(0));
            init_instructions.push(Instruction::SetGlobal(global_index as u32));
        }

        code.instructions = init_instructions
            .into_iter()
            .chain(code.instructions)
            .collect();

        code
    }

    pub(crate) fn create_vm(&'i mut self) -> VM<'i> {
        VM::new(self.interp)
    }

    pub(crate) fn push_const(&mut self, code: &mut CompiledCode, value: Value) {
        let slot = self.interp.constants.len();
        self.interp.constants.push(value);
        code.push(Instruction::PushConst(slot as u32));
    }

    pub(crate) fn push_const_unit(&mut self, code: &mut CompiledCode) {
        // to avoid redundancy, when pushing a unit value,
        // we just use the first value in the constants vec
        code.push(Instruction::PushConst(0));
    }

    pub(crate) fn insert_global(&mut self, id: BindingInfoId, value: Value) -> usize {
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

    pub(crate) fn get_global(&self, id: BindingInfoId) -> Option<usize> {
        self.interp.bindings_to_globals.get(&id).cloned()
    }

    pub(crate) fn module_id(&self) -> ModuleId {
        self.env_stack.last().unwrap().0
    }

    pub(crate) fn env(&self) -> &Env {
        &self.env_stack.last().unwrap().1
    }

    pub(crate) fn env_mut(&mut self) -> &mut Env {
        &mut self.env_stack.last_mut().unwrap().1
    }

    pub(crate) fn find_symbol(&self, module_id: ModuleId, symbol: Ustr) -> BindingInfoId {
        BindingInfoId(
            self.workspace
                .binding_infos
                .iter()
                .position(|info| info.module_id == module_id && info.symbol == symbol)
                .unwrap_or_else(|| {
                    panic!(
                        "couldn't find member `{}` in module `{}`",
                        self.workspace.get_module_info(module_id).unwrap().name,
                        symbol
                    )
                }),
        )
    }

    pub(crate) fn add_local(&mut self, code: &mut CompiledCode, id: BindingInfoId) {
        code.locals += 1;
        self.env_mut().insert(id, code.locals as i16);
    }
}
