use crate::{
    instruction::{Bytecode, Instruction},
    lower::Lower,
    pretty_print::dump_bytecode_to_file,
    value::Value,
    vm::{Constants, Globals, VM},
};
use chili_ast::{
    ast,
    workspace::{BindingInfoId, ModuleId, Workspace},
};
use chili_infer::ty_ctx::TyCtx;
use common::{scopes::Scopes, time};
use std::collections::HashMap;
use ustr::Ustr;

pub type InterpResult = Result<Value, InterpErr>;

#[derive(Debug)]
pub enum InterpErr {}

pub struct Interp {
    pub(crate) globals: Globals,
    pub(crate) constants: Constants,

    bindings_to_globals: HashMap<BindingInfoId, usize>,
}

impl Interp {
    pub fn new() -> Self {
        Self {
            globals: vec![],
            constants: vec![],
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
        }
    }
}

pub struct InterpSess<'i> {
    pub(crate) interp: &'i mut Interp,
    pub(crate) workspace: &'i Workspace,
    pub(crate) tycx: &'i TyCtx,
    pub(crate) typed_ast: &'i ast::TypedAst,
    pub(crate) env_stack: Vec<(ModuleId, Env)>,
}

pub type Env = Scopes<BindingInfoId, isize>;

impl<'i> InterpSess<'i> {
    pub fn eval(&'i mut self, expr: &ast::Expr, module_id: ModuleId) -> InterpResult {
        let verbose = self.workspace.build_options.verbose;
        let mut code = Bytecode::new();

        self.env_stack.push((module_id, Env::default()));

        expr.lower(self, &mut code);
        code.push(Instruction::Halt);

        self.env_stack.pop();

        if verbose {
            dump_bytecode_to_file(&self.interp.globals, &self.interp.constants, &code);
        }

        let result = time! { verbose, "vm", {
            let mut vm = self.create_vm();
            vm.run(code)
        }};

        println!("result = {}", result);

        Ok(result)
    }

    pub(crate) fn create_vm(&'i mut self) -> VM<'i> {
        VM::new(self.interp)
    }

    pub(crate) fn push_const(&mut self, code: &mut Bytecode, value: Value) {
        let slot = self.interp.constants.len();
        self.interp.constants.push(value);
        code.push(Instruction::PushConst(slot as u32));
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
}
