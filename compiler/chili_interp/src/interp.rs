use crate::{
    dump_bytecode_to_file,
    instruction::Instruction,
    lower::Lower,
    value::Value,
    vm::{Bytecode, Globals, VM},
};
use chili_ast::ast;
use std::collections::HashMap;

pub type InterpResult = Result<Value, InterpErr>;

#[derive(Debug)]
pub enum InterpErr {}

pub struct Interp {
    pub(crate) globals: Globals,
    pub(crate) constants: Vec<Value>,
}

impl Interp {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            constants: vec![],
        }
    }

    pub fn create_session<'i>(&'i mut self, typed_ast: &'i ast::TypedAst) -> InterpSess<'i> {
        InterpSess {
            interp: self,
            typed_ast,
        }
    }
}

pub struct InterpSess<'i> {
    interp: &'i mut Interp,
    typed_ast: &'i ast::TypedAst,
}

impl<'i> InterpSess<'i> {
    pub fn eval(&mut self, expr: &ast::Expr) -> InterpResult {
        let mut code = vec![];
        expr.lower(self, &mut code);
        code.push(Instruction::Halt);

        dump_bytecode_to_file(&self.interp.globals, &self.interp.constants, &code);

        let mut vm = self.create_vm();
        let result = vm.run(code);

        println!("result = {}", result);

        Ok(result)
    }

    pub(crate) fn create_vm(&'i self) -> VM<'i> {
        VM::new(self.interp)
    }

    pub(crate) fn push_const(&mut self, code: &mut Bytecode, value: Value) {
        self.interp.constants.push(value);
        code.push(Instruction::Const(self.interp.constants.len() - 1));
    }
}
