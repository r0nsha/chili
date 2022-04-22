use std::collections::HashMap;

use chili_ast::{ast, value::Value, workspace::BindingInfoId};

use crate::vm::Bytecode;

pub type InterpResult = Result<Value, InterpErr>;

#[derive(Debug)]
pub enum InterpErr {}

pub struct Interp {
    globals: HashMap<BindingInfoId, Value>,
    functions: HashMap<BindingInfoId, Bytecode>,
}

impl Interp {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            functions: HashMap::new(),
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
        todo!("eval")
    }
}
