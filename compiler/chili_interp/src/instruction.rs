use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use chili_ast::ast;
use ustr::Ustr;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Instruction {
    Noop,
    Pop,
    Const(usize),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    Not,
    Eq,
    Neq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Jmp(isize),
    Jmpt(isize),
    Jmpf(isize),
    Return,
    Call(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(isize),
    SetLocal(isize),
    Access(Ustr),
    Index(usize),
    Halt,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Instruction::Noop => "noop".to_string(),
                Instruction::Pop => "pop".to_string(),
                Instruction::Const(addr) => format!("const %{}", addr),
                Instruction::Add => "add".to_string(),
                Instruction::Sub => "sub".to_string(),
                Instruction::Mul => "mul".to_string(),
                Instruction::Div => "div".to_string(),
                Instruction::Rem => "mod".to_string(),
                Instruction::Neg => "neg".to_string(),
                Instruction::Not => "not".to_string(),
                Instruction::Eq => "eq".to_string(),
                Instruction::Neq => "neq".to_string(),
                Instruction::Lt => "lt".to_string(),
                Instruction::LtEq => "lteq".to_string(),
                Instruction::Gt => "gt".to_string(),
                Instruction::GtEq => "gteq".to_string(),
                Instruction::And => "band".to_string(),
                Instruction::Or => "bor".to_string(),
                Instruction::Jmp(offset) => format!("jmp &{:06}", offset),
                Instruction::Jmpt(offset) => format!("jmpt &{:06}", offset),
                Instruction::Jmpf(offset) => format!("jmpf &{:06}", offset),
                Instruction::Return => "return".to_string(),
                Instruction::Call(arg_count) => format!("call ({})", arg_count),
                Instruction::GetGlobal(slot) => format!("get_global ${}", slot),
                Instruction::SetGlobal(slot) => format!("set_global ${}", slot),
                Instruction::GetLocal(slot) => format!("get_local ${}", slot),
                Instruction::SetLocal(slot) => format!("set_local ${}", slot),
                Instruction::Access(member) => format!("access `{}`", member),
                Instruction::Index(idx) => format!("index {}", idx),
                Instruction::Halt => "halt".to_string(),
            }
        )
    }
}

impl From<ast::BinaryOp> for Instruction {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => Instruction::Add,
            ast::BinaryOp::Sub => Instruction::Sub,
            ast::BinaryOp::Mul => Instruction::Mul,
            ast::BinaryOp::Div => Instruction::Div,
            ast::BinaryOp::Rem => Instruction::Rem,
            ast::BinaryOp::Eq => Instruction::Eq,
            ast::BinaryOp::Neq => Instruction::Neq,
            ast::BinaryOp::Lt => Instruction::Lt,
            ast::BinaryOp::LtEq => Instruction::LtEq,
            ast::BinaryOp::Gt => Instruction::Gt,
            ast::BinaryOp::GtEq => Instruction::GtEq,
            ast::BinaryOp::And => Instruction::And,
            ast::BinaryOp::Or => Instruction::Or,
            ast::BinaryOp::Shl => todo!("shl"),
            ast::BinaryOp::Shr => todo!("shr"),
            ast::BinaryOp::BitwiseAnd => Instruction::And,
            ast::BinaryOp::BitwiseOr => Instruction::Or,
            ast::BinaryOp::BitwiseXor => todo!("xor"),
        }
    }
}

impl From<ast::UnaryOp> for Instruction {
    fn from(op: ast::UnaryOp) -> Self {
        match op {
            ast::UnaryOp::Ref(_) => todo!("ref"),
            ast::UnaryOp::Deref => todo!("deref"),
            ast::UnaryOp::Neg => Instruction::Neg,
            ast::UnaryOp::Plus => Instruction::Noop,
            ast::UnaryOp::Not => Instruction::Not,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Bytecode(Vec<Instruction>);

impl Deref for Bytecode {
    type Target = Vec<Instruction>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Bytecode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Bytecode {
    pub fn new() -> Self {
        Self(vec![])
    }
}
