use std::fmt::Display;

use chili_ast::ast;

use crate::value::ValueKind;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Instruction {
    Noop,
    Pop,
    PushConst(u32),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    Not,
    Deref,
    Eq,
    Neq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Jmp(i32),
    Jmpt(i32),
    Jmpf(i32),
    Return,
    Call(u32),
    GetGlobal(u32),
    GetGlobalPtr(u32),
    SetGlobal(u32),
    Peek(i32),
    PeekPtr(i32),
    SetLocal(i32),
    Index,
    IndexPtr,
    ConstIndex(u32),
    ConstIndexPtr(u32),
    Assign,
    Cast(CastInstruction),
    AggregateAlloc,
    AggregatePush,
    Copy,
    Increment,
    Halt,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum CastInstruction {
    I8,
    I16,
    I32,
    I64,
    Int,
    U8,
    U16,
    U32,
    U64,
    Uint,
    F32,
    F64,
    Ptr(ValueKind),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Instruction::Noop => "noop".to_string(),
                Instruction::Pop => "pop".to_string(),
                Instruction::PushConst(addr) => format!("push_const %{}", addr),
                Instruction::Add => "add".to_string(),
                Instruction::Sub => "sub".to_string(),
                Instruction::Mul => "mul".to_string(),
                Instruction::Div => "div".to_string(),
                Instruction::Rem => "mod".to_string(),
                Instruction::Neg => "neg".to_string(),
                Instruction::Not => "not".to_string(),
                Instruction::Deref => "deref".to_string(),
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
                Instruction::Call(arg_count) => format!("call {}", arg_count),
                Instruction::GetGlobal(slot) => format!("get_global ${}", slot),
                Instruction::GetGlobalPtr(slot) => format!("get_global_ptr ${}", slot),
                Instruction::SetGlobal(slot) => format!("set_global ${}", slot),
                Instruction::Peek(slot) => format!("peek ${}", slot),
                Instruction::PeekPtr(slot) => format!("peek_ptr ${}", slot),
                Instruction::SetLocal(slot) => format!("set_local ${}", slot),
                Instruction::Index => "index".to_string(),
                Instruction::IndexPtr => "index_ptr".to_string(),
                Instruction::ConstIndex(index) => format!("const_index {}", index),
                Instruction::ConstIndexPtr(index) => format!("const_index_ptr {}", index),
                Instruction::Assign => "assign".to_string(),
                Instruction::Cast(cast) => format!("cast {:?}", cast),
                Instruction::AggregateAlloc => "aggregate_alloc".to_string(),
                Instruction::AggregatePush => "aggregate_push".to_string(),
                Instruction::Copy => "copy".to_string(),
                Instruction::Increment => "increment".to_string(),
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
            ast::UnaryOp::Ref(_) => unimplemented!(),
            ast::UnaryOp::Deref => Instruction::Deref,
            ast::UnaryOp::Neg => Instruction::Neg,
            ast::UnaryOp::Plus => Instruction::Noop,
            ast::UnaryOp::Not => Instruction::Not,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompiledCode {
    pub instructions: Vec<Instruction>,
    pub locals: u16,
}

impl CompiledCode {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            locals: 0,
        }
    }

    #[inline]
    pub fn push(&mut self, inst: Instruction) -> usize {
        self.instructions.push(inst);
        self.instructions.len() - 1
    }
}
