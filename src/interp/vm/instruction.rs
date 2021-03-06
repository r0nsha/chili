use super::value::ValueKind;
use crate::ast;
use std::fmt::Display;

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
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Shl,
    Shr,
    Xor,
    Jmp(i32),
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
    Offset,
    ConstIndex(u32),
    ConstIndexPtr(u32),
    Assign,
    Cast(CastInstruction),
    BufferAlloc(u32),
    BufferPut(u32),
    BufferFill(u32),
    Copy(u32),
    Roll(u32),
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
                Instruction::Ne => "ne".to_string(),
                Instruction::Lt => "lt".to_string(),
                Instruction::Le => "le".to_string(),
                Instruction::Gt => "gt".to_string(),
                Instruction::Ge => "ge".to_string(),
                Instruction::And => "band".to_string(),
                Instruction::Or => "bor".to_string(),
                Instruction::Shl => "shl".to_string(),
                Instruction::Shr => "shr".to_string(),
                Instruction::Xor => "xor".to_string(),
                Instruction::Jmp(offset) => format!("jmp {}", offset),
                Instruction::Jmpf(offset) => format!("jmpf {}", offset),
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
                Instruction::Offset => "offset".to_string(),
                Instruction::ConstIndex(index) => format!("const_index {}", index),
                Instruction::ConstIndexPtr(index) => format!("const_index_ptr {}", index),
                Instruction::Assign => "assign".to_string(),
                Instruction::Cast(cast) => format!("cast {:?}", cast),
                Instruction::BufferAlloc(size) => format!("buffer_alloc {}", size),
                Instruction::BufferPut(pos) => format!("buffer_put {}", pos),
                Instruction::BufferFill(size) => format!("buffer_fill {}", size),
                Instruction::Copy(offset) => format!("copy {}", -(*offset as i32)),
                Instruction::Roll(offset) => format!("roll {}", -(*offset as i32)),
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
            ast::BinaryOp::Ne => Instruction::Ne,
            ast::BinaryOp::Lt => Instruction::Lt,
            ast::BinaryOp::Le => Instruction::Le,
            ast::BinaryOp::Gt => Instruction::Gt,
            ast::BinaryOp::Ge => Instruction::Ge,
            ast::BinaryOp::And | ast::BinaryOp::BitAnd => Instruction::And,
            ast::BinaryOp::Or | ast::BinaryOp::BitOr => Instruction::Or,
            ast::BinaryOp::Shl => Instruction::Shl,
            ast::BinaryOp::Shr => Instruction::Shr,
            ast::BinaryOp::BitXor => Instruction::Xor,
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

impl Default for CompiledCode {
    fn default() -> Self {
        Self::new()
    }
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

    #[inline]
    pub fn last_local(&self) -> i32 {
        self.locals as i32 - 1
    }
}
