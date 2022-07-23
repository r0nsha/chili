use crate::ast;
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Inst {
    Noop,
    Pop,
    LoadConst(u32),
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
    Cast,
    BufferAlloc(u32),
    BufferPut(u32),
    BufferFill(u32),
    Copy(u32),
    Swap(u32),
    Halt,
}

impl Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Inst::Noop => "noop".to_string(),
                Inst::Pop => "pop".to_string(),
                Inst::LoadConst(addr) => format!("load_const %{}", addr),
                Inst::Add => "add".to_string(),
                Inst::Sub => "sub".to_string(),
                Inst::Mul => "mul".to_string(),
                Inst::Div => "div".to_string(),
                Inst::Rem => "mod".to_string(),
                Inst::Neg => "neg".to_string(),
                Inst::Not => "not".to_string(),
                Inst::Deref => "deref".to_string(),
                Inst::Eq => "eq".to_string(),
                Inst::Ne => "ne".to_string(),
                Inst::Lt => "lt".to_string(),
                Inst::Le => "le".to_string(),
                Inst::Gt => "gt".to_string(),
                Inst::Ge => "ge".to_string(),
                Inst::And => "band".to_string(),
                Inst::Or => "bor".to_string(),
                Inst::Shl => "shl".to_string(),
                Inst::Shr => "shr".to_string(),
                Inst::Xor => "xor".to_string(),
                Inst::Jmp(offset) => format!("jmp {}", offset),
                Inst::Jmpf(offset) => format!("jmpf {}", offset),
                Inst::Return => "return".to_string(),
                Inst::Call(arg_count) => format!("call {}", arg_count),
                Inst::GetGlobal(slot) => format!("get_global ${}", slot),
                Inst::GetGlobalPtr(slot) => format!("get_global_ptr ${}", slot),
                Inst::SetGlobal(slot) => format!("set_global ${}", slot),
                Inst::Peek(slot) => format!("peek ${}", slot),
                Inst::PeekPtr(slot) => format!("peek_ptr ${}", slot),
                Inst::SetLocal(slot) => format!("set_local ${}", slot),
                Inst::Index => "index".to_string(),
                Inst::IndexPtr => "index_ptr".to_string(),
                Inst::Offset => "offset".to_string(),
                Inst::ConstIndex(index) => format!("const_index {}", index),
                Inst::ConstIndexPtr(index) => format!("const_index_ptr {}", index),
                Inst::Assign => "assign".to_string(),
                Inst::Cast => "cast".to_string(),
                Inst::BufferAlloc(size) => format!("buffer_alloc {}", size),
                Inst::BufferPut(pos) => format!("buffer_put {}", pos),
                Inst::BufferFill(size) => format!("buffer_fill {}", size),
                Inst::Copy(offset) => format!("copy {}", -(*offset as i32)),
                Inst::Swap(offset) => format!("swap {}", -(*offset as i32)),
                Inst::Halt => "halt".to_string(),
            }
        )
    }
}

impl From<ast::BinaryOp> for Inst {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => Inst::Add,
            ast::BinaryOp::Sub => Inst::Sub,
            ast::BinaryOp::Mul => Inst::Mul,
            ast::BinaryOp::Div => Inst::Div,
            ast::BinaryOp::Rem => Inst::Rem,
            ast::BinaryOp::Eq => Inst::Eq,
            ast::BinaryOp::Ne => Inst::Ne,
            ast::BinaryOp::Lt => Inst::Lt,
            ast::BinaryOp::Le => Inst::Le,
            ast::BinaryOp::Gt => Inst::Gt,
            ast::BinaryOp::Ge => Inst::Ge,
            ast::BinaryOp::And | ast::BinaryOp::BitAnd => Inst::And,
            ast::BinaryOp::Or | ast::BinaryOp::BitOr => Inst::Or,
            ast::BinaryOp::Shl => Inst::Shl,
            ast::BinaryOp::Shr => Inst::Shr,
            ast::BinaryOp::BitXor => Inst::Xor,
        }
    }
}

impl From<ast::UnaryOp> for Inst {
    fn from(op: ast::UnaryOp) -> Self {
        match op {
            ast::UnaryOp::Ref(_) => unimplemented!(),
            ast::UnaryOp::Deref => Inst::Deref,
            ast::UnaryOp::Neg => Inst::Neg,
            ast::UnaryOp::Plus => Inst::Noop,
            ast::UnaryOp::Not => Inst::Not,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompiledCode {
    pub instructions: Vec<Inst>,
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
    pub fn push(&mut self, inst: Inst) -> usize {
        self.instructions.push(inst);
        self.instructions.len() - 1
    }

    #[inline]
    pub fn last_local(&self) -> i32 {
        self.locals as i32 - 1
    }
}
