use crate::ast;
use byteorder::{NativeEndian, ReadBytesExt, WriteBytesExt};
use std::fmt::Display;
use std::mem;

pub struct Bytecode {
    buf: Vec<u8>,
    locals: u16,
}

impl Bytecode {
    #[inline(always)]
    pub fn write_op(&mut self, op: Op) -> usize {
        self.write_u8(op.into());
        self.buf.len() - 1
    }

    #[inline(always)]
    pub fn write_u8(&mut self, x: u8) {
        self.buf.write_u8(x).unwrap();
    }

    #[inline(always)]
    pub fn write_u16(&mut self, x: u16) {
        self.buf.write_u16::<NativeEndian>(x).unwrap();
    }

    #[inline(always)]
    pub fn write_i16(&mut self, x: i16) {
        self.buf.write_i16::<NativeEndian>(x).unwrap();
    }

    #[inline(always)]
    pub fn write_u32(&mut self, x: u32) {
        self.buf.write_u32::<NativeEndian>(x).unwrap();
    }

    #[inline(always)]
    pub fn write_i32(&mut self, x: i32) {
        self.buf.write_i32::<NativeEndian>(x).unwrap();
    }

    pub fn reader(&self) -> BytecodeReader {
        BytecodeReader {
            bytecode: self,
            pointer: 0,
        }
    }
}

pub struct BytecodeReader<'a> {
    bytecode: &'a Bytecode,
    pointer: usize,
}

impl<'a> BytecodeReader<'a> {
    #[inline(always)]
    pub fn set_pointer(&mut self, to: usize) {
        self.pointer = to;
    }

    #[inline(always)]
    pub fn read_op(&mut self) -> Op {
        self.read_u8().into()
    }

    #[inline(always)]
    pub fn read_u8(&mut self) -> u8 {
        let value = self.slice().read_u8().unwrap();
        self.pointer += mem::size_of::<u8>();
        value
    }

    #[inline(always)]
    pub fn read_u16(&mut self) -> u16 {
        let value = self.slice().read_u16::<NativeEndian>().unwrap();
        self.pointer += mem::size_of::<u16>();
        value
    }

    #[inline(always)]
    pub fn read_i16(&mut self) -> i16 {
        let value = self.slice().read_i16::<NativeEndian>().unwrap();
        self.pointer += mem::size_of::<i16>();
        value
    }

    #[inline(always)]
    pub fn read_u32(&mut self) -> u32 {
        let value = self.slice().read_u32::<NativeEndian>().unwrap();
        self.pointer += mem::size_of::<u32>();
        value
    }

    #[inline(always)]
    pub fn read_i32(&mut self) -> i32 {
        let value = self.slice().read_i32::<NativeEndian>().unwrap();
        self.pointer += mem::size_of::<i32>();
        value
    }

    #[inline(always)]
    fn slice(&self) -> &[u8] {
        &self.bytecode.buf[self.pointer..]
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Op {
    Noop,
    Pop,
    LoadConst,
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
    Jmp,
    Jmpf,
    Return,
    Call,
    GetGlobal,
    GetGlobalPtr,
    SetGlobal,
    Peek,
    PeekPtr,
    SetLocal,
    Index,
    IndexPtr,
    Offset,
    ConstIndex,
    ConstIndexPtr,
    Assign,
    Cast,
    BufferAlloc,
    BufferPut,
    BufferFill,
    Copy,
    Swap,
    Halt,
}

impl From<u8> for Op {
    fn from(x: u8) -> Self {
        use Op::*;

        match x {
            0 => Noop,
            1 => Pop,
            2 => LoadConst,
            3 => Add,
            4 => Sub,
            5 => Mul,
            6 => Div,
            7 => Rem,
            8 => Neg,
            9 => Not,
            10 => Deref,
            12 => Eq,
            13 => Ne,
            14 => Lt,
            15 => Le,
            16 => Gt,
            17 => Ge,
            18 => And,
            19 => Or,
            20 => Shl,
            21 => Shr,
            22 => Xor,
            23 => Jmp,
            24 => Jmpf,
            25 => Return,
            26 => Call,
            27 => GetGlobal,
            28 => GetGlobalPtr,
            29 => SetGlobal,
            30 => Peek,
            31 => PeekPtr,
            32 => SetLocal,
            33 => Index,
            34 => IndexPtr,
            35 => Offset,
            36 => ConstIndex,
            37 => ConstIndexPtr,
            38 => Assign,
            39 => Cast,
            40 => BufferAlloc,
            41 => BufferPut,
            42 => BufferFill,
            43 => Copy,
            44 => Swap,
            45 => Halt,
            _ => panic!(),
        }
    }
}

impl From<Op> for u8 {
    fn from(x: Op) -> Self {
        use Op::*;

        match x {
            Noop => 0,
            Pop => 1,
            LoadConst => 2,
            Add => 3,
            Sub => 4,
            Mul => 5,
            Div => 6,
            Rem => 7,
            Neg => 8,
            Not => 9,
            Deref => 10,
            Eq => 12,
            Ne => 13,
            Lt => 14,
            Le => 15,
            Gt => 16,
            Ge => 17,
            And => 18,
            Or => 19,
            Shl => 20,
            Shr => 21,
            Xor => 22,
            Jmp => 23,
            Jmpf => 24,
            Return => 25,
            Call => 26,
            GetGlobal => 27,
            GetGlobalPtr => 28,
            SetGlobal => 29,
            Peek => 30,
            PeekPtr => 31,
            SetLocal => 32,
            Index => 33,
            IndexPtr => 34,
            Offset => 35,
            ConstIndex => 36,
            ConstIndexPtr => 37,
            Assign => 38,
            Cast => 39,
            BufferAlloc => 40,
            BufferPut => 41,
            BufferFill => 42,
            Copy => 43,
            Swap => 44,
            Halt => 45,
        }
    }
}

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
