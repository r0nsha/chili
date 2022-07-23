use crate::ast;
use byteorder::{NativeEndian, ReadBytesExt, WriteBytesExt};
use std::fmt::Display;
use std::io::Write;
use std::mem;
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone)]
pub struct Bytecode {
    buf: Vec<u8>,
    pub locals: u32,
}

impl Index<usize> for Bytecode {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buf[index]
    }
}

impl IndexMut<usize> for Bytecode {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.buf[index]
    }
}

impl Bytecode {
    pub fn new() -> Self {
        Self {
            buf: vec![],
            locals: 0,
        }
    }

    #[inline(always)]
    pub fn write_inst(&mut self, inst: Inst) -> usize {
        match inst {
            Inst::Noop => self.write_op(Op::Noop),
            Inst::Pop => self.write_op(Op::Pop),
            Inst::LoadConst(slot) => {
                let addr = self.write_op(Op::LoadConst);
                self.write_u32(slot);
                addr
            }
            Inst::Add => self.write_op(Op::Add),
            Inst::Sub => self.write_op(Op::Sub),
            Inst::Mul => self.write_op(Op::Mul),
            Inst::Div => self.write_op(Op::Div),
            Inst::Rem => self.write_op(Op::Rem),
            Inst::Neg => self.write_op(Op::Neg),
            Inst::Not => self.write_op(Op::Not),
            Inst::Deref => self.write_op(Op::Deref),
            Inst::Eq => self.write_op(Op::Eq),
            Inst::Ne => self.write_op(Op::Ne),
            Inst::Lt => self.write_op(Op::Lt),
            Inst::Le => self.write_op(Op::Le),
            Inst::Gt => self.write_op(Op::Gt),
            Inst::Ge => self.write_op(Op::Ge),
            Inst::And => self.write_op(Op::And),
            Inst::Or => self.write_op(Op::Or),
            Inst::Shl => self.write_op(Op::Shl),
            Inst::Shr => self.write_op(Op::Shr),
            Inst::Xor => self.write_op(Op::Xor),
            Inst::Jmp(addr) => {
                let op_addr = self.write_op(Op::Jmp);
                self.write_i32(addr);
                op_addr
            }
            Inst::Jmpf(addr) => {
                let op_addr = self.write_op(Op::Jmpf);
                self.write_i32(addr);
                op_addr
            }
            Inst::Return => self.write_op(Op::Return),
            Inst::Call(args) => {
                let addr = self.write_op(Op::Call);
                self.write_u32(args);
                addr
            }
            Inst::LoadGlobal(slot) => {
                let addr = self.write_op(Op::LoadGlobal);
                self.write_u32(slot);
                addr
            }
            Inst::LoadGlobalPtr(slot) => {
                let addr = self.write_op(Op::LoadGlobalPtr);
                self.write_u32(slot);
                addr
            }
            Inst::StoreGlobal(slot) => {
                let addr = self.write_op(Op::StoreGlobal);
                self.write_u32(slot);
                addr
            }
            Inst::Peek(offset) => {
                let addr = self.write_op(Op::Peek);
                self.write_i32(offset);
                addr
            }
            Inst::PeekPtr(offset) => {
                let addr = self.write_op(Op::PeekPtr);
                self.write_i32(offset);
                addr
            }
            Inst::StoreLocal(slot) => {
                let addr = self.write_op(Op::StoreLocal);
                self.write_i32(slot);
                addr
            }
            Inst::Index => self.write_op(Op::Index),
            Inst::IndexPtr => self.write_op(Op::IndexPtr),
            Inst::Offset => self.write_op(Op::Offset),
            Inst::ConstIndex(index) => {
                let addr = self.write_op(Op::ConstIndex);
                self.write_u32(index);
                addr
            }
            Inst::ConstIndexPtr(index) => {
                let addr = self.write_op(Op::ConstIndexPtr);
                self.write_u32(index);
                addr
            }
            Inst::Assign => self.write_op(Op::Assign),
            Inst::Cast => self.write_op(Op::Cast),
            Inst::BufferAlloc(size) => {
                let addr = self.write_op(Op::BufferAlloc);
                self.write_u32(size);
                addr
            }
            Inst::BufferPut(offset) => {
                let addr = self.write_op(Op::BufferPut);
                self.write_u32(offset);
                addr
            }
            Inst::BufferFill(size) => {
                let addr = self.write_op(Op::BufferFill);
                self.write_u32(size);
                addr
            }
            Inst::Copy(offset) => {
                let addr = self.write_op(Op::Copy);
                self.write_u32(offset);
                addr
            }
            Inst::Swap(offset) => {
                let addr = self.write_op(Op::Swap);
                self.write_u32(offset);
                addr
            }
            Inst::Halt => self.write_op(Op::Halt),
        }
    }

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

    #[inline(always)]
    pub fn write_all(&mut self, x: &[u8]) {
        self.buf.write_all(x);
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[u8] {
        &self.buf[..]
    }

    #[inline(always)]
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.buf[..]
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn reader(&self) -> BytecodeReader {
        BytecodeReader {
            bytecode: self,
            cursor: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BytecodeReader<'a> {
    bytecode: &'a Bytecode,
    cursor: usize,
}

impl<'a> BytecodeReader<'a> {
    #[inline(always)]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    #[inline(always)]
    pub fn set_cursor(&mut self, to: usize) {
        self.cursor = to;
    }

    #[inline(always)]
    pub fn has_remaining(&self) -> bool {
        self.cursor < self.bytecode.buf.len()
    }

    #[inline(always)]
    pub fn try_read_op(&mut self) -> Option<Op> {
        if self.has_remaining() {
            Some(Op::from(self.read_u8()))
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn read_op(&mut self) -> Op {
        Op::from(self.read_u8())
    }

    #[inline(always)]
    pub fn read_u8(&mut self) -> u8 {
        let value = self.as_slice().read_u8().unwrap();
        self.cursor += mem::size_of::<u8>();
        value
    }

    #[inline(always)]
    pub fn read_u16(&mut self) -> u16 {
        let value = self.as_slice().read_u16::<NativeEndian>().unwrap();
        self.cursor += mem::size_of::<u16>();
        value
    }

    #[inline(always)]
    pub fn read_i16(&mut self) -> i16 {
        let value = self.as_slice().read_i16::<NativeEndian>().unwrap();
        self.cursor += mem::size_of::<i16>();
        value
    }

    #[inline(always)]
    pub fn read_u32(&mut self) -> u32 {
        let value = self.as_slice().read_u32::<NativeEndian>().unwrap();
        self.cursor += mem::size_of::<u32>();
        value
    }

    #[inline(always)]
    pub fn read_i32(&mut self) -> i32 {
        let value = self.as_slice().read_i32::<NativeEndian>().unwrap();
        self.cursor += mem::size_of::<i32>();
        value
    }

    #[inline(always)]
    fn as_slice(&self) -> &[u8] {
        &self.bytecode.buf[self.cursor..]
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
    LoadGlobal,
    LoadGlobalPtr,
    StoreGlobal,
    Peek,
    PeekPtr,
    StoreLocal,
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
            27 => LoadGlobal,
            28 => LoadGlobalPtr,
            29 => StoreGlobal,
            30 => Peek,
            31 => PeekPtr,
            32 => StoreLocal,
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
            LoadGlobal => 27,
            LoadGlobalPtr => 28,
            StoreGlobal => 29,
            Peek => 30,
            PeekPtr => 31,
            StoreLocal => 32,
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

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Noop => write!(f, "noop"),
            Op::Pop => write!(f, "pop"),
            Op::LoadConst => write!(f, "load_const"),
            Op::Add => write!(f, "add"),
            Op::Sub => write!(f, "sub"),
            Op::Mul => write!(f, "mul"),
            Op::Div => write!(f, "div"),
            Op::Rem => write!(f, "rem"),
            Op::Neg => write!(f, "neg"),
            Op::Not => write!(f, "not"),
            Op::Deref => write!(f, "deref"),
            Op::Eq => write!(f, "eq"),
            Op::Ne => write!(f, "ne"),
            Op::Lt => write!(f, "lt"),
            Op::Le => write!(f, "le"),
            Op::Gt => write!(f, "gt"),
            Op::Ge => write!(f, "ge"),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Shl => write!(f, "shl"),
            Op::Shr => write!(f, "shr"),
            Op::Xor => write!(f, "xor"),
            Op::Jmp => write!(f, "jmp"),
            Op::Jmpf => write!(f, "jmpf"),
            Op::Return => write!(f, "return"),
            Op::Call => write!(f, "call"),
            Op::LoadGlobal => write!(f, "load_global"),
            Op::LoadGlobalPtr => write!(f, "load_global_ptr"),
            Op::StoreGlobal => write!(f, "store_global"),
            Op::Peek => write!(f, "peek"),
            Op::PeekPtr => write!(f, "peek_ptr"),
            Op::StoreLocal => write!(f, "store_local"),
            Op::Index => write!(f, "index"),
            Op::IndexPtr => write!(f, "index_ptr"),
            Op::Offset => write!(f, "offset"),
            Op::ConstIndex => write!(f, "const_index"),
            Op::ConstIndexPtr => write!(f, "const_index_ptr"),
            Op::Assign => write!(f, "assign"),
            Op::Cast => write!(f, "cast"),
            Op::BufferAlloc => write!(f, "buffer_alloc"),
            Op::BufferPut => write!(f, "buffer_put"),
            Op::BufferFill => write!(f, "buffer_fill"),
            Op::Copy => write!(f, "copy"),
            Op::Swap => write!(f, "swap"),
            Op::Halt => write!(f, "halt"),
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
    LoadGlobal(u32),
    LoadGlobalPtr(u32),
    StoreGlobal(u32),
    Peek(i32),
    PeekPtr(i32),
    StoreLocal(i32),
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
                Inst::LoadGlobal(slot) => format!("load_global ${}", slot),
                Inst::LoadGlobalPtr(slot) => format!("load_global_ptr ${}", slot),
                Inst::StoreGlobal(slot) => format!("store_global ${}", slot),
                Inst::Peek(slot) => format!("peek ${}", slot),
                Inst::PeekPtr(slot) => format!("peek_ptr ${}", slot),
                Inst::StoreLocal(slot) => format!("store_local ${}", slot),
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
