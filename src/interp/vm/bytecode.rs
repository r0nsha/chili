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
        Self { buf: vec![], locals: 0 }
    }

    #[inline(always)]
    pub fn write_inst(&mut self, inst: Inst) -> usize {
        match inst {
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

    #[allow(unused)]
    #[inline(always)]
    pub fn write_u16(&mut self, x: u16) {
        self.buf.write_u16::<NativeEndian>(x).unwrap();
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn write_i16(&mut self, x: i16) {
        self.buf.write_i16::<NativeEndian>(x).unwrap();
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn write_u32(&mut self, x: u32) {
        self.buf.write_u32::<NativeEndian>(x).unwrap();
    }

    #[inline(always)]
    pub fn write_i32(&mut self, x: i32) {
        self.buf.write_i32::<NativeEndian>(x).unwrap();
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn write_all(&mut self, x: &[u8]) {
        self.buf.write_all(x).unwrap();
    }

    #[allow(unused)]
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

    #[allow(unused)]
    #[inline(always)]
    pub fn read_u16(&mut self) -> u16 {
        let value = self.as_slice().read_u16::<NativeEndian>().unwrap();
        self.cursor += mem::size_of::<u16>();
        value
    }

    #[allow(unused)]
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
            0 => Pop,
            1 => LoadConst,
            2 => Add,
            3 => Sub,
            4 => Mul,
            5 => Div,
            6 => Rem,
            7 => Neg,
            8 => Not,
            9 => Deref,
            11 => Eq,
            12 => Ne,
            13 => Lt,
            14 => Le,
            15 => Gt,
            16 => Ge,
            17 => And,
            18 => Or,
            19 => Shl,
            20 => Shr,
            21 => Xor,
            22 => Jmp,
            23 => Jmpf,
            24 => Return,
            25 => Call,
            26 => LoadGlobal,
            27 => LoadGlobalPtr,
            28 => StoreGlobal,
            29 => Peek,
            30 => PeekPtr,
            31 => StoreLocal,
            32 => Offset,
            33 => ConstIndex,
            34 => ConstIndexPtr,
            35 => Assign,
            36 => Cast,
            37 => BufferAlloc,
            38 => BufferPut,
            39 => BufferFill,
            40 => Copy,
            41 => Swap,
            42 => Halt,
            _ => panic!(),
        }
    }
}

impl From<Op> for u8 {
    fn from(x: Op) -> Self {
        use Op::*;

        match x {
            Pop => 0,
            LoadConst => 1,
            Add => 2,
            Sub => 3,
            Mul => 4,
            Div => 5,
            Rem => 6,
            Neg => 7,
            Not => 8,
            Deref => 9,
            Eq => 11,
            Ne => 12,
            Lt => 13,
            Le => 14,
            Gt => 15,
            Ge => 16,
            And => 17,
            Or => 18,
            Shl => 19,
            Shr => 20,
            Xor => 21,
            Jmp => 22,
            Jmpf => 23,
            Return => 24,
            Call => 25,
            LoadGlobal => 26,
            LoadGlobalPtr => 27,
            StoreGlobal => 28,
            Peek => 29,
            PeekPtr => 30,
            StoreLocal => 31,
            Offset => 32,
            ConstIndex => 33,
            ConstIndexPtr => 34,
            Assign => 35,
            Cast => 36,
            BufferAlloc => 37,
            BufferPut => 38,
            BufferFill => 39,
            Copy => 40,
            Swap => 41,
            Halt => 42,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Inst {
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
