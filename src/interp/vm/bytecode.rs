use byteorder::{NativeEndian, ReadBytesExt, WriteBytesExt};

pub struct Bytecode {
    buf: Vec<u8>,
}

impl Bytecode {
    #[inline(always)]
    pub fn write_op(&mut self, op: Op) {
        self.write_u8(op.into())
    }

    #[inline(always)]
    pub fn write_u8(&mut self, x: u8) {
        self.buf.write_u8(x).unwrap();
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
    pub fn read_op(&mut self) -> Op {
        self.read_u8().into()
    }

    #[inline(always)]
    pub fn read_u8(&mut self) -> u8 {
        self.slice().read_u8().unwrap()
    }

    #[inline(always)]
    pub fn read_u32(&mut self) -> u32 {
        self.slice().read_u32::<NativeEndian>().unwrap()
    }

    #[inline(always)]
    pub fn read_i32(&mut self) -> i32 {
        self.slice().read_i32::<NativeEndian>().unwrap()
    }

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
