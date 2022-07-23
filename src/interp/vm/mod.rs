use self::{
    bytecode::{BytecodeReader, Op},
    value::FunctionValue,
};
use super::{
    ffi::RawPointer,
    interp::Interp,
    vm::{
        byte_seq::{ByteSeq, PutValue},
        disassemble::bytecode_reader_write_single_inst,
        stack::Stack,
        value::{Buffer, Function, Value},
    },
};
use colored::Colorize;
use std::{fmt::Display, ptr};
use ustr::ustr;

pub mod byte_seq;
pub mod bytecode;
mod cast;
pub mod disassemble;
mod index;
mod intrinsics;
mod stack;
pub mod value;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (std::u8::MAX as usize) + 1;

pub type Constants = Vec<Value>;
pub type Globals = Vec<Value>;

#[derive(Debug, Clone)]
pub struct StackFrame<'a> {
    func: *const Function,
    reader: BytecodeReader<'a>,
    stack_slot: usize,
}

impl<'a> Display for StackFrame<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:06}\t{}>", self.reader.cursor(), self.func().name)
    }
}

impl<'a> StackFrame<'a> {
    pub fn new(func: &Function, slot: usize) -> Self {
        Self {
            func: func as _,
            reader: unsafe { *func }.code.reader(),
            stack_slot: slot,
        }
    }

    #[inline]
    pub fn func(&self) -> &Function {
        debug_assert!(!self.func.is_null());
        unsafe { &*self.func }
    }
}

macro_rules! binary_op_int_only {
    ($vm:expr, $op:tt) => {{
        let b = $vm.stack.pop();
        let a = $vm.stack.pop();

        match (&a, &b) {
            (Value::I8(a), Value::I8(b)) => $vm.stack.push(Value::I8(a $op b)),
            (Value::I16(a), Value::I16(b)) => $vm.stack.push(Value::I16(a $op b)),
            (Value::I32(a), Value::I32(b)) => $vm.stack.push(Value::I32(a $op b)),
            (Value::I64(a), Value::I64(b)) => $vm.stack.push(Value::I64(a $op b)),
            (Value::Int(a), Value::Int(b)) => $vm.stack.push(Value::Int(a $op b)),
            (Value::U8(a), Value::U8(b)) => $vm.stack.push(Value::U8(a $op b)),
            (Value::U16(a), Value::U16(b)) => $vm.stack.push(Value::U16(a $op b)),
            (Value::U32(a), Value::U32(b)) => $vm.stack.push(Value::U32(a $op b)),
            (Value::U64(a), Value::U64(b)) => $vm.stack.push(Value::U64(a $op b)),
            (Value::Uint(a), Value::Uint(b)) => $vm.stack.push(Value::Uint(a $op b)),
            _=> panic!("invalid types in binary operation `{}` : `{}` and `{}`", stringify!($op), a.to_string() ,b.to_string())
        }

        $vm.next();
    }};
}

macro_rules! compare_op {
    ($vm:expr, $op:tt) => {
        let b = $vm.stack.pop();
        let a = $vm.stack.pop();

        match (&a, &b) {
            (Value::Bool(a), Value::Bool(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::I8(a), Value::I8(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::I16(a), Value::I16(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::I32(a), Value::I32(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::I64(a), Value::I64(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::Int(a), Value::Int(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::U8(a), Value::U8(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::U16(a), Value::U16(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::U32(a), Value::U32(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::U64(a), Value::U64(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::Uint(a), Value::Uint(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::F32(a), Value::F32(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::F64(a), Value::F64(b)) => $vm.stack.push(Value::Bool(a $op b)),
            (Value::Pointer(a), Value::Pointer(b)) => $vm.stack.push(Value::Bool(a.as_inner_raw() $op b.as_inner_raw())),
            _ => panic!("invalid types in compare operation `{}` and `{}`", a.to_string() ,b.to_string())
        }

        $vm.next();
    };
}

macro_rules! logic_op {
    ($vm:expr, $op:tt) => {
        let b = $vm.stack.pop();
        let a = $vm.stack.pop();

        $vm.stack.push(Value::Bool(a.into_bool() $op b.into_bool()));

        $vm.next();
    };
}

pub struct VM<'vm, 'f> {
    pub interp: &'vm mut Interp,
    pub stack: Stack<Value, STACK_MAX>,
    pub frames: Stack<StackFrame<'f>, FRAMES_MAX>,
    pub frame: *mut StackFrame<'f>,
}

impl<'vm, 'f> VM<'vm, 'f> {
    pub fn new(interp: &'vm mut Interp) -> Self {
        Self {
            interp,
            stack: Stack::new(),
            frames: Stack::new(),
            frame: ptr::null_mut(),
        }
    }

    pub fn run_func(&'vm mut self, function: Function) -> Value {
        self.push_frame(&function);
        self.run_inner()
    }

    fn run_inner(&'vm mut self) -> Value {
        loop {
            let frame = self.frame();
            let reader = &mut frame.reader;

            // self.trace(TraceLevel::Full);

            let op = frame.reader.read_op();

            match op {
                Op::Noop => {
                    self.next();
                }
                Op::Pop => {
                    self.stack.pop();
                    self.next();
                }
                Op::LoadConst => {
                    let addr = reader.read_u32();
                    let const_ = self.interp.constants.get(addr as usize).unwrap();

                    let value = match const_ {
                        Value::ExternVariable(variable) => {
                            let symbol = unsafe {
                                self.interp
                                    .ffi
                                    .load_symbol(ustr(&variable.lib.path()), variable.name)
                            };

                            unsafe { Value::from_type_and_ptr(&variable.ty, *symbol as RawPointer) }
                        }
                        value => value.clone(),
                    };

                    self.stack.push(value);
                    self.next();
                }
                Op::Add => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    match (&a, &b) {
                        (Value::I8(a), Value::I8(b)) => self.stack.push(Value::I8(a + b)),
                        (Value::I16(a), Value::I16(b)) => self.stack.push(Value::I16(a + b)),
                        (Value::I32(a), Value::I32(b)) => self.stack.push(Value::I32(a + b)),
                        (Value::I64(a), Value::I64(b)) => self.stack.push(Value::I64(a + b)),
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a + b)),
                        (Value::U8(a), Value::U8(b)) => self.stack.push(Value::U8(a + b)),
                        (Value::U16(a), Value::U16(b)) => self.stack.push(Value::U16(a + b)),
                        (Value::U32(a), Value::U32(b)) => self.stack.push(Value::U32(a + b)),
                        (Value::U64(a), Value::U64(b)) => self.stack.push(Value::U64(a + b)),
                        (Value::Uint(a), Value::Uint(b)) => self.stack.push(Value::Uint(a + b)),
                        (Value::F32(a), Value::F32(b)) => self.stack.push(Value::F32(a + b)),
                        (Value::F64(a), Value::F64(b)) => self.stack.push(Value::F64(a + b)),
                        (Value::Pointer(a), Value::Int(b)) => {
                            self.stack.push(Value::Pointer(unsafe { a.offset(*b) }))
                        }
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(+),
                            a.to_string(),
                            b.to_string()
                        ),
                    }

                    self.next();
                }
                Op::Sub => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    match (&a, &b) {
                        (Value::I8(a), Value::I8(b)) => self.stack.push(Value::I8(a - b)),
                        (Value::I16(a), Value::I16(b)) => self.stack.push(Value::I16(a - b)),
                        (Value::I32(a), Value::I32(b)) => self.stack.push(Value::I32(a - b)),
                        (Value::I64(a), Value::I64(b)) => self.stack.push(Value::I64(a - b)),
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a - b)),
                        (Value::U8(a), Value::U8(b)) => self.stack.push(Value::U8(a - b)),
                        (Value::U16(a), Value::U16(b)) => self.stack.push(Value::U16(a - b)),
                        (Value::U32(a), Value::U32(b)) => self.stack.push(Value::U32(a - b)),
                        (Value::U64(a), Value::U64(b)) => self.stack.push(Value::U64(a - b)),
                        (Value::Uint(a), Value::Uint(b)) => self.stack.push(Value::Uint(a - b)),
                        (Value::F32(a), Value::F32(b)) => self.stack.push(Value::F32(a - b)),
                        (Value::F64(a), Value::F64(b)) => self.stack.push(Value::F64(a - b)),
                        (Value::Pointer(a), Value::Int(b)) => {
                            self.stack.push(Value::Pointer(unsafe { a.offset(-*b) }))
                        }
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(-),
                            a.to_string(),
                            b.to_string()
                        ),
                    }

                    self.next();
                }
                Op::Mul => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    match (&a, &b) {
                        (Value::I8(a), Value::I8(b)) => self.stack.push(Value::I8(a * b)),
                        (Value::I16(a), Value::I16(b)) => self.stack.push(Value::I16(a * b)),
                        (Value::I32(a), Value::I32(b)) => self.stack.push(Value::I32(a * b)),
                        (Value::I64(a), Value::I64(b)) => self.stack.push(Value::I64(a * b)),
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a * b)),
                        (Value::U8(a), Value::U8(b)) => self.stack.push(Value::U8(a * b)),
                        (Value::U16(a), Value::U16(b)) => self.stack.push(Value::U16(a * b)),
                        (Value::U32(a), Value::U32(b)) => self.stack.push(Value::U32(a * b)),
                        (Value::U64(a), Value::U64(b)) => self.stack.push(Value::U64(a * b)),
                        (Value::Uint(a), Value::Uint(b)) => self.stack.push(Value::Uint(a * b)),
                        (Value::F32(a), Value::F32(b)) => self.stack.push(Value::F32(a * b)),
                        (Value::F64(a), Value::F64(b)) => self.stack.push(Value::F64(a * b)),
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(*),
                            a.to_string(),
                            b.to_string()
                        ),
                    }

                    self.next();
                }
                Op::Div => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    match (&a, &b) {
                        (Value::I8(a), Value::I8(b)) => self.stack.push(Value::I8(a % b)),
                        (Value::I16(a), Value::I16(b)) => self.stack.push(Value::I16(a % b)),
                        (Value::I32(a), Value::I32(b)) => self.stack.push(Value::I32(a % b)),
                        (Value::I64(a), Value::I64(b)) => self.stack.push(Value::I64(a % b)),
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a % b)),
                        (Value::U8(a), Value::U8(b)) => self.stack.push(Value::U8(a % b)),
                        (Value::U16(a), Value::U16(b)) => self.stack.push(Value::U16(a % b)),
                        (Value::U32(a), Value::U32(b)) => self.stack.push(Value::U32(a % b)),
                        (Value::U64(a), Value::U64(b)) => self.stack.push(Value::U64(a % b)),
                        (Value::Uint(a), Value::Uint(b)) => self.stack.push(Value::Uint(a % b)),
                        (Value::F32(a), Value::F32(b)) => self.stack.push(Value::F32(a % b)),
                        (Value::F64(a), Value::F64(b)) => self.stack.push(Value::F64(a % b)),
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(%),
                            a.to_string(),
                            b.to_string()
                        ),
                    }

                    self.next();
                }
                Op::Rem => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    match (&a, &b) {
                        (Value::I8(a), Value::I8(b)) => self.stack.push(Value::I8(a % b)),
                        (Value::I16(a), Value::I16(b)) => self.stack.push(Value::I16(a % b)),
                        (Value::I32(a), Value::I32(b)) => self.stack.push(Value::I32(a % b)),
                        (Value::I64(a), Value::I64(b)) => self.stack.push(Value::I64(a % b)),
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a % b)),
                        (Value::U8(a), Value::U8(b)) => self.stack.push(Value::U8(a % b)),
                        (Value::U16(a), Value::U16(b)) => self.stack.push(Value::U16(a % b)),
                        (Value::U32(a), Value::U32(b)) => self.stack.push(Value::U32(a % b)),
                        (Value::U64(a), Value::U64(b)) => self.stack.push(Value::U64(a % b)),
                        (Value::Uint(a), Value::Uint(b)) => self.stack.push(Value::Uint(a % b)),
                        (Value::F32(a), Value::F32(b)) => self.stack.push(Value::F32(a % b)),
                        (Value::F64(a), Value::F64(b)) => self.stack.push(Value::F64(a % b)),
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(%),
                            a.to_string(),
                            b.to_string()
                        ),
                    }

                    self.next();
                }
                Op::Neg => {
                    match self.stack.pop() {
                        Value::Int(v) => self.stack.push(Value::Int(-v)),
                        value => panic!("invalid value {}", value.to_string()),
                    }
                    self.next();
                }
                Op::Not => {
                    let result = match self.stack.pop() {
                        Value::I8(v) => Value::I8(!v),
                        Value::I16(v) => Value::I16(!v),
                        Value::I32(v) => Value::I32(!v),
                        Value::I64(v) => Value::I64(!v),
                        Value::Int(v) => Value::Int(!v),
                        Value::U8(v) => Value::U8(!v),
                        Value::U16(v) => Value::U16(!v),
                        Value::U32(v) => Value::U32(!v),
                        Value::U64(v) => Value::U64(!v),
                        Value::Uint(v) => Value::Uint(!v),
                        Value::Bool(v) => Value::Bool(!v),
                        v => panic!("invalid value {}", v.to_string()),
                    };
                    self.stack.push(result);
                    self.next();
                }
                Op::Deref => {
                    match self.stack.pop() {
                        Value::Pointer(ptr) => {
                            let value = unsafe { ptr.deref_value() };
                            self.stack.push(value);
                        }
                        value => panic!("invalid value {}", value.to_string()),
                    }
                    self.next();
                }
                Op::Eq => {
                    compare_op!(self, ==);
                }
                Op::Ne => {
                    compare_op!(self, !=);
                }
                Op::Lt => {
                    compare_op!(self, <);
                }
                Op::Le => {
                    compare_op!(self, <=);
                }
                Op::Gt => {
                    compare_op!(self, >);
                }
                Op::Ge => {
                    compare_op!(self, >=);
                }
                Op::And => {
                    logic_op!(self, &&);
                }
                Op::Or => {
                    logic_op!(self, ||);
                }
                Op::Shl => {
                    binary_op_int_only!(self, <<)
                }
                Op::Shr => {
                    binary_op_int_only!(self, >>);
                }
                Op::Xor => {
                    binary_op_int_only!(self, ^);
                }
                Op::Jmp => {
                    let offset = reader.read_i32();
                    self.jmp(offset);
                }
                Op::Jmpf => {
                    let offset = reader.read_i32();

                    if !self.stack.pop().into_bool() {
                        self.jmp(offset);
                    } else {
                        self.next();
                    }
                }
                Op::Return => {
                    let frame = self.frames.pop();
                    let return_value = self.stack.pop();

                    if self.frames.is_empty() {
                        break return_value;
                    } else {
                        self.stack
                            .truncate(frame.stack_slot - frame.func().ty.params.len());
                        self.frame = self.frames.last_mut() as _;
                        self.stack.push(return_value);
                        self.next();
                    }
                }
                Op::Call => {
                    let arg_count = reader.read_u32();

                    match self.stack.pop() {
                        Value::Function(addr) => {
                            match self.interp.get_function(addr.id).unwrap_or_else(|| {
                                panic!("couldn't find '{}' {:?}", addr.name, addr.id)
                            }) {
                                FunctionValue::Orphan(function) => {
                                    self.push_frame(function);
                                }
                                FunctionValue::Extern(function) => {
                                    let mut values = (0..arg_count)
                                        .into_iter()
                                        .map(|_| self.stack.pop())
                                        .collect::<Vec<Value>>();

                                    values.reverse();

                                    let function = function.clone();
                                    let vm_ptr = self as *mut _;
                                    let interp_ptr = self.interp as *const _;

                                    let result = unsafe {
                                        self.interp.ffi.call(function, values, vm_ptr, interp_ptr)
                                    };

                                    self.stack.push(result);

                                    self.next();
                                }
                            }
                        }
                        Value::Intrinsic(intrinsic) => self.dispatch_intrinsic(intrinsic),
                        value => panic!("tried to call uncallable value `{}`", value.to_string()),
                    }
                }
                Op::LoadGlobal => {
                    let slot = reader.read_u32();

                    match self.interp.globals.get(slot as usize) {
                        Some(value) => self.stack.push(value.clone()),
                        None => panic!("undefined global `{}`", slot),
                    }

                    self.next();
                }
                Op::LoadGlobalPtr => {
                    let slot = reader.read_u32();

                    match self.interp.globals.get_mut(slot as usize) {
                        Some(value) => self.stack.push(Value::Pointer(value.into())),
                        None => panic!("undefined global `{}`", slot),
                    }

                    self.next();
                }
                Op::StoreGlobal => {
                    let slot = reader.read_u32();
                    self.interp.globals[slot as usize] = self.stack.pop();
                    self.next();
                }
                Op::Peek => {
                    let offset = reader.read_i32();
                    let slot = self.frame().stack_slot as isize + offset as isize;

                    let value = self.stack.get(slot as usize).clone();
                    self.stack.push(value);

                    self.next();
                }
                Op::PeekPtr => {
                    let offset = reader.read_i32();
                    let slot = self.frame().stack_slot as isize + offset as isize;

                    let value = self.stack.get_mut(slot as usize);
                    let value = Value::Pointer(value.into());
                    self.stack.push(value);

                    self.next();
                }
                Op::StoreLocal => {
                    let offset = reader.read_i32();
                    let slot = self.frame().stack_slot as isize + offset as isize;

                    let value = self.stack.pop();
                    self.stack.set(slot as usize, value);

                    self.next();
                }
                Op::Index => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.index(value, index);
                    self.next();
                }
                Op::IndexPtr => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.index_ptr(value, index);
                    self.next();
                }
                Op::Offset => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.offset(value, index);
                    self.next();
                }
                Op::ConstIndex => {
                    let index = reader.read_u32();

                    let value = self.stack.pop();
                    self.index(value, index as usize);

                    self.next();
                }
                Op::ConstIndexPtr => {
                    let index = reader.read_u32();

                    let value = self.stack.pop();
                    self.index_ptr(value, index as usize);

                    self.next();
                }
                Op::Assign => {
                    let lhs = self.stack.pop().into_pointer();
                    let rhs = self.stack.pop();
                    unsafe { lhs.write_value(rhs) }
                    self.next();
                }
                Op::Cast => {
                    self.cast_inst();
                    self.next();
                }
                Op::BufferAlloc => {
                    let size = reader.read_u32();

                    let ty = self.stack.pop().into_type();
                    self.stack.push(Value::Buffer(Buffer {
                        bytes: ByteSeq::new(size as usize),
                        ty,
                    }));

                    self.next();
                }
                Op::BufferPut => {
                    let offset = reader.read_u32();

                    let value = self.stack.pop();

                    let buf = self.stack.peek_mut(0).as_buffer_mut();
                    buf.bytes.offset_mut(offset as usize).put_value(&value);

                    self.next();
                }
                Op::BufferFill => {
                    let size = reader.read_u32();

                    let value = self.stack.pop();
                    let buf = self.stack.peek_mut(0).as_buffer_mut();

                    for _ in 0..size {
                        buf.bytes.put_value(&value);
                    }

                    self.next();
                }
                Op::Copy => {
                    let offset = reader.read_u32();
                    let value = self.stack.peek(offset as usize).clone();
                    self.stack.push(value);
                    self.next();
                }
                Op::Swap => {
                    let offset = reader.read_u32();
                    let last_index = self.stack.len() - 1;
                    self.stack.swap(last_index, last_index - offset as usize);
                    self.next();
                }
                Op::Halt => {
                    let result = self.stack.pop();
                    break result;
                }
            }
        }
    }

    #[inline]
    pub fn push_frame(&mut self, func: &Function) {
        let stack_slot = self.stack.len();

        for _ in 0..func.code.locals {
            self.stack.push(Value::default());
        }

        self.frames.push(StackFrame::new(func, stack_slot));

        self.frame = self.frames.last_mut() as _;
    }

    #[inline]
    pub fn frame(&self) -> &StackFrame {
        debug_assert!(!self.frame.is_null());
        unsafe { &*self.frame }
    }

    #[inline]
    pub fn frame_mut(&mut self) -> &mut StackFrame {
        debug_assert!(!self.frame.is_null());
        unsafe { &mut *self.frame }
    }

    #[inline]
    pub fn next(&mut self) {
        self.frame_mut().ip += 1;
    }

    #[inline]
    pub fn jmp(&mut self, offset: i32) {
        let new_inst_pointer = self.frame().ip as isize + offset as isize;
        self.frame_mut().ip = new_inst_pointer as usize;
    }

    #[allow(unused)]
    pub fn trace(&self, level: TraceLevel) {
        let frame = self.frame();

        bytecode_reader_write_single_inst(&mut frame.reader.clone(), &mut std::io::stdout());

        match level {
            TraceLevel::Minimal => {
                println!(" [stack items: {}]", self.stack.len());
            }
            TraceLevel::Full => {
                self.trace_stack(frame);
            }
        }
    }

    #[allow(unused)]
    fn trace_stack(&self, frame: &StackFrame) {
        print!("\t[");
        let frame_slot = frame.stack_slot;
        for (index, value) in self.stack.iter().enumerate() {
            print!(
                "{}",
                if index == frame_slot {
                    // frame slot
                    value.to_string().bright_yellow()
                } else if index > frame_slot - frame.func().ty.params.len()
                    && index <= frame_slot + frame.func().code.locals as usize
                {
                    // local value
                    value.to_string().bright_magenta()
                } else {
                    // any other value
                    value.to_string().white()
                }
            );

            if index < self.stack.len() - 1 {
                print!(", ");
            }
        }
        println!("] ({})\n", self.stack.len());
    }
}

#[allow(dead_code)]
pub enum TraceLevel {
    Minimal,
    Full,
}
