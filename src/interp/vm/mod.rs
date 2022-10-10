use self::{
    bytecode::{BytecodeReader, Op},
    value::{FunctionValue, IntrinsicFunction, Pointer},
};
use super::{
    ffi::RawPointer,
    interp::Interp,
    vm::{
        byte_seq::{ByteSeq, PutValue},
        disassemble::bytecode_reader_write_single_inst,
        value::{Buffer, Function, Value},
    },
    workspace::{BuildTargetValue, OptimizationLevelValue, WorkspaceValue},
};
use crate::{
    common::{
        build_options::{BuildOptions, CodegenOptions, OptimizationLevel},
        target::TargetPlatform,
    },
    types::{FloatType, InferType, IntType, Type, UintType},
};
use bumpalo::Bump;
use colored::Colorize;
use path_absolutize::Absolutize;
use std::{ffi::c_void, fmt::Display, path::PathBuf, ptr};
use ustr::ustr;

macro_rules! cast_to_int {
    ($value:expr => $name:ident, $to:ty) => {
        match $value {
            Value::I8(v) => Value::$name(v as $to),
            Value::I16(v) => Value::$name(v as $to),
            Value::I32(v) => Value::$name(v as $to),
            Value::I64(v) => Value::$name(v as $to),
            Value::Int(v) => Value::$name(v as $to),
            Value::U8(v) => Value::$name(v as $to),
            Value::U16(v) => Value::$name(v as $to),
            Value::U32(v) => Value::$name(v as $to),
            Value::U64(v) => Value::$name(v as $to),
            Value::Uint(v) => Value::$name(v as $to),
            Value::F32(v) => Value::$name(v as $to),
            Value::F64(v) => Value::$name(v as $to),
            Value::Bool(v) => Value::$name(v as $to),
            _ => panic!("invalid value {}", $value.to_string()),
        }
    };
}

// Note (Ron): We have a variant for floats since bool can't be cast to float
macro_rules! cast_to_float {
    ($value:expr => $name:ident, $to:ty) => {
        match $value {
            Value::I8(v) => Value::$name(v as $to),
            Value::I16(v) => Value::$name(v as $to),
            Value::I32(v) => Value::$name(v as $to),
            Value::I64(v) => Value::$name(v as $to),
            Value::Int(v) => Value::$name(v as $to),
            Value::U8(v) => Value::$name(v as $to),
            Value::U16(v) => Value::$name(v as $to),
            Value::U32(v) => Value::$name(v as $to),
            Value::U64(v) => Value::$name(v as $to),
            Value::Uint(v) => Value::$name(v as $to),
            Value::F32(v) => Value::$name(v as $to),
            Value::F64(v) => Value::$name(v as $to),
            _ => panic!("invalid value {}", $value.to_string()),
        }
    };
}

pub mod byte_seq;
pub mod bytecode;
pub mod disassemble;
pub mod value;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (std::u8::MAX as usize) + 1;

pub type Constants = Vec<Value>;
pub type Globals = Vec<Value>;

#[derive(Debug)]
pub struct Stack<T, const CAPACITY: usize> {
    inner: Vec<T>,
}

impl<T: ToString, const CAPACITY: usize> Stack<T, CAPACITY> {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(CAPACITY),
        }
    }

    pub fn push(&mut self, value: T) {
        debug_assert!(self.inner.len() <= self.inner.capacity(), "stack overflow");
        self.inner.push(value);
    }

    #[inline]
    pub fn pop(&mut self) -> T {
        self.inner.pop().unwrap()
    }

    #[inline]
    pub fn peek(&self, offset: usize) -> &T {
        &self.inner[self.len() - 1 - offset]
    }

    #[inline]
    pub fn peek_mut(&mut self, offset: usize) -> &mut T {
        let len = self.len();
        &mut self.inner[len - 1 - offset]
    }

    #[allow(unused)]
    #[inline]
    pub fn last(&self) -> &T {
        self.inner.last().unwrap()
    }

    #[inline]
    pub fn last_mut(&mut self) -> &mut T {
        self.inner.last_mut().unwrap()
    }

    #[inline]
    pub fn swap(&mut self, a: usize, b: usize) {
        self.inner.swap(a, b)
    }

    #[inline]
    pub fn get(&self, index: usize) -> &T {
        &self.inner[index]
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.inner[index]
    }

    #[inline]
    pub fn set(&mut self, index: usize, value: T) {
        self.inner[index] = value;
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.inner.truncate(len)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.inner.iter()
    }
}

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
    pub fn new(func: *const Function, slot: usize) -> Self {
        Self {
            func,
            reader: unsafe { &*func }.code.reader(),
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
    };
}

macro_rules! logic_op {
    ($vm:expr, $op:tt) => {
        let b = $vm.stack.pop();
        let a = $vm.stack.pop();

        $vm.stack.push(Value::Bool(a.into_bool() $op b.into_bool()));
    };
}

pub struct VM<'vm> {
    pub interp: &'vm mut Interp,
    pub bump: &'vm mut Bump,
    pub stack: Stack<Value, STACK_MAX>,
    pub frames: Stack<StackFrame<'vm>, FRAMES_MAX>,
    pub frame: *mut StackFrame<'vm>,
}

impl<'vm> VM<'vm> {
    pub fn new(interp: &'vm mut Interp, bump: &'vm mut Bump) -> Self {
        Self {
            interp,
            bump,
            stack: Stack::new(),
            frames: Stack::new(),
            frame: ptr::null_mut(),
        }
    }

    pub fn run_function(&mut self, function: Function) -> Value {
        self.push_frame(&function);
        self.run_inner()
    }

    fn run_inner(&mut self) -> Value {
        loop {
            // self.trace(TraceLevel::Full);

            let reader = &mut self.frame_mut().reader;

            match reader.read_op() {
                Op::Pop => {
                    self.stack.pop();
                }
                Op::LoadConst => {
                    let addr = reader.read_u32();
                    let const_ = self.interp.constants.get(addr as usize).unwrap();

                    let value = match const_ {
                        Value::ExternVariable(variable) => {
                            let symbol =
                                unsafe { self.interp.ffi.load_symbol(ustr(&variable.lib.path()), variable.name) };

                            unsafe { Value::from_type_and_ptr(&variable.ty, *symbol as RawPointer) }
                        }
                        value => value.clone(),
                    };

                    self.stack.push(value);
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
                        (Value::Pointer(a), Value::Int(b)) => self.stack.push(Value::Pointer(unsafe { a.offset(*b) })),
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(+),
                            a.to_string(),
                            b.to_string()
                        ),
                    }
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
                        (Value::Pointer(a), Value::Int(b)) => self.stack.push(Value::Pointer(unsafe { a.offset(-*b) })),
                        _ => panic!(
                            "invalid types in binary operation `{}` : `{}` and `{}`",
                            stringify!(-),
                            a.to_string(),
                            b.to_string()
                        ),
                    }
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
                }
                Op::Neg => match self.stack.pop() {
                    Value::Int(v) => self.stack.push(Value::Int(-v)),
                    value => panic!("invalid value {}", value.to_string()),
                },
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
                }
                Op::Deref => match self.stack.pop() {
                    Value::Pointer(ptr) => {
                        let value = unsafe { ptr.deref_value() };
                        self.stack.push(value);
                    }
                    value => panic!("invalid value {}", value.to_string()),
                },
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
                    }
                }
                Op::Return => {
                    let frame = self.frames.pop();
                    let return_value = self.stack.pop();

                    if self.frames.is_empty() {
                        break return_value;
                    } else {
                        self.stack.truncate(frame.stack_slot - frame.func().ty.params.len());
                        self.frame = self.frames.last_mut() as _;
                        self.stack.push(return_value);
                    }
                }
                Op::Call => {
                    let arg_count = reader.read_u32();

                    match self.stack.pop() {
                        Value::Function(addr) => {
                            let function = self
                                .interp
                                .get_function(addr.id)
                                .unwrap_or_else(|| panic!("couldn't find '{}' {:?}", addr.name, addr.id));

                            match function {
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

                                    let result = unsafe { self.interp.ffi.call(function, values, vm_ptr, interp_ptr) };

                                    self.stack.push(result);
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
                }
                Op::LoadGlobalPtr => {
                    let slot = reader.read_u32();

                    match self.interp.globals.get_mut(slot as usize) {
                        Some(value) => self.stack.push(Value::Pointer(value.into())),
                        None => panic!("undefined global `{}`", slot),
                    }
                }
                Op::StoreGlobal => {
                    let slot = reader.read_u32();
                    self.interp.globals[slot as usize] = self.stack.pop();
                }
                Op::Peek => {
                    let offset = reader.read_i32();
                    let slot = self.frame().stack_slot as isize + offset as isize;

                    let value = self.stack.get(slot as usize).clone();
                    self.stack.push(value);
                }
                Op::PeekPtr => {
                    let offset = reader.read_i32();
                    let slot = self.frame().stack_slot as isize + offset as isize;

                    let value = self.stack.get_mut(slot as usize);
                    let value = Value::Pointer(value.into());
                    self.stack.push(value);
                }
                Op::StoreLocal => {
                    let offset = reader.read_i32();
                    let slot = self.frame().stack_slot as isize + offset as isize;

                    let value = self.stack.pop();
                    self.stack.set(slot as usize, value);
                }
                Op::Offset => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.offset(value, index);
                }
                Op::ConstIndex => {
                    let index = reader.read_u32();

                    let value = self.stack.pop();
                    self.index(value, index as usize);
                }
                Op::ConstIndexPtr => {
                    let index = reader.read_u32();

                    let value = self.stack.pop();
                    self.index_ptr(value, index as usize);
                }
                Op::Assign => {
                    let lhs = self.stack.pop().into_pointer();
                    let rhs = self.stack.pop();
                    unsafe { lhs.write_value(rhs) }
                }
                Op::Cast => {
                    self.cast_op();
                }
                Op::BufferAlloc => {
                    let size = reader.read_u32();

                    let ty = self.stack.pop().into_type();
                    self.stack.push(Value::Buffer(Buffer {
                        bytes: ByteSeq::new(size as usize),
                        ty,
                    }));
                }
                Op::BufferPut => {
                    let offset = reader.read_u32();

                    let value = self.stack.pop();

                    let buf = self.stack.peek_mut(0).as_buffer_mut();
                    buf.bytes.offset_mut(offset as usize).put_value(&value);
                }
                Op::BufferFill => {
                    let size = reader.read_u32();

                    let value = self.stack.pop();
                    let buf = self.stack.peek_mut(0).as_buffer_mut();

                    for _ in 0..size {
                        buf.bytes.put_value(&value);
                    }
                }
                Op::Copy => {
                    let offset = reader.read_u32();
                    let value = self.stack.peek(offset as usize).clone();
                    self.stack.push(value);
                }
                Op::Swap => {
                    let offset = reader.read_u32();
                    let last_index = self.stack.len() - 1;
                    self.stack.swap(last_index, last_index - offset as usize);
                }
                Op::Halt => {
                    let result = self.stack.pop();
                    break result;
                }
            }
        }
    }

    #[inline]
    pub fn push_frame(&mut self, function: *const Function) {
        let stack_slot = self.stack.len();

        for _ in 0..unsafe { &*function }.code.locals {
            self.stack.push(Value::default());
        }

        self.frames.push(StackFrame::<'vm>::new(function, stack_slot));

        self.frame = self.frames.last_mut() as _;
    }

    #[inline]
    pub fn frame(&self) -> &StackFrame<'vm> {
        debug_assert!(!self.frame.is_null());
        unsafe { &*self.frame }
    }

    #[inline]
    pub fn frame_mut(&mut self) -> &mut StackFrame<'vm> {
        debug_assert!(!self.frame.is_null());
        unsafe { &mut *self.frame }
    }

    #[inline]
    pub fn jmp(&mut self, offset: i32) {
        // The offset is calculated from the op-code's position.
        // The current actual offset the op-code's position + the amount of bytes,
        // The the jmp instruction's operand takes (which is the size of `i32`)
        const COMPENSATION: usize = std::mem::size_of::<i32>();

        let reader = &mut self.frame_mut().reader;
        let cursor = reader.cursor() - 1 - COMPENSATION;
        let new_cursor = cursor as isize + offset as isize;

        reader.set_cursor(new_cursor as usize);
    }

    #[inline]
    fn index(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Buffer(buf) => {
                    let buf = unsafe { &**buf };
                    let value = buf.get_value_at_index(index);
                    self.stack.push(value);
                }
                _ => panic!("invalid value {}", value.to_string()),
            },
            Value::Buffer(buf) => {
                let value = buf.get_value_at_index(index);
                self.stack.push(value);
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    fn index_ptr(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Buffer(buf) => {
                    let buf = unsafe { &mut **buf };
                    let ptr = buf.bytes.offset_mut(index).as_mut_ptr();
                    let value = Value::Pointer(Pointer::from_type_and_ptr(buf.ty.element_type().unwrap(), ptr as _));
                    self.stack.push(value);
                }
                _ => panic!("invalid value {}", value.to_string()),
            },
            Value::Buffer(_) => self.offset(value, index),
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    fn offset(&mut self, value: Value, offset: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Buffer(buf) => {
                    let buf = unsafe { &mut *buf };
                    let ptr = buf.bytes.offset_mut(offset).as_mut_ptr();
                    let value = Value::Pointer(Pointer::from_type_and_ptr(&buf.ty, ptr as _));
                    self.stack.push(value);
                }
                ptr => {
                    let ptr = if ptr.is_pointer() {
                        unsafe { &*ptr.into_pointer() }
                    } else {
                        &ptr
                    };

                    let raw = ptr.as_inner_raw();
                    let offset = unsafe { raw.add(offset) };

                    self.stack
                        .push(Value::Pointer(Pointer::from_kind_and_ptr(ptr.kind(), offset)))
                }
            },
            Value::Buffer(buf) => {
                let bytes = buf.bytes.offset(offset);
                let ptr = &bytes[0];
                self.stack.push(Value::Pointer(Pointer::from_type_and_ptr(
                    &buf.ty,
                    ptr as *const u8 as *mut u8 as _,
                )));
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => {
                let value = self.stack.pop();
                let workspace_value = WorkspaceValue::from(&value);

                let source_file = PathBuf::from(workspace_value.build_options.input_file.as_str())
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let output_file = PathBuf::from(workspace_value.build_options.output_file.as_str())
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let build_options = BuildOptions {
                    source_file,
                    output_file: Some(output_file),
                    target_platform: match &workspace_value.build_options.target {
                        BuildTargetValue::Auto => TargetPlatform::current().unwrap(),
                        BuildTargetValue::Linux => TargetPlatform::LinuxAmd64,
                        BuildTargetValue::Windows => TargetPlatform::WindowsAmd64,
                    },
                    optimization_level: match &workspace_value.build_options.optimization_level {
                        OptimizationLevelValue::Debug => OptimizationLevel::Debug,
                        OptimizationLevelValue::Release => OptimizationLevel::Release,
                    },
                    emit_times: self.interp.build_options.emit_times,
                    emit_hir: self.interp.build_options.emit_hir,
                    emit_bytecode: self.interp.build_options.emit_bytecode,
                    diagnostic_options: self.interp.build_options.diagnostic_options.clone(),
                    codegen_options: CodegenOptions::Codegen {
                        emit_llvm_ir: self.interp.build_options.codegen_options.emit_llvm_ir(),
                    },
                    include_paths: vec![],
                    check_mode: false,
                };

                let result = crate::driver::start_workspace(workspace_value.name.to_string(), build_options);

                let (output_file, ok) = if let Some(output_file) = &result.output_file {
                    // TODO: Remove null terminator after implementing printing/formatting
                    (
                        self.bump
                            .alloc_slice_copy(format!("{}\0", output_file.to_str().unwrap()).as_bytes()),
                        true,
                    )
                } else {
                    (self.bump.alloc_slice_copy(b""), false)
                };

                let result_type = Type::Tuple(vec![Type::str_pointer(), Type::Bool]);

                let result_value = Value::Buffer(Buffer::from_values(
                    [Value::Buffer(Buffer::from_str_bytes(output_file)), Value::Bool(ok)],
                    result_type,
                ));

                self.stack.push(result_value);
            }
        }
    }

    #[inline]
    fn cast_op(&mut self) {
        let ty = self.stack.pop().into_type();
        let value = self.stack.pop();

        let new_value = match ty {
            Type::Int(IntType::I8) => cast_to_int!(value => I8, i8),
            Type::Int(IntType::I16) => cast_to_int!(value => I16, i16),
            Type::Int(IntType::I32) => cast_to_int!(value => I32, i32),
            Type::Int(IntType::I64) => cast_to_int!(value => I64, i64),
            Type::Int(IntType::Int) | Type::Infer(_, InferType::AnyInt) => {
                cast_to_int!(value => Int, isize)
            }
            Type::Uint(UintType::U8) => cast_to_int!(value => U8, u8),
            Type::Uint(UintType::U16) => cast_to_int!(value => U16, u16),
            Type::Uint(UintType::U32) => cast_to_int!(value => U32, u32),
            Type::Uint(UintType::U64) => cast_to_int!(value => U64, u64),
            Type::Uint(UintType::Uint) => cast_to_int!(value => Uint, usize),
            Type::Float(FloatType::F32) => cast_to_float!(value => F32, f32),
            Type::Float(FloatType::F64) => cast_to_float!(value => F64, f64),
            Type::Float(FloatType::Float) | Type::Infer(_, InferType::AnyFloat) => {
                cast_to_float!(value => F64, f64)
            }
            Type::Pointer(inner, _) => {
                let raw_ptr = match value {
                    Value::Int(value) => value as RawPointer,
                    Value::Uint(value) => value as RawPointer,
                    Value::Pointer(ptr) => match ptr {
                        Pointer::Buffer(buf) => {
                            if buf.is_null() {
                                std::ptr::null_mut::<c_void>()
                            } else {
                                unsafe { &mut *buf }.bytes.as_mut_ptr() as _
                            }
                        }
                        _ => ptr.as_inner_raw(),
                    },
                    _ => panic!("invalid value {}", value.to_string()),
                };

                let new_ptr = Pointer::from_type_and_ptr(&inner, raw_ptr);
                Value::Pointer(new_ptr)
            }
            _ => panic!("{:?}", ty),
        };

        self.stack.push(new_value);
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
                print!("\n\t[");
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
    }
}

#[allow(dead_code)]
pub enum TraceLevel {
    Minimal,
    Full,
}
