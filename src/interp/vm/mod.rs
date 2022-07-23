use self::{bytecode::Inst, value::FunctionValue};
use super::{
    ffi::RawPointer,
    interp::Interp,
    vm::{
        byte_seq::{ByteSeq, PutValue},
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
pub mod inst;
mod intrinsics;
mod stack;
pub mod value;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (std::u8::MAX as usize) + 1;

pub type Constants = Vec<Value>;
pub type Globals = Vec<Value>;

#[derive(Debug, Clone)]
pub struct StackFrame {
    func: *const Function,
    stack_slot: usize,
    ip: usize,
}

impl StackFrame {
    pub fn new(func: *const Function, slot: usize) -> Self {
        Self {
            func,
            stack_slot: slot,
            ip: 0,
        }
    }

    #[inline]
    pub fn func(&self) -> &Function {
        debug_assert!(!self.func.is_null());
        unsafe { &*self.func }
    }
}

impl Display for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:06}\t{}>", self.ip, self.func().name)
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

pub struct VM<'vm> {
    pub interp: &'vm mut Interp,
    pub stack: Stack<Value, STACK_MAX>,
    pub frames: Stack<StackFrame, FRAMES_MAX>,
    pub frame: *mut StackFrame,
}

impl<'vm> VM<'vm> {
    pub fn new(interp: &'vm mut Interp) -> Self {
        Self {
            interp,
            stack: Stack::new(),
            frames: Stack::new(),
            frame: ptr::null_mut(),
        }
    }

    pub fn run_func(&'vm mut self, function: Function) -> Value {
        self.push_frame(&function as *const Function);
        self.run_inner()
    }

    fn run_inner(&'vm mut self) -> Value {
        loop {
            let frame = self.frame();
            let inst = frame.func().code.instructions[frame.ip];

            // self.trace(&inst, TraceLevel::Full);

            match inst {
                Inst::Noop => {
                    self.next();
                }
                Inst::Pop => {
                    self.stack.pop();
                    self.next();
                }
                Inst::LoadConst(addr) => {
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
                Inst::Add => {
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
                Inst::Sub => {
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
                Inst::Mul => {
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
                Inst::Div => {
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
                Inst::Rem => {
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
                Inst::Neg => {
                    match self.stack.pop() {
                        Value::Int(v) => self.stack.push(Value::Int(-v)),
                        value => panic!("invalid value {}", value.to_string()),
                    }
                    self.next();
                }
                Inst::Not => {
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
                Inst::Deref => {
                    match self.stack.pop() {
                        Value::Pointer(ptr) => {
                            let value = unsafe { ptr.deref_value() };
                            self.stack.push(value);
                        }
                        value => panic!("invalid value {}", value.to_string()),
                    }
                    self.next();
                }
                Inst::Eq => {
                    compare_op!(self, ==);
                }
                Inst::Ne => {
                    compare_op!(self, !=);
                }
                Inst::Lt => {
                    compare_op!(self, <);
                }
                Inst::Le => {
                    compare_op!(self, <=);
                }
                Inst::Gt => {
                    compare_op!(self, >);
                }
                Inst::Ge => {
                    compare_op!(self, >=);
                }
                Inst::And => {
                    logic_op!(self, &&);
                }
                Inst::Or => {
                    logic_op!(self, ||);
                }
                Inst::Shl => {
                    binary_op_int_only!(self, <<)
                }
                Inst::Shr => {
                    binary_op_int_only!(self, >>);
                }
                Inst::Xor => {
                    binary_op_int_only!(self, ^);
                }
                Inst::Jmp(offset) => {
                    self.jmp(offset);
                }
                Inst::Jmpf(offset) => {
                    if !self.stack.pop().into_bool() {
                        self.jmp(offset);
                    } else {
                        self.next();
                    }
                }
                Inst::Return => {
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
                Inst::Call(arg_count) => match self.stack.pop() {
                    Value::Function(addr) => {
                        match self.interp.get_function(addr.id).unwrap_or_else(|| {
                            panic!("couldn't find '{}' {:?}", addr.name, addr.id)
                        }) {
                            FunctionValue::Orphan(function) => {
                                self.push_frame(function as *const Function);
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
                },
                Inst::LoadGlobal(slot) => {
                    match self.interp.globals.get(slot as usize) {
                        Some(value) => self.stack.push(value.clone()),
                        None => panic!("undefined global `{}`", slot),
                    }
                    self.next();
                }
                Inst::LoadGlobalPtr(slot) => {
                    match self.interp.globals.get_mut(slot as usize) {
                        Some(value) => self.stack.push(Value::Pointer(value.into())),
                        None => panic!("undefined global `{}`", slot),
                    }
                    self.next();
                }
                Inst::StoreGlobal(slot) => {
                    self.interp.globals[slot as usize] = self.stack.pop();
                    self.next();
                }
                Inst::Peek(slot) => {
                    let slot = self.frame().stack_slot as isize + slot as isize;
                    let value = self.stack.get(slot as usize).clone();
                    self.stack.push(value);
                    self.next();
                }
                Inst::PeekPtr(slot) => {
                    let slot = self.frame().stack_slot as isize + slot as isize;
                    let value = self.stack.get_mut(slot as usize);
                    let value = Value::Pointer(value.into());
                    self.stack.push(value);
                    self.next();
                }
                Inst::StoreLocal(slot) => {
                    let slot = self.frame().stack_slot as isize + slot as isize;
                    let value = self.stack.pop();
                    self.stack.set(slot as usize, value);
                    self.next();
                }
                Inst::Index => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.index(value, index);
                    self.next();
                }
                Inst::IndexPtr => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.index_ptr(value, index);
                    self.next();
                }
                Inst::Offset => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.offset(value, index);
                    self.next();
                }
                Inst::ConstIndex(index) => {
                    let value = self.stack.pop();
                    self.index(value, index as usize);
                    self.next();
                }
                Inst::ConstIndexPtr(index) => {
                    let value = self.stack.pop();
                    self.index_ptr(value, index as usize);
                    self.next();
                }
                Inst::Assign => {
                    let lhs = self.stack.pop().into_pointer();
                    let rhs = self.stack.pop();
                    unsafe { lhs.write_value(rhs) }
                    self.next();
                }
                Inst::Cast => {
                    self.cast_inst();
                    self.next();
                }
                Inst::BufferAlloc(size) => {
                    let ty = self.stack.pop().into_type();
                    self.stack.push(Value::Buffer(Buffer {
                        bytes: ByteSeq::new(size as usize),
                        ty,
                    }));
                    self.next();
                }
                Inst::BufferPut(pos) => {
                    let value = self.stack.pop();

                    let buf = self.stack.peek_mut(0).as_buffer_mut();
                    buf.bytes.offset_mut(pos as usize).put_value(&value);

                    self.next();
                }
                Inst::BufferFill(size) => {
                    let value = self.stack.pop();
                    let buf = self.stack.peek_mut(0).as_buffer_mut();

                    for _ in 0..size {
                        buf.bytes.put_value(&value);
                    }

                    self.next();
                }
                Inst::Copy(offset) => {
                    let value = self.stack.peek(offset as usize).clone();
                    self.stack.push(value);
                    self.next();
                }
                Inst::Swap(offset) => {
                    let last_index = self.stack.len() - 1;
                    self.stack.swap(last_index, last_index - offset as usize);
                    self.next();
                }
                Inst::Halt => {
                    let result = self.stack.pop();
                    break result;
                }
            }
        }
    }

    #[inline]
    pub fn push_frame(&mut self, func: *const Function) {
        debug_assert!(!func.is_null());

        let stack_slot = self.stack.len();

        let locals = unsafe { &*func }.code.locals;
        for _ in 0..locals {
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
    pub fn trace(&self, inst: &Inst, level: TraceLevel) {
        let frame = self.frame();

        match level {
            TraceLevel::Minimal => {
                println!(
                    "{:06}\t{:<20}{}",
                    frame.ip,
                    inst.to_string().bold(),
                    format!("[stack items: {}]", self.stack.len()).bright_cyan()
                );
            }
            TraceLevel::Full => {
                println!("{:06}\t{}", frame.ip, inst.to_string().bold());
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
