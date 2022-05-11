use crate::{
    interp::Interp,
    vm::{
        byte_seq::{ByteSeq, PutValue},
        instruction::{CompiledCode, Instruction},
        stack::Stack,
        value::{Array, Func, Pointer, Value},
    },
};
use chili_ast::ty::TyKind;
use colored::Colorize;
use std::fmt::Display;
use ustr::ustr;

pub(crate) mod byte_seq;
mod cast;
pub mod display;
mod index;
pub mod instruction;
mod stack;
pub mod value;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (std::u8::MAX as usize) + 1;

pub type Constants = Vec<Value>;
pub type Globals = Vec<Value>;

#[derive(Debug, Clone)]
pub(crate) struct StackFrame {
    func: Func,
    stack_slot: usize,
    inst_pointer: usize,
}

impl StackFrame {
    pub(crate) fn new(func: Func, slot: usize) -> Self {
        Self {
            func,
            stack_slot: slot,
            inst_pointer: 0,
        }
    }
}

impl Display for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:06}\t{}>", self.inst_pointer, self.func.name)
    }
}

macro_rules! binary_op {
    ($vm:expr, $op:tt) => {
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
            (Value::F32(a), Value::F32(b)) => $vm.stack.push(Value::F32(a $op b)),
            (Value::F64(a), Value::F64(b)) => $vm.stack.push(Value::F64(a $op b)),
            _=> panic!("invalid types in binary operation `{}` : `{}` and `{}`", stringify!($op), a.to_string() ,b.to_string())
        }

        $vm.next_inst();
    };
}

macro_rules! comp_op {
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
            _ => panic!("invalid types in compare operation `{}` and `{}`", a.to_string() ,b.to_string())
        }

        $vm.next_inst();
    };
}

macro_rules! logic_op {
    ($vm:expr, $op:tt) => {
        let b = $vm.stack.pop();
        let a = $vm.stack.pop();

        $vm.stack.push(Value::Bool(a.into_bool() $op b.into_bool()));

        $vm.next_inst();
    };
}

pub(crate) struct VM<'vm> {
    pub(crate) interp: &'vm mut Interp,
    pub(crate) stack: Stack<Value, STACK_MAX>,
    pub(crate) frames: Stack<StackFrame, FRAMES_MAX>,
    // pub(crate) bytecode: Bytecode<'vm>,
}

impl<'vm> VM<'vm> {
    pub(crate) fn new(interp: &'vm mut Interp) -> Self {
        Self {
            interp,
            stack: Stack::new(),
            frames: Stack::new(),
        }
    }

    pub(crate) fn run_code(&'vm mut self, code: CompiledCode) -> Value {
        self.run_func(Func {
            name: ustr("__vm_start"),
            arg_types: vec![],
            return_type: TyKind::Unit,
            code,
        })
    }

    pub(crate) fn run_func(&'vm mut self, func: Func) -> Value {
        self.stack.push(Value::Func(func.clone()));
        self.push_frame(func);
        self.run_inner()
    }

    fn run_inner(&'vm mut self) -> Value {
        loop {
            let inst = self.inst();

            self.trace(&inst, TraceLevel::None);
            // std::thread::sleep(std::time::Duration::from_millis(10));

            match inst {
                Instruction::Noop => {
                    self.next_inst();
                }
                Instruction::Pop => {
                    self.stack.pop();
                    self.next_inst();
                }
                Instruction::PushConst(addr) => {
                    self.stack.push(self.get_const(addr).clone());
                    self.next_inst();
                }
                Instruction::Add => {
                    binary_op!(self, +);
                }
                Instruction::Sub => {
                    binary_op!(self, -);
                }
                Instruction::Mul => {
                    binary_op!(self, *);
                }
                Instruction::Div => {
                    binary_op!(self, /);
                }
                Instruction::Rem => {
                    binary_op!(self, %);
                }
                Instruction::Neg => {
                    match self.stack.pop() {
                        Value::Int(v) => self.stack.push(Value::Int(-v)),
                        value => panic!("invalid value {}", value.to_string()),
                    }
                    self.next_inst();
                }
                Instruction::Not => {
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
                    self.next_inst();
                }
                Instruction::Deref => {
                    match self.stack.pop() {
                        Value::Pointer(ptr) => {
                            let value = unsafe { ptr.deref_value() };
                            self.stack.push(value);
                        }
                        value => panic!("invalid value {}", value.to_string()),
                    }
                    self.next_inst();
                }
                Instruction::Eq => {
                    comp_op!(self, ==);
                }
                Instruction::Neq => {
                    comp_op!(self, !=);
                }
                Instruction::Lt => {
                    comp_op!(self, <);
                }
                Instruction::LtEq => {
                    comp_op!(self, <=);
                }
                Instruction::Gt => {
                    comp_op!(self, >);
                }
                Instruction::GtEq => {
                    comp_op!(self, >=);
                }
                Instruction::And => {
                    logic_op!(self, &&);
                }
                Instruction::Or => {
                    logic_op!(self, ||);
                }
                Instruction::Jmp(offset) => {
                    self.jmp(offset);
                }
                Instruction::Jmpt(offset) => {
                    if self.stack.pop().into_bool() {
                        self.jmp(offset);
                    } else {
                        self.next_inst();
                    }
                }
                Instruction::Jmpf(offset) => {
                    if !self.stack.pop().into_bool() {
                        self.jmp(offset);
                    } else {
                        self.next_inst();
                    }
                }
                Instruction::Return => {
                    let frame = self.frames.pop();
                    let return_value = self.stack.pop();

                    if self.frames.is_empty() {
                        break return_value;
                    } else {
                        self.stack
                            .truncate(frame.stack_slot - frame.func.arg_types.len());
                        self.stack.push(return_value);
                        self.next_inst();
                    }
                }
                Instruction::Call(arg_count) => {
                    let value = self.stack.peek(0);
                    match value {
                        Value::Func(func) => {
                            let func = func.clone();
                            self.push_frame(func)
                        }
                        Value::ForeignFunc(func) => {
                            let func = func.clone();
                            self.stack.pop(); // this pops the actual foreign function

                            let mut values = (0..arg_count)
                                .into_iter()
                                .map(|_| self.stack.pop())
                                .collect::<Vec<Value>>();

                            values.reverse();

                            let vm_ptr = self as *mut VM;
                            let result = unsafe { self.interp.ffi.call(vm_ptr, func, values) };
                            self.stack.push(result);

                            self.next_inst();
                        }
                        _ => panic!("tried to call uncallable value `{}`", value.to_string()),
                    }
                }
                Instruction::GetGlobal(slot) => {
                    match self.interp.globals.get(slot as usize) {
                        Some(value) => self.stack.push(value.clone()),
                        None => panic!("undefined global `{}`", slot),
                    }
                    self.next_inst();
                }
                Instruction::GetGlobalPtr(slot) => {
                    match self.interp.globals.get_mut(slot as usize) {
                        Some(value) => self.stack.push(Value::Pointer(value.into())),
                        None => panic!("undefined global `{}`", slot),
                    }
                    self.next_inst();
                }
                Instruction::SetGlobal(slot) => {
                    self.interp.globals[slot as usize] = self.stack.pop();
                    self.next_inst();
                }
                Instruction::Peek(slot) => {
                    let slot = self.frame().stack_slot as isize + slot as isize;
                    let value = self.stack.get(slot as usize).clone();
                    self.stack.push(value);
                    self.next_inst();
                }
                Instruction::PeekPtr(slot) => {
                    let slot = self.frame().stack_slot as isize + slot as isize;
                    let value = self.stack.get_mut(slot as usize);
                    let value = Value::Pointer(value.into());
                    self.stack.push(value);
                    self.next_inst();
                }
                Instruction::SetLocal(slot) => {
                    let slot = self.frame().stack_slot as isize + slot as isize;
                    let value = self.stack.pop();
                    self.stack.set(slot as usize, value);
                    self.next_inst();
                }
                Instruction::Index => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.index(value, index);
                    self.next_inst();
                }
                Instruction::IndexPtr => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.index_ptr(value, index);
                    self.next_inst();
                }
                Instruction::Offset => {
                    let index = self.stack.pop().into_uint();
                    let value = self.stack.pop();
                    self.offset(value, index);
                    self.next_inst();
                }
                Instruction::ConstIndex(index) => {
                    let value = self.stack.pop();
                    self.index(value, index as usize);
                    self.next_inst();
                }
                Instruction::ConstIndexPtr(index) => {
                    let value = self.stack.pop();
                    self.index_ptr(value, index as usize);
                    self.next_inst();
                }
                Instruction::Assign => {
                    let lvalue = self.stack.pop().into_pointer();
                    let rvalue = self.stack.pop();
                    unsafe { lvalue.write_value(rvalue) }
                    self.next_inst();
                }
                Instruction::Cast(cast) => {
                    self.cast_inst(cast);
                    self.next_inst();
                }
                Instruction::AggregateAlloc => {
                    self.stack.push(Value::unit());
                    self.next_inst();
                }
                Instruction::AggregatePush => {
                    let value = self.stack.pop();
                    let aggregate = self.stack.peek_mut(0).as_aggregate_mut();
                    aggregate.elements.push(value);
                    self.next_inst();
                }
                Instruction::ArrayAlloc(size) => {
                    let ty = self.stack.pop().into_type();
                    self.stack.push(Value::Array(Array {
                        bytes: ByteSeq::new(size as usize),
                        ty,
                    }));
                    self.next_inst();
                }
                Instruction::ArrayPut(pos) => {
                    let value = self.stack.pop();
                    let array = self.stack.peek_mut(0).as_array_mut();

                    let offset_bytes = array.bytes.offset_mut(pos as usize);
                    offset_bytes.put_value(&value);

                    self.next_inst();
                }
                Instruction::ArrayFill(size) => {
                    let value = self.stack.pop();
                    let array = self.stack.peek_mut(0).as_array_mut();

                    for _ in 0..size {
                        array.bytes.put_value(&value);
                    }

                    self.next_inst();
                }
                Instruction::Copy(offset) => {
                    let value = self.stack.peek(offset as usize).clone();
                    self.stack.push(value);
                    self.next_inst();
                }
                Instruction::Roll(offset) => {
                    let value = self.stack.take(offset as usize);
                    self.stack.push(value);
                    self.next_inst();
                }
                Instruction::Increment => {
                    let ptr = self.stack.pop().into_pointer();
                    unsafe {
                        match ptr {
                            Pointer::I8(v) => *v += 1,
                            Pointer::I16(v) => *v += 1,
                            Pointer::I32(v) => *v += 1,
                            Pointer::I64(v) => *v += 1,
                            Pointer::Int(v) => *v += 1,
                            Pointer::U8(v) => *v += 1,
                            Pointer::U16(v) => *v += 1,
                            Pointer::U32(v) => *v += 1,
                            Pointer::U64(v) => *v += 1,
                            Pointer::Uint(v) => *v += 1,
                            _ => panic!("invalid pointer in increment {:?}", ptr),
                        }
                    }
                    self.next_inst();
                }
                Instruction::Panic => {
                    // Note (Ron): the panic message is a slice, which is an aggregate in the VM
                    let message = self.stack.pop().into_aggregate();
                    let ptr = message.elements[0].clone().into_pointer().as_inner_raw();
                    let len = message.elements[1].clone().into_uint();
                    let slice = unsafe { std::slice::from_raw_parts(ptr as *mut u8, len) };
                    let str = std::str::from_utf8(slice).unwrap();

                    // TODO: instead of using Rust's panic, we should be using our own panic function
                    panic!("{}", str);
                }
                Instruction::Halt => {
                    let result = self.stack.pop();
                    self.stack.pop(); // pop the current function
                    break result;
                }
            }
        }
    }

    #[inline]
    pub(crate) fn push_frame(&mut self, func: Func) {
        let stack_slot = self.stack.len() - 1;
        for _ in 0..func.code.locals {
            self.stack.push(Value::unit());
        }
        self.frames.push(StackFrame::new(func, stack_slot));
    }

    #[inline]
    pub(crate) fn frame(&self) -> &StackFrame {
        self.frames.peek(0)
    }

    #[inline]
    pub(crate) fn frame_mut(&mut self) -> &mut StackFrame {
        self.frames.peek_mut(0)
    }

    #[inline]
    pub(crate) fn inst(&self) -> Instruction {
        let frame = self.frame();
        frame.func.code.instructions[frame.inst_pointer]
    }

    #[inline]
    pub(crate) fn next_inst(&mut self) {
        self.frame_mut().inst_pointer += 1;
    }

    #[inline]
    pub(crate) fn get_const(&self, addr: u32) -> &Value {
        self.interp.constants.get(addr as usize).unwrap()
    }

    #[inline]
    pub(crate) fn jmp(&mut self, offset: i32) {
        let new_inst_pointer = self.frame().inst_pointer as isize + offset as isize;
        self.frame_mut().inst_pointer = new_inst_pointer as usize;
    }

    pub(crate) fn trace(&self, inst: &Instruction, level: TraceLevel) {
        let frame = self.frame();

        match level {
            TraceLevel::None => (),
            TraceLevel::Minimal => {
                println!(
                    "{:06}\t{:<20}{}",
                    frame.inst_pointer,
                    inst.to_string().bold(),
                    format!("[stack items: {}]", self.stack.len()).bright_cyan()
                );
            }
            TraceLevel::Full => {
                println!("{:06}\t{}", frame.inst_pointer, inst.to_string().bold());

                print!("\t[");

                let frame_slot = frame.stack_slot;

                for (index, value) in self.stack.iter().enumerate() {
                    print!(
                        "{}",
                        if index == frame_slot {
                            // frame slot
                            value.to_string().bright_yellow()
                        } else if index > frame_slot - frame.func.arg_types.len()
                            && index <= frame_slot + frame.func.code.locals as usize
                        {
                            // local value
                            value.to_string().bright_magenta()
                        } else {
                            // any other value
                            value.to_string().bright_cyan()
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
pub(crate) enum TraceLevel {
    None,
    Minimal,
    Full,
}
