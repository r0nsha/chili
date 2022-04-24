use crate::{
    ffi::call_foreign_func,
    instruction::{Bytecode, Instruction},
    interp::Interp,
    stack::Stack,
    value::{Func, Value},
};
use colored::Colorize;
use std::fmt::Display;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (std::u8::MAX as usize) + 1;

pub type Constants = Vec<Value>;
pub type Globals = Vec<Value>;

#[derive(Debug, Clone)]
struct CallFrame {
    func: Func,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(func: Func, slot: usize) -> Self {
        Self { func, ip: 0, slot }
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:06}\t{}>", self.ip, self.func.name,)
    }
}

macro_rules! binary_op {
    ($stack: expr, $op: tt) => {
        let b = $stack.pop();
        let a = $stack.pop();

        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => $stack.push(Value::Int(a $op b)),
            _=> panic!("invalid types in binary operation `{}` and`{}`", a ,b)
        }
    };
}

macro_rules! comp_op {
    ($stack: expr, $op: tt) => {
        let b = $stack.pop();
        let a = $stack.pop();

        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => $stack.push(Value::Bool(a $op b)),
            (Value::Bool(a), Value::Bool(b)) => $stack.push(Value::Bool(a $op b)),
            _ => panic!("invalid types incompare operation `{}` and `{}`", a ,b)
        }
    };
}

macro_rules! logic_op {
    ($stack: expr, $op: tt) => {
        let b = $stack.pop();
        let a = $stack.pop();

        $stack.push(Value::Bool(a.is_truthy() $op b.is_truthy()));
    };
}

pub(crate) struct VM<'vm> {
    interp: &'vm Interp,
    stack: Stack<Value, STACK_MAX>,
    frames: Stack<CallFrame, FRAMES_MAX>,
    // ffi: FFI,
}

impl<'vm> VM<'vm> {
    pub(crate) fn new(interp: &'vm Interp) -> Self {
        Self {
            interp,
            stack: Stack::new(),
            frames: Stack::new(),
            // ffi: FFI::new(),
        }
    }

    pub(crate) fn run(&'vm mut self, code: Bytecode) -> Value {
        let function = Func {
            name: "root".to_string(),
            param_count: 0,
            code,
        };

        self.frames.push(CallFrame::new(function, 0));

        self.run_loop()
    }

    fn run_loop(&'vm mut self) -> Value {
        loop {
            let inst = self.code()[self.frames.peek(0).ip];

            self.trace(&self.frames.peek(0).ip, &inst);

            self.frames.peek_mut().ip += 1;

            match inst {
                Instruction::Noop => (),
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::Const(addr) => {
                    self.stack.push(self.get_const(addr).clone());
                }
                Instruction::Add => {
                    binary_op!(self.stack, +);
                }
                Instruction::Sub => {
                    binary_op!(self.stack, -);
                }
                Instruction::Mul => {
                    binary_op!(self.stack, *);
                }
                Instruction::Div => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();

                    match (b, a) {
                        (Value::Int(b), Value::Int(a)) => {
                            if a == 0 {
                                panic!("divide by zero")
                            }

                            self.stack.push(Value::Int(b / a))
                        }
                        _ => panic!("invalid types in division"),
                    }
                }
                Instruction::Rem => {
                    binary_op!(self.stack, %);
                }
                Instruction::Neg => match self.stack.pop() {
                    Value::Int(v) => self.stack.push(Value::Int(-v)),
                    _ => panic!("invalid type in neg"),
                },
                Instruction::Not => {
                    let value = self.stack.pop();
                    self.stack.push(Value::Bool(!value.is_truthy()));
                }
                Instruction::Eq => {
                    comp_op!(self.stack, ==);
                }
                Instruction::Neq => {
                    comp_op!(self.stack, !=);
                }
                Instruction::Lt => {
                    comp_op!(self.stack, <);
                }
                Instruction::LtEq => {
                    comp_op!(self.stack, <=);
                }
                Instruction::Gt => {
                    comp_op!(self.stack, >);
                }
                Instruction::GtEq => {
                    comp_op!(self.stack, >=);
                }
                Instruction::And => {
                    logic_op!(self.stack, &&);
                }
                Instruction::Or => {
                    logic_op!(self.stack, ||);
                }
                Instruction::Jmp(addr) => {
                    self.jmp(addr);
                }
                Instruction::Jmpt(addr) => {
                    let value = self.stack.peek(0);
                    if value.is_truthy() {
                        self.jmp(addr);
                    }
                }
                Instruction::Jmpf(addr) => {
                    let value = self.stack.peek(0);
                    if !value.is_truthy() {
                        self.jmp(addr);
                    }
                }
                Instruction::Return => {
                    let frame = self.frames.pop();
                    let return_value = self.stack.pop();

                    if self.frames.is_empty() {
                        break return_value;
                    } else {
                        self.stack.truncate(frame.slot - frame.func.param_count);
                        self.stack.push(return_value);
                    }
                }
                Instruction::Call(arg_count) => {
                    let value = self.stack.peek(0);
                    match value {
                        Value::Func(func) => {
                            let frame = CallFrame::new(func.clone(), self.stack.len() - 1);
                            self.frames.push(frame);
                        }
                        Value::ForeignFunc(func) => {
                            let func = func.clone();

                            self.stack.pop(); // this pops the actual foreign function

                            let mut values = (0..arg_count)
                                .into_iter()
                                .map(|_| self.stack.pop())
                                .collect::<Vec<Value>>();
                            values.reverse();

                            // TODO: call_foreign_func should return a `Value`
                            let result = call_foreign_func(func, values);
                            self.stack.push(Value::Int(result as i64));
                        }
                        _ => panic!("tried to call an uncallable value `{}`", value),
                    }
                }
                Instruction::GetGlobal(slot) => {
                    match self.interp.globals.get(slot) {
                        Some(value) => self.stack.push(value.clone()),
                        None => panic!("undefined global `{}`", slot),
                    };
                }
                // Instruction::SetGlobal(name) => {
                //     self.globals.insert(name, self.stack.pop());
                // }
                Instruction::GetLocal(slot) => {
                    let slot = self.frames.peek(0).slot as isize + slot;
                    let value = self.stack.get(slot as usize).clone();
                    self.stack.push(value);
                }
                // Instruction::SetLocal(slot) => {
                //     let slot = self.frames.peek(0).slot as isize + slot;
                //     let value = self.stack.peek(0).clone();
                //     self.stack.set(slot as usize, value);
                // }
                Instruction::Access(member) => {
                    todo!("access")
                }
                Instruction::Index(index) => {
                    let value = self.stack.pop();

                    match value {
                        Value::Tuple(elements) => self.stack.push(elements[index].clone()),
                        Value::Slice(slice) => match index {
                            0 => self.stack.push(Value::Ptr(slice.ptr)),
                            1 => self.stack.push(Value::Int(slice.len as _)),
                            _ => panic!("invalid index {}", index),
                        },
                        _ => panic!("invalid value {}", value),
                    }
                }
                Instruction::Halt => break self.stack.pop(),
            }
        }
    }

    fn code(&self) -> &Bytecode {
        &self.func().code
    }

    fn func(&self) -> &Func {
        &self.frames.peek(0).func
    }

    fn get_const(&self, addr: usize) -> &Value {
        self.interp.constants.get(addr).unwrap()
    }

    fn jmp(&mut self, offset: isize) {
        let new_ip = self.frames.peek_mut().ip as isize + offset;
        self.frames.peek_mut().ip = new_ip as usize;
    }

    fn trace(&self, ip: &usize, inst: &Instruction) {
        let stack_trace = self.stack.trace();

        println!(
            "{:06}\t{}\n\t{}",
            ip,
            inst.to_string().bold(),
            stack_trace.blue()
        );
    }
}
