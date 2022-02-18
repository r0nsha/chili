// pub mod ffi;
// pub mod instruction;
// pub mod interp;
// pub mod stack;
// pub mod value;

// use std::{
//     fmt::Display,
//     fs::OpenOptions,
//     io::{BufWrvalue, Write},
//     path::Path,
// };

// use colored::Colorize;
// use ffi::FFI;
// use instruction::Instruction;
// use stack::Stack;
// use ustr::{ustr, UstrMap};
// use value::{Function, Value};

// const FRAMES_MAX: usize = 64;
// const STACK_MAX: usize = FRAMES_MAX * (std::u8::MAX as usize) + 1;

// pub type Bytecode = Vec<Instruction>;
// pub type Constants = Vec<Value>;
// pub type Globals = UstrMap<Value>;

// #[derive(Debug, Clone)]
// struct CallFrame {
//     function: Function,
//     ip: usize,
//     slot: usize,
// }

// impl CallFrame {
//     fn new(function: Function, slot: usize) -> Self {
//         Self {
//             function,
//             ip: 0,
//             slot,
//         }
//     }
// }

// impl Display for CallFrame {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "<{:06}\t{}>", self.ip, self.function.name,)
//     }
// }

// pub fn run(globals: &mut Globals, constants: &mut Constants, code: Bytecode) -> Value {
//     let mut vm = VM::new(globals, constants);
//     vm.run(code)
// }

// macro_rules! binary_op {
//     ($stack: expr, $op: tt) => {
//         let b = $stack.pop();
//         let a = $stack.pop();

//         match (&a, &b) {
//             (Value::Int(a), Value::Int(b)) => $stack.push(Value::Int(a $op b)),
//             _=> panic!("invalid types in binary operation `{}` and `{}`", a ,b)
//         }
//     };
// }

// macro_rules! comp_op {
//     ($stack: expr, $op: tt) => {
//         let b = $stack.pop();
//         let a = $stack.pop();

//         match (&a, &b) {
//             (Value::Int(a), Value::Int(b)) => $stack.push(Value::Bool(a $op b)),
//             (Value::Bool(a), Value::Bool(b)) => $stack.push(Value::Bool(a $op b)),
//             _=> panic!("invalid types in compare operation `{}` and `{}`", a ,b)
//         }
//     };
// }

// macro_rules! logic_op {
//     ($stack: expr, $op: tt) => {
//         let b = $stack.pop();
//         let a = $stack.pop();

//         $stack.push(Value::Bool(a.is_truthy() $op b.is_truthy()));
//     };
// }

// struct VM<'vm> {
//     globals: &'vm mut Globals,
//     constants: &'vm mut Constants,
//     stack: Stack<Value, STACK_MAX>,
//     frames: Stack<CallFrame, FRAMES_MAX>,
//     ffi: FFI,
// }

// impl<'vm> VM<'vm> {
//     fn new(globals: &'vm mut Globals, constants: &'vm mut Constants) -> Self {
//         Self {
//             globals,
//             constants,
//             stack: Stack::new(),
//             frames: Stack::new(),
//             ffi: FFI::new(),
//         }
//     }

//     fn run(&'vm mut self, code: Bytecode) -> Value {
//         let function = Function {
//             name: ustr(""),
//             arg_count: 0,
//             code,
//         };

//         self.frames.push(CallFrame::new(function, 0));

//         self.run_loop()
//     }

//     fn run_loop(&'vm mut self) -> Value {
//         loop {
//             let inst = &self.current_code()[self.frames.peek(0).ip].clone();

//             // self.trace(&self.frames.peek(0).ip, &inst);

//             self.frames.peek_mut().ip += 1;

//             // std::thread::sleep(core::time::Duration::from_millis(100));

//             match inst {
//                 &Instruction::Noop => (),
//                 &Instruction::Pop => {
//                     self.stack.pop();
//                 }
//                 &Instruction::Const(addr) => {
//                     self.stack.push(self.get_const(addr).clone());
//                 }
//                 &Instruction::Add => {
//                     binary_op!(self.stack, +);
//                 }
//                 &Instruction::Sub => {
//                     binary_op!(self.stack, -);
//                 }
//                 &Instruction::Mul => {
//                     binary_op!(self.stack, *);
//                 }
//                 &Instruction::Div => {
//                     let b = self.stack.pop();
//                     let a = self.stack.pop();

//                     match (b, a) {
//                         (Value::Int(b), Value::Int(a)) => {
//                             if a == 0 {
//                                 panic!("divide by zero")
//                             }

//                             self.stack.push(Value::Int(b / a))
//                         }
//                         _ => panic!("invalid types in division"),
//                     }
//                 }
//                 &Instruction::Mod => {
//                     binary_op!(self.stack, %);
//                 }
//                 &Instruction::Neg => match self.stack.pop() {
//                     Value::Int(v) => self.stack.push(Value::Int(-v)),
//                     _ => panic!("invalid type in neg"),
//                 },
//                 &Instruction::Not => {
//                     let value = self.stack.pop();
//                     self.stack.push(Value::Bool(!value.is_truthy()));
//                 }
//                 &Instruction::Eq => {
//                     comp_op!(self.stack, ==);
//                 }
//                 &Instruction::NEq => {
//                     comp_op!(self.stack, !=);
//                 }
//                 &Instruction::Lt => {
//                     comp_op!(self.stack, <);
//                 }
//                 &Instruction::LtEq => {
//                     comp_op!(self.stack, <=);
//                 }
//                 &Instruction::Gt => {
//                     comp_op!(self.stack, >);
//                 }
//                 &Instruction::GtEq => {
//                     comp_op!(self.stack, >=);
//                 }
//                 &Instruction::BAnd => {
//                     logic_op!(self.stack, &&);
//                 }
//                 &Instruction::BOr => {
//                     logic_op!(self.stack, ||);
//                 }
//                 &Instruction::Jmp(addr) => {
//                     self.jmp(addr);
//                 }
//                 &Instruction::Jmpt(addr) => {
//                     let value = self.stack.peek(0);
//                     if value.is_truthy() {
//                         self.jmp(addr);
//                     }
//                 }
//                 &Instruction::Jmpf(addr) => {
//                     let value = self.stack.peek(0);
//                     if !value.is_truthy() {
//                         self.jmp(addr);
//                     }
//                 }
//                 &Instruction::Return => {
//                     let frame = self.frames.pop();
//                     let return_value = self.stack.pop();

//                     if self.frames.is_empty() {
//                         return return_value;
//                     } else {
//                         self.stack.truncate(frame.slot - frame.function.arg_count);
//                         self.stack.push(return_value);
//                     }
//                 }
//                 &Instruction::Call(arg_count) => {
//                     let value = self.stack.peek(0);
//                     match value {
//                         Value::Func(func) => {
//                             let frame = CallFrame::new(func.clone(), self.stack.len() - 1);
//                             self.frames.push(frame);
//                         }
//                         Value::ForeignFunc(func) => {
//                             let func = func.clone();

//                             self.stack.pop(); // this pops the actual foreign function

//                             let mut values = (0..arg_count)
//                                 .into_iter()
//                                 .map(|_| self.stack.pop())
//                                 .collect::<Vec<Value>>();
//                             values.reverse();

//                             // TODO: push actual value by the return value of the func
//                             let result = self.ffi.call(func, values).unwrap();
//                             self.stack.push(Value::Int(result as i64));
//                         }
//                         _ => panic!("tried to call an uncallable value `{}`", value),
//                     }
//                 }
//                 &Instruction::GetGlobal(name) => {
//                     match self.globals.get(&name) {
//                         Some(value) => self.stack.push(value.clone()),
//                         None => panic!("undefined global `{}`", name),
//                     };
//                 }
//                 &Instruction::SetGlobal(name) => {
//                     self.globals.insert(name, self.stack.pop());
//                 }
//                 &Instruction::GetLocal(slot) => {
//                     let slot = self.frames.peek(0).slot as isize + slot;
//                     let value = self.stack.get(slot as usize).clone();
//                     self.stack.push(value);
//                 }
//                 &Instruction::SetLocal(slot) => {
//                     let slot = self.frames.peek(0).slot as isize + slot;
//                     let value = self.stack.peek(0).clone();
//                     self.stack.set(slot as usize, value);
//                 }
//             }
//         }
//     }

//     fn current_code(&self) -> &Bytecode {
//         &self.current_func().code
//     }

//     fn current_func(&self) -> &Function {
//         &self.frames.peek(0).function
//     }

//     fn get_const(&self, addr: usize) -> &Value {
//         self.constants.get(addr).unwrap()
//     }

//     fn jmp(&mut self, offset: isize) {
//         let new_ip = self.frames.peek_mut().ip as isize + offset;
//         self.frames.peek_mut().ip = new_ip as usize;
//     }

//     fn trace(&self, ip: &usize, inst: &Instruction) {
//         let stack_trace = self.stack.trace();

//         println!(
//             "{:06}\t{}\n\t{}",
//             ip,
//             inst.to_string().bold(),
//             stack_trace.blue()
//         );
//     }
// }

// pub fn dump_bytecode_to_file(globals: &Globals, constants: &Constants, code: &Bytecode) {
//     if let Ok(file) = &OpenOptions::new()
//         .read(false)
//         .write(true)
//         .create(true)
//         .truncate(true)
//         .append(false)
//         .open(Path::new("vm.out"))
//     {
//         let mut writer = BufWriter::new(file);

//         for (index, inst) in code.iter().enumerate() {
//             writer
//                 .write(format!("{:06}\t{}\n", index, inst).as_bytes())
//                 .unwrap();
//         }

//         writer.write("\nglobals:\n".as_bytes()).unwrap();

//         for (name, value) in globals.iter() {
//             writer
//                 .write(
//                     format!(
//                         "${} = {}\n",
//                         name,
//                         match value {
//                             Value::()
//                             | Value::Int(_)
//                             | Value::Bool(_)
//                             | Value::Str(_)
//                             | Value::ForeignFunc(_) => value.to_string(),
//                             Value::Func(func) => format!(
//                                 "func:\n{}",
//                                 func.code
//                                     .iter()
//                                     .enumerate()
//                                     .map(|(index, inst)| format!("{:06}\t{}", index, inst))
//                                     .collect::<Vec<String>>()
//                                     .join("\n")
//                             ),
//                         },
//                     )
//                     .as_bytes(),
//                 )
//                 .unwrap();
//         }

//         writer.write("\nconstants:\n".as_bytes()).unwrap();

//         for (index, constant) in constants.iter().enumerate() {
//             writer
//                 .write(format!("%{}\t{}\n", index, constant,).as_bytes())
//                 .unwrap();
//         }
//     }
// }
