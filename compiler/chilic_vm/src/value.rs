// use std::fmt::Display;

// use ustr::Ustr;

// use crate::{ffi::ForeignFunction, Bytecode};

// #[derive(Debug, Clone)]
// pub enum Value {
//     (),
//     Int(i64),
//     Bool(bool),
//     Str(String),
//     Func(Function),
//     ForeignFunc(ForeignFunction),
// }

// impl Value {
//     pub fn is_truthy(&self) -> bool {
//         match self {
//             Value::() => false,
//             Value::Bool(v) => *v,
//             _ => true,
//         }
//     }
// }

// impl Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 Value::() => "()".to_string(),
//                 Value::Int(v) => format!("int {}", v),
//                 Value::Bool(v) => format!("bool {}", v),
//                 Value::Str(v) => format!("str \"{}\"", v),
//                 Value::Func(func) => format!("func {}", func.name,),
//                 Value::ForeignFunc(func) => format!("foreign \"{}\" func {}", func.lib, func.name,),
//             }
//         )
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Function {
//     pub name: Ustr,
//     pub arg_count: usize,
//     pub code: Bytecode,
// }
