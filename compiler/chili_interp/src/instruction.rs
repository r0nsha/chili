use std::fmt::Display;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Instruction {
    Noop,
    Pop,
    Const(usize),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Not,
    Eq,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    BAnd,
    BOr,
    // Jmp(isize),
    // Jmpt(isize),
    // Jmpf(isize),
    // Return,
    // Call(usize),
    // GetGlobal(Ustr),
    // SetGlobal(Ustr),
    // GetLocal(isize),
    // SetLocal(isize),
    Halt,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Instruction::Noop => "noop".to_string(),
                Instruction::Pop => "pop".to_string(),
                Instruction::Const(addr) => format!("const %{}", addr),
                Instruction::Add => "add".to_string(),
                Instruction::Sub => "sub".to_string(),
                Instruction::Mul => "mul".to_string(),
                Instruction::Div => "div".to_string(),
                Instruction::Mod => "mod".to_string(),
                Instruction::Neg => "neg".to_string(),
                Instruction::Not => "not".to_string(),
                Instruction::Eq => "eq".to_string(),
                Instruction::NEq => "neq".to_string(),
                Instruction::Lt => "lt".to_string(),
                Instruction::LtEq => "lteq".to_string(),
                Instruction::Gt => "gt".to_string(),
                Instruction::GtEq => "gteq".to_string(),
                Instruction::BAnd => "band".to_string(),
                Instruction::BOr => "bor".to_string(),
                // Instruction::Jmp(offset) => format!("jmp &{:06}", offset),
                // Instruction::Jmpt(offset) => format!("jmpt &{:06}", offset),
                // Instruction::Jmpf(offset) => format!("jmpf &{:06}", offset),
                // Instruction::Return => "return".to_string(),
                // Instruction::Call(arg_count) => format!("call ({})", arg_count),
                // Instruction::GetGlobal(name) => format!("get_global ${}", name),
                // Instruction::SetGlobal(name) => format!("set_global ${}", name),
                // Instruction::GetLocal(slot) => format!("get_local ${}", slot),
                // Instruction::SetLocal(slot) => format!("set_local ${}", slot),
                Instruction::Halt => "halt".to_string(),
            }
        )
    }
}
