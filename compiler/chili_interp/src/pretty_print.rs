use crate::{
    instruction::Bytecode,
    value::Value,
    vm::{Constants, Globals},
};
use std::{
    fs::OpenOptions,
    io::{BufWriter, Write},
    path::Path,
};

pub(crate) trait PrettyPrint {
    fn pretty_print(&self) -> String;
}

pub fn dump_bytecode_to_file(globals: &Globals, constants: &Constants, code: &Bytecode) {
    if let Ok(file) = &OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .truncate(true)
        .append(false)
        .open(Path::new("vm.out"))
    {
        let mut writer = BufWriter::new(file);

        for (index, inst) in code.iter().enumerate() {
            writer
                .write(format!("{:06}\t{}\n", index, inst).as_bytes())
                .unwrap();
        }

        writer.write("\nglobals:\n".as_bytes()).unwrap();

        for (slot, value) in globals.iter().enumerate() {
            writer
                .write(format!("${} = {}\n", slot, value.pretty_print(),).as_bytes())
                .unwrap();
        }

        writer.write("\nconstants:\n".as_bytes()).unwrap();

        for (index, constant) in constants.iter().enumerate() {
            writer
                .write(format!("%{}\t{}\n", index, constant,).as_bytes())
                .unwrap();
        }
    }
}

impl PrettyPrint for Value {
    fn pretty_print(&self) -> String {
        match self {
            Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::Tuple(_) => self.to_string(),
            Value::Func(func) => format!(
                "fn:{}\n{}",
                if func.name.is_empty() {
                    "".to_string()
                } else {
                    format!(" {}", func.name)
                },
                func.code
                    .iter()
                    .enumerate()
                    .map(|(index, inst)| format!("{:06}\t{}", index, inst))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        }
    }
}
