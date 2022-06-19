use crate::interp::interp::Interp;

use super::{instruction::CompiledCode, value::Value};
use std::{
    fs::OpenOptions,
    io::{BufWriter, Write},
    path::Path,
};

pub trait PrettyPrint {
    fn pretty_print(&self, interp: &Interp) -> String;
}

pub fn dump_bytecode_to_file(interp: &Interp, code: &CompiledCode) {
    if let Ok(file) = &OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .truncate(true)
        .append(false)
        .open(Path::new("vm.out"))
    {
        let mut writer = BufWriter::new(file);

        for (index, inst) in code.instructions.iter().enumerate() {
            writer
                .write_all(format!("{:06}\t{}\n", index, inst).as_bytes())
                .unwrap();
        }

        writer.write_all("\nglobals:\n".as_bytes()).unwrap();

        for (slot, value) in interp.globals.iter().enumerate() {
            writer
                .write_all(format!("${} = {}\n", slot, value.pretty_print(interp)).as_bytes())
                .unwrap();
        }

        writer.write_all("\nconstants:\n".as_bytes()).unwrap();

        for (index, constant) in interp.constants.iter().enumerate() {
            writer
                .write_all(format!("%{}\t{}\n", index, constant.pretty_print(interp)).as_bytes())
                .unwrap();
        }
    }
}

impl PrettyPrint for Value {
    fn pretty_print(&self, interp: &Interp) -> String {
        match self {
            Value::Function(func) => {
                format!(
                    "fn {}\n{}",
                    if func.name.is_empty() {
                        "<anon>"
                    } else {
                        &func.name
                    },
                    interp.functions[&func.id]
                        .code
                        .instructions
                        .iter()
                        .enumerate()
                        .map(|(index, inst)| format!("{:06}\t{}", index, inst))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
            }
            _ => self.to_string(),
        }
    }
}
