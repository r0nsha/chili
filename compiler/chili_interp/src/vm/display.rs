use crate::vm::{instruction::CompiledCode, value::Value, Constants, Globals};
use std::{
    fs::OpenOptions,
    io::{BufWriter, Write},
    path::Path,
};

pub(crate) trait PrettyPrint {
    fn pretty_print(&self) -> String;
}

pub fn dump_bytecode_to_file(globals: &Globals, constants: &Constants, code: &CompiledCode) {
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

        for (slot, value) in globals.iter().enumerate() {
            writer
                .write_all(format!("${} = {}\n", slot, value.pretty_print()).as_bytes())
                .unwrap();
        }

        writer.write_all("\nconstants:\n".as_bytes()).unwrap();

        for (index, constant) in constants.iter().enumerate() {
            writer
                .write_all(format!("%{}\t{}\n", index, constant.pretty_print()).as_bytes())
                .unwrap();
        }
    }
}

impl PrettyPrint for Value {
    fn pretty_print(&self) -> String {
        match self {
            Value::Func(func) => format!(
                "fn {}\n{}",
                if func.name.is_empty() {
                    "<anon>"
                } else {
                    &func.name
                },
                func.code
                    .instructions
                    .iter()
                    .enumerate()
                    .map(|(index, inst)| format!("{:06}\t{}", index, inst))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            _ => self.to_string(),
        }
    }
}
