use std::{
    fs::OpenOptions,
    io::{BufWriter, Write},
    path::Path,
};

pub mod ffi;
pub mod instruction;
pub mod interp;
mod lower;
mod stack;
pub mod value;
pub mod vm;

use value::Value;
use vm::{Bytecode, Constants, Globals};

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

        for (id, value) in globals.iter() {
            writer
                .write(
                    format!(
                        "${} = {}\n",
                        id.0,
                        match value {
                            Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::Tuple(_) =>
                                value.to_string(),
                            Value::Func(func) => format!(
                                "func:\n{}",
                                func.code
                                    .iter()
                                    .enumerate()
                                    .map(|(index, inst)| format!("{:06}\t{}", index, inst))
                                    .collect::<Vec<String>>()
                                    .join("\n")
                            ),
                        },
                    )
                    .as_bytes(),
                )
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
