use crate::interp::interp::Interp;

use super::{
    inst::CompiledCode,
    value::{FunctionValue, Value},
};
use std::{
    fs::{File, OpenOptions},
    io::{BufWriter, Write},
    path::Path,
};

pub fn dump_bytecode_to_file(interp: &Interp, code: &CompiledCode) {
    if let Ok(file) = &OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .truncate(true)
        .append(false)
        .open(Path::new("vm.out"))
    {
        let mut w = BufWriter::new(file);

        for (index, inst) in code.instructions.iter().enumerate() {
            write!(&mut w, "{:06}\t{}\n", index, inst).unwrap();
        }

        write!(&mut w, "\nglobals:\n").unwrap();

        for (slot, value) in interp.globals.iter().enumerate() {
            write!(&mut w, "${} = ", slot).unwrap();
            value.disassemble(&mut w, interp);
            write!(&mut w, "\n").unwrap();
        }

        write!(&mut w, "\nconstants:\n").unwrap();

        for (slot, constant) in interp.constants.iter().enumerate() {
            write!(&mut w, "%{}\t", slot).unwrap();
            constant.disassemble(&mut w, interp);
            write!(&mut w, "\n").unwrap();
        }
    }
}

pub trait Disassemble {
    fn disassemble(&self, w: &mut BufWriter<&File>, interp: &Interp);
}

impl Disassemble for Value {
    fn disassemble(&self, w: &mut BufWriter<&File>, interp: &Interp) {
        match self {
            Value::Function(addr) => match interp.get_function(addr.id).unwrap() {
                FunctionValue::Orphan(function) => {
                    write!(
                        w,
                        "fn {}\n",
                        if function.name.is_empty() {
                            "<anon>"
                        } else {
                            &function.name
                        }
                    )
                    .unwrap();

                    for (index, inst) in function.code.instructions.iter().enumerate() {
                        write!(w, "{:06}\t{}\n", index, inst).unwrap();
                    }
                }
                FunctionValue::Extern(_) => w.write_all(self.to_string().as_bytes()).unwrap(),
            },
            _ => {
                w.write_all(self.to_string().as_bytes()).unwrap();
            }
        }
    }
}
