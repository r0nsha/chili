use crate::interp::interp::Interp;

use super::{
    bytecode::{BytecodeReader, Op},
    inst::CompiledCode,
    value::{FunctionValue, Value},
};
use std::{
    fs::OpenOptions,
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

pub trait Disassemble<W: Write> {
    fn disassemble(&self, w: &mut BufWriter<W>, interp: &Interp);
}

impl<W: Write> Disassemble<W> for Value {
    fn disassemble(&self, w: &mut BufWriter<W>, interp: &Interp) {
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

impl<'a, W: Write> Disassemble<W> for BytecodeReader<'a> {
    fn disassemble(&self, w: &mut BufWriter<W>, _: &Interp) {
        let mut reader = self.clone();

        while reader.has_remaining() {
            bytecode_reader_write_single_inst(&mut reader, w);
            write!(w, "\n").unwrap();
        }
    }
}

fn bytecode_reader_write_single_inst<'a, W: Write>(reader: &mut BytecodeReader<'a>, w: &mut W) {
    if let Some(op) = reader.try_read_op() {
        write!(w, "{:06}\t{}", reader.cursor(), op).unwrap();

        match op {
            Op::LoadConst => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::Jmp => write!(w, "\t{}", reader.read_i32()).unwrap(),
            Op::Jmpf => write!(w, "\t{}", reader.read_i32()).unwrap(),
            Op::Call => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::LoadGlobal => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::LoadGlobalPtr => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::StoreGlobal => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::Peek => write!(w, "\t{}", reader.read_i32()).unwrap(),
            Op::PeekPtr => write!(w, "\t{}", reader.read_i32()).unwrap(),
            Op::StoreLocal => write!(w, "\t{}", reader.read_i32()).unwrap(),
            Op::ConstIndex => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::ConstIndexPtr => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::BufferAlloc => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::BufferPut => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::BufferFill => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::Copy => write!(w, "\t{}", reader.read_u32()).unwrap(),
            Op::Swap => write!(w, "\t{}", reader.read_u32()).unwrap(),
            _ => (),
        }
    }
}
