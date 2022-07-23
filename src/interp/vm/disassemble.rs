use crate::interp::interp::Interp;

use super::{
    bytecode::{Bytecode, BytecodeReader, Op},
    value::{FunctionValue, Value},
};
use std::{
    fs::OpenOptions,
    io::{BufWriter, Write},
    path::Path,
};

pub fn dump_bytecode_to_file(interp: &Interp, code: &Bytecode) {
    if let Ok(file) = &OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .truncate(true)
        .append(false)
        .open(Path::new("vm.out"))
    {
        let mut w = BufWriter::new(file);

        code.reader().disassemble(&mut w, interp);

        write!(&mut w, "\nglobals:\n").unwrap();

        for (slot, value) in interp.globals.iter().enumerate() {
            write!(&mut w, "${} = ", slot).unwrap();
            value.disassemble(&mut w, interp);
            write!(&mut w, "\n").unwrap();
        }

        write!(&mut w, "\nfunctions:\n").unwrap();

        let last_function_index = interp.functions.len() - 1;
        for (index, (_, function)) in interp.functions.iter().enumerate() {
            write!(
                &mut w,
                "fn {}\n",
                if function.name.is_empty() {
                    "<anon>"
                } else {
                    &function.name
                }
            )
            .unwrap();

            function.code.reader().disassemble(&mut w, interp);

            if index < last_function_index {
                write!(&mut w, "\n").unwrap();
            }
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
                FunctionValue::Orphan(f) => write!(w, "fn {}", f.name).unwrap(),
                FunctionValue::Extern(f) => write!(w, "extern fn {}", f.name).unwrap(),
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

pub(super) fn bytecode_reader_write_single_inst<'a, W: Write>(
    reader: &mut BytecodeReader<'a>,
    w: &mut W,
) {
    if let Some(op) = reader.try_read_op() {
        write!(w, "{:06}\t{}", reader.cursor(), op).unwrap();

        match op {
            Op::LoadConst => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::Jmp => write!(w, " {}", reader.read_i32()).unwrap(),
            Op::Jmpf => write!(w, " {}", reader.read_i32()).unwrap(),
            Op::Call => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::LoadGlobal => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::LoadGlobalPtr => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::StoreGlobal => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::Peek => write!(w, " {}", reader.read_i32()).unwrap(),
            Op::PeekPtr => write!(w, " {}", reader.read_i32()).unwrap(),
            Op::StoreLocal => write!(w, " {}", reader.read_i32()).unwrap(),
            Op::ConstIndex => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::ConstIndexPtr => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::BufferAlloc => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::BufferPut => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::BufferFill => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::Copy => write!(w, " {}", reader.read_u32()).unwrap(),
            Op::Swap => write!(w, " {}", reader.read_u32()).unwrap(),
            _ => (),
        }
    }
}
