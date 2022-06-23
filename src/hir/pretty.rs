use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::Path;

use crate::ast::{ty::Type, workspace::Workspace};
use crate::hir;
use crate::infer::normalize::Normalize;
use crate::infer::{display::DisplayTy, ty_ctx::TyCtx};

const INDENT: u16 = 2;

#[allow(unused)]
pub fn print(cache: &hir::Cache, workspace: &Workspace, tycx: &TyCtx) {
    if let Ok(file) = &OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .truncate(true)
        .append(false)
        .open(Path::new("hir.pretty.chili"))
    {
        let mut printer = Printer::new(workspace, tycx);
        cache.print(&mut printer);
    }
}

struct Printer<'a> {
    workspace: &'a Workspace,
    tycx: &'a TyCtx,
    buf: BufWriter<Vec<u8>>,
    identation: u16,
}

impl<'a> Printer<'a> {
    fn new(workspace: &'a Workspace, tycx: &'a TyCtx) -> Self {
        Self {
            workspace,
            tycx,
            buf: BufWriter::new(Vec::new()),
            identation: 0,
        }
    }

    fn write(&mut self, s: &str) {
        self.buf.write_all(s.as_bytes()).unwrap();
    }

    fn indent(&mut self) {
        self.identation += INDENT;
    }

    fn dedent(&mut self) {
        self.identation -= INDENT;
    }
}

trait Print {
    fn print(&self, p: &mut Printer);
}

impl<T: Print> Print for Vec<T> {
    fn print(&self, p: &mut Printer) {
        self.as_slice().print(p);
    }
}

impl<T: Print> Print for &[T] {
    fn print(&self, p: &mut Printer) {
        self.iter().for_each(|element| {
            element.print(p);
        });
    }
}

impl<T: Print> Print for Option<T> {
    fn print(&self, p: &mut Printer) {
        if let Some(e) = self {
            e.print(p);
        }
    }
}

impl<T: Print> Print for Box<T> {
    fn print(&self, p: &mut Printer) {
        self.as_ref().print(p);
    }
}

impl Print for hir::Cache {
    fn print(&self, p: &mut Printer) {
        // for (_, binding) in self.bindings.iter() {
        //     binding.print(p);
        // }

        // for (_, function) in self.functions.iter() {
        //     function.print(p);
        // }
    }
}

impl Print for hir::Node {
    fn print(&self, p: &mut Printer) {
        match self {
            hir::Node::Const(_) => todo!(),
            hir::Node::Binding(_) => todo!(),
            hir::Node::Id(_) => todo!(),
            hir::Node::Assignment(_) => todo!(),
            hir::Node::MemberAccess(_) => todo!(),
            hir::Node::Call(_) => todo!(),
            hir::Node::Sequence(_) => todo!(),
            hir::Node::Control(_) => todo!(),
            hir::Node::Builtin(_) => todo!(),
            hir::Node::Literal(_) => todo!(),
        }
    }
}
