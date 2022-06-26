use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::Path;

use itertools::Itertools;

use crate::ast::{ty::Type, workspace::Workspace};
use crate::hir;
use crate::infer::normalize::Normalize;
use crate::infer::{display::DisplayTy, ty_ctx::TyCtx};

use super::const_value::ConstValue;

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
        let mut printer = Printer::new(workspace, tycx, file);
        cache.print(&mut printer);
    }
}

struct Printer<'a, W: Write> {
    workspace: &'a Workspace,
    tycx: &'a TyCtx,
    writer: W,
    identation: u16,
}

impl<'a, W: Write> Printer<'a, W> {
    fn new(workspace: &'a Workspace, tycx: &'a TyCtx, writer: W) -> Self {
        Self {
            workspace,
            tycx,
            writer,
            identation: 0,
        }
    }

    fn indent(&mut self) {
        self.identation += INDENT;
    }

    fn dedent(&mut self) {
        self.identation -= INDENT;
    }

    fn write(&mut self, s: &str) {
        self.writer.write_all(s.as_bytes()).unwrap();
    }

    fn write_indented(&mut self, s: &str) {
        if self.identation > 0 {
            self.write(&(0..=self.identation).map(|_| " ").collect::<String>());
        }
        self.write(s);
    }

    fn write_comment(&mut self, s: &str) {
        self.write_indented(&format!("// {}", s))
    }
}

trait Print<'a, W: Write> {
    fn print(&self, p: &mut Printer<'a, W>);
}

impl<'a, W: Write, T: Print<'a, W>> Print<'a, W> for Vec<T> {
    fn print(&self, p: &mut Printer<'a, W>) {
        self.as_slice().print(p);
    }
}

impl<'a, W: Write, T: Print<'a, W>> Print<'a, W> for &[T] {
    fn print(&self, p: &mut Printer<'a, W>) {
        self.iter().for_each(|element| {
            element.print(p);
        });
    }
}

impl<'a, W: Write, T: Print<'a, W>> Print<'a, W> for Option<T> {
    fn print(&self, p: &mut Printer<'a, W>) {
        if let Some(e) = self {
            e.print(p);
        }
    }
}

impl<'a, W: Write, T: Print<'a, W>> Print<'a, W> for Box<T> {
    fn print(&self, p: &mut Printer<'a, W>) {
        self.as_ref().print(p);
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Cache {
    fn print(&self, p: &mut Printer<'a, W>) {
        enum Item<'a> {
            Binding(&'a hir::Binding),
            Function(&'a hir::Function),
        }

        self.bindings
            .iter()
            .map(|(_, b)| Item::Binding(b))
            .chain(self.functions.iter().map(|(_, f)| Item::Function(f)))
            .group_by(|item| match item {
                Item::Binding(x) => x.module_id,
                Item::Function(x) => x.module_id,
            })
            .into_iter()
            .for_each(|(module_id, items)| {
                let module_info = p.workspace.module_infos.get(module_id).unwrap();

                p.write_comment(&format!(
                    "{} ({})\n\n",
                    module_info.name, module_info.file_path
                ));

                for item in items {
                    match item {
                        Item::Binding(binding) => binding.print(p),
                        Item::Function(function) => function.print(p),
                    }

                    p.write(";\n\n");
                }
            });

        // for (_, binding) in self.bindings.iter() {
        //     binding.print(p);
        // }

        // for (_, function) in self.functions.iter() {
        //     function.print(p);
        // }
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Node {
    fn print(&self, p: &mut Printer<'a, W>) {
        match self {
            hir::Node::Const(x) => x.print(p),
            hir::Node::Binding(x) => x.print(p),
            hir::Node::Id(x) => x.print(p),
            hir::Node::Assignment(x) => x.print(p),
            hir::Node::MemberAccess(x) => x.print(p),
            hir::Node::Call(x) => x.print(p),
            hir::Node::Cast(x) => x.print(p),
            hir::Node::Sequence(x) => x.print(p),
            hir::Node::Control(x) => x.print(p),
            hir::Node::Builtin(x) => x.print(p),
            hir::Node::Literal(x) => x.print(p),
        }
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Binding {
    fn print(&self, p: &mut Printer<'a, W>) {
        p.write_indented("let ");
        p.write(&self.name);
        // p.write(": ");
        // p.write(
        //     &p.workspace
        //         .binding_infos
        //         .get(self.id)
        //         .unwrap()
        //         .ty
        //         .display(p.tycx),
        // );
        p.write(" = ");
        self.value.print(p);
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Function {
    fn print(&self, p: &mut Printer<'a, W>) {
        match &self.kind {
            hir::FunctionKind::Orphan { .. } => p.write_indented("fn "),
            hir::FunctionKind::Extern { lib } => {
                if let Some(lib) = lib {
                    p.write_indented(&format!("extern fn \"{}\" ", lib.path()))
                } else {
                    p.write_indented("extern fn ")
                }
            }
            hir::FunctionKind::Intrinsic(_) => p.write_indented("intrinsic fn "),
        }

        p.write(&self.name);

        let function_type = self.ty.normalize(p.tycx).into_function();

        p.write("(");
        for param in function_type.params.iter() {
            p.write(&param.display(p.tycx));
        }
        p.write(") -> ");
        p.write(&function_type.return_type.display(p.tycx));
        p.write(" ");

        match &self.kind {
            hir::FunctionKind::Orphan { body } => body.as_ref().unwrap().print(p),
            hir::FunctionKind::Extern { .. } | hir::FunctionKind::Intrinsic(..) => (),
        }
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Const {
    fn print(&self, p: &mut Printer<'a, W>) {
        p.write(&self.value.display(p.tycx));
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Id {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Assignment {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::MemberAccess {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Call {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Cast {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Sequence {
    fn print(&self, p: &mut Printer<'a, W>) {
        if self.is_block {
            p.write_indented("{\n");
            p.indent();
        }

        for (index, statement) in self.statements.iter().enumerate() {
            // statement.print(p);
            p.write_indented("statement");
            if index < self.statements.len() - 1 {
                p.write(";\n");
            } else {
                p.write("\n");
            }
        }

        if self.is_block {
            p.dedent();
            p.write_indented("}");
        }
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Control {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Builtin {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}

impl<'a, W: Write> Print<'a, W> for hir::Literal {
    fn print(&self, p: &mut Printer<'a, W>) {
        todo!();
    }
}
