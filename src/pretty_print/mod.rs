use crate::ast::{self, ty::Type, workspace::Workspace};
use crate::infer::{display::DisplayTy, ty_ctx::TyCtx};
use ptree::{
    print_config::UTF_CHARS_BOLD, print_tree_with, Color, PrintConfig, Style, TreeBuilder,
};

#[allow(unused)]
pub fn print_typed_ast(typed_ast: &ast::TypedAst, workspace: &Workspace, tycx: &TyCtx) {
    let config = {
        let mut config = PrintConfig::from_env();
        config.branch = Style {
            foreground: Some(Color::Blue),
            dimmed: true,
            ..Style::default()
        };
        config.leaf = Style {
            bold: true,
            ..Style::default()
        };
        config.characters = UTF_CHARS_BOLD.into();
        config.indent = 3;
        config
    };

    let mut b = TreeBuilder::new("Program".to_string());

    typed_ast.print_tree(&mut b, workspace, tycx);

    print_tree_with(&b.build(), &config).expect("error printing ir tree");

    println!();
}

#[allow(unused)]
pub fn print_ast(module: &ast::Module, workspace: &Workspace, tycx: &TyCtx) {
    let config = {
        let mut config = PrintConfig::from_env();
        config.branch = Style {
            foreground: Some(Color::Blue),
            dimmed: true,
            ..Style::default()
        };
        config.leaf = Style {
            bold: true,
            ..Style::default()
        };
        config.characters = UTF_CHARS_BOLD.into();
        config.indent = 3;
        config
    };

    let mut b = TreeBuilder::new(format!(
        "'{}' - {}",
        module.module_info.name, module.module_info.file_path
    ));

    module.print_tree(&mut b, workspace, tycx);

    print_tree_with(&b.build(), &config).expect("error printing ir tree");

    println!();
}

pub trait PrintTree {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx);
}

impl<T: PrintTree> PrintTree for Vec<T> {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        for element in self {
            element.print_tree(b, workspace, tycx);
        }
    }
}

impl<T: PrintTree> PrintTree for &[T] {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        for element in self.iter() {
            element.print_tree(b, workspace, tycx);
        }
    }
}

impl<T: PrintTree> PrintTree for Option<T> {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        if let Some(e) = self {
            e.print_tree(b, workspace, tycx);
        }
    }
}

impl<T: PrintTree> PrintTree for Box<T> {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        self.as_ref().print_tree(b, workspace, tycx);
    }
}

impl PrintTree for ast::TypedAst {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        for (_, binding) in self.bindings.iter() {
            binding.print_tree(b, workspace, tycx);
        }
    }
}

impl PrintTree for ast::Module {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        self.bindings.print_tree(b, workspace, tycx);
    }
}

impl PrintTree for ast::Binding {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        let module_info = workspace.module_infos.get(self.module_id).unwrap();
        let module_prefix = if module_info.name.is_empty() {
            "".to_string()
        } else {
            format!("{}: ", module_info.name)
        };

        b.begin_child(format!(
            "{}let {} <{}>",
            module_prefix,
            self.pattern,
            tycx.ty_kind(self.ty)
        ));

        if let Some(value) = &self.expr {
            value.print_tree(b, workspace, tycx);
        } else {
            b.add_empty_child("[uninit]".to_string());
        }

        b.end_child();
    }
}

impl PrintTree for ast::FunctionExpr {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        b.begin_child("fn".to_string());
        self.sig.print_tree(b, workspace, tycx);
        self.body.print_tree(b, workspace, tycx);
        b.end_child();
    }
}

impl PrintTree for ast::Block {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        let ty = match self.statements.last() {
            Some(e) => tycx.ty_kind(e.ty()),
            None => Type::Unit,
        };
        b.begin_child(format!("block <{}>", ty));
        self.statements.print_tree(b, workspace, tycx);
        b.end_child();
    }
}

impl PrintTree for ast::FunctionSig {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        b.begin_child(format!(
            "{}sig",
            if self.kind.is_extern() { "extern " } else { "" }
        ));

        b.begin_child("parameters".to_string());

        for param in &self.params {
            b.begin_child(param.pattern.to_string());

            if let Some(ty) = &param.ty_expr {
                ty.print_tree(b, workspace, tycx);
            } else {
                b.add_empty_child("inferred".to_string());
            }

            b.end_child();
        }

        b.end_child();

        if let Some(ret) = &self.ret {
            b.begin_child("return".to_string());
            ret.print_tree(b, workspace, tycx);
            b.end_child();
        }

        b.end_child();
    }
}

impl PrintTree for ast::Ast {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        match self {
            ast::Ast::Binding(binding) => binding.print_tree(b, workspace, tycx),
            ast::Ast::Assignment(assignment) => {
                b.begin_child("assignment".to_string());
                assignment.lvalue.print_tree(b, workspace, tycx);
                assignment.rvalue.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::Cast(cast) => {
                b.begin_child("cast".to_string());
                cast.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::Builtin(builtin) => {
                match &builtin.kind {
                    ast::BuiltinKind::Import(path) => {
                        b.add_empty_child(format!("import!(\"{}\")", path.to_str().unwrap()));
                    }
                    ast::BuiltinKind::SizeOf(expr) => {
                        b.begin_child("size_of!".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::BuiltinKind::AlignOf(expr) => {
                        b.begin_child("align_of!".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::BuiltinKind::Run(expr, result) => {
                        b.begin_child(format!("run!(resulted in: {:?})", result.as_ref().unwrap()));
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::BuiltinKind::Panic(expr) => {
                        b.begin_child("panic!".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                };
            }
            ast::Ast::Function(closure) => {
                b.begin_child(closure.sig.to_string());
                closure.body.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::While(while_) => {
                b.begin_child("while".to_string());
                while_.condition.print_tree(b, workspace, tycx);
                while_.block.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::For(for_) => {
                if let Some(index_binding) = &for_.index_binding {
                    b.begin_child(format!(
                        "for {}, {}",
                        for_.iter_binding.name, index_binding.name
                    ));
                } else {
                    b.begin_child(format!("for {}", for_.iter_binding.name));
                }

                match &for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        b.begin_child("start".to_string());
                        start.print_tree(b, workspace, tycx);
                        b.end_child();

                        b.begin_child("end".to_string());
                        end.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::ForIter::Value(value) => {
                        b.begin_child("iter".to_string());
                        value.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                }

                for_.block.print_tree(b, workspace, tycx);

                b.end_child();
            }
            ast::Ast::Break(_) => {
                b.add_empty_child("break".to_string());
            }
            ast::Ast::Continue(_) => {
                b.add_empty_child("continue".to_string());
            }
            ast::Ast::Return(ret) => {
                b.begin_child("return".to_string());
                ret.expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::If(if_) => {
                b.begin_child(format!("if <{}>", tycx.ty_kind(if_.ty)));
                if_.condition.print_tree(b, workspace, tycx);
                if_.then.print_tree(b, workspace, tycx);

                if let Some(otherwise) = &if_.otherwise {
                    b.begin_child("else".to_string());
                    otherwise.print_tree(b, workspace, tycx);
                    b.end_child();
                }

                b.end_child();
            }
            ast::Ast::Block(block) => {
                block.print_tree(b, workspace, tycx);
            }
            ast::Ast::Binary(binary) => {
                b.begin_child(format!("{} <{}>", binary.op, tycx.ty_kind(binary.ty)));
                binary.lhs.print_tree(b, workspace, tycx);
                binary.rhs.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::Unary(unary) => {
                b.begin_child(format!("{} <{}>", unary.op, tycx.ty_kind(unary.ty)));
                unary.value.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::Subscript(sub) => {
                b.begin_child(format!("subscript <{}>", tycx.ty_kind(sub.ty)));
                sub.expr.print_tree(b, workspace, tycx);
                sub.index.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::Slice(slice) => {
                b.begin_child("slice".to_string());
                slice.expr.print_tree(b, workspace, tycx);

                if let Some(low) = &slice.low {
                    low.print_tree(b, workspace, tycx);
                } else {
                    b.add_empty_child("0".to_string());
                }

                if let Some(high) = &slice.high {
                    high.print_tree(b, workspace, tycx);
                } else {
                    b.add_empty_child("n".to_string());
                }

                b.end_child();
            }
            ast::Ast::Call(call) => {
                b.begin_child(format!("call <{}>", tycx.ty_kind(call.ty)));

                b.begin_child(format!("callee <{}>", tycx.ty_kind(call.callee.ty())));
                call.callee.print_tree(b, workspace, tycx);
                b.end_child();

                if !call.args.is_empty() {
                    b.begin_child("args".to_string());
                    call.args.print_tree(b, workspace, tycx);
                    b.end_child();
                }

                b.end_child();
            }
            ast::Ast::MemberAccess(access) => {
                b.begin_child(format!(
                    "access `{}` <{}>",
                    access.member,
                    tycx.ty_kind(access.ty)
                ));
                access.expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::Ident(ident) => {
                b.add_empty_child(format!("`{}` <{}>", ident.symbol, tycx.ty_kind(ident.ty)));
            }
            ast::Ast::ArrayLiteral(lit) => {
                b.begin_child(format!("array literal <{}>", tycx.ty_kind(lit.ty)));

                match &lit.kind {
                    ast::ArrayLiteralKind::List(elements) => {
                        b.begin_child("list".to_string());
                        elements.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::ArrayLiteralKind::Fill { expr, len } => {
                        b.begin_child("fill".to_string());
                        len.print_tree(b, workspace, tycx);
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                }

                b.end_child();
            }
            ast::Ast::TupleLiteral(lit) => {
                b.begin_child(format!("tuple literal <{}>", tycx.ty_kind(lit.ty)));
                lit.elements.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::StructLiteral(lit) => {
                b.begin_child(format!("struct literal <{}>", tycx.ty_kind(lit.ty)));
                lit.type_expr.print_tree(b, workspace, tycx);
                for f in &lit.fields {
                    b.begin_child(f.symbol.to_string());
                    f.expr.print_tree(b, workspace, tycx);
                    b.end_child();
                }
                b.end_child();
            }
            ast::Ast::Literal(lit) => {
                b.add_empty_child(format!("{} <{}>", lit.kind, tycx.ty_kind(lit.ty)));
            }
            ast::Ast::PointerType(em) => {
                b.begin_child(format!(
                    "{}pointer type <{}>",
                    if em.is_mutable { "mut " } else { "" },
                    tycx.ty_kind(em.ty)
                ));
                em.inner.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::MultiPointerType(em) => {
                b.begin_child(format!(
                    "{}multi-pointer type <{}>",
                    if em.is_mutable { "mut " } else { "" },
                    tycx.ty_kind(em.ty)
                ));
                em.inner.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::ArrayType(at) => {
                b.begin_child(format!("array type <{}>", tycx.ty_kind(at.ty)));

                b.begin_child("type".to_string());
                at.inner.print_tree(b, workspace, tycx);
                b.end_child();

                b.begin_child("size".to_string());
                at.size.print_tree(b, workspace, tycx);
                b.end_child();

                b.end_child();
            }
            ast::Ast::SliceType(em) => {
                b.begin_child(format!(
                    "{}slice type <{}>",
                    if em.is_mutable { "mut " } else { "" },
                    tycx.ty_kind(em.ty)
                ));
                em.inner.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::StructType(ty) => {
                b.begin_child("struct type".to_string());
                for field in &ty.fields {
                    b.begin_child(format!("field {}", field.name));
                    field.ty.print_tree(b, workspace, tycx);
                    b.end_child();
                }
                b.end_child();
            }
            ast::Ast::FunctionType(sig) => {
                b.begin_child("fn type".to_string());
                sig.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::Ast::SelfType(x) => {
                b.add_empty_child(format!("Self <{}>", tycx.ty_kind(x.ty)));
            }
            ast::Ast::Placeholder(x) => {
                b.add_empty_child(format!("_ (type hole) <{}>", tycx.ty_kind(x.ty)));
            }
            ast::Ast::Const(const_) => {
                b.add_empty_child(format!(
                    "const value: {} <{}>",
                    const_.value.to_string(),
                    tycx.ty_kind(const_.ty)
                ));
            }
            ast::Ast::Error(_) => {
                b.add_empty_child("Error".to_string());
            }
        }
    }
}

impl PrintTree for ast::Cast {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        b.begin_child("from".to_string());
        self.expr.print_tree(b, workspace, tycx);
        b.end_child();

        if let Some(type_expr) = &self.target {
            b.begin_child("to".to_string());
            type_expr.print_tree(b, workspace, tycx);
            b.end_child();
        } else {
            b.add_empty_child(format!("autocast -> {}", self.ty.display(tycx)));
        }
    }
}
