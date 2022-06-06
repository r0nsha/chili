use chili_ast::{ast, pattern::Pattern, ty::TyKind, workspace::Workspace};
use chili_infer::{display::DisplayTy, ty_ctx::TyCtx};
use ptree::{
    print_config::UTF_CHARS_BOLD, print_tree_with, Color, PrintConfig, Style, TreeBuilder,
};

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

pub fn print_ast(ast: &ast::Ast, workspace: &Workspace, tycx: &TyCtx) {
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
        ast.module_info.name, ast.module_info.file_path
    ));

    ast.print_tree(&mut b, workspace, tycx);

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
        for binding in self.bindings.iter() {
            binding.print_tree(b, workspace, tycx);
        }
    }
}

impl PrintTree for ast::Ast {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        self.bindings.print_tree(b, workspace, tycx);
    }
}

impl PrintTree for ast::Binding {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        let module_info = workspace.get_module_info(self.module_id).unwrap();
        let module_prefix = if module_info.name.is_empty() {
            "".to_string()
        } else {
            format!("{}: ", module_info.name)
        };

        b.begin_child(match &self.pattern {
            Pattern::Symbol(pat) => {
                let ty = &workspace.get_binding_info(pat.id).unwrap().ty;
                format!(
                    "{}let {} <{}>",
                    module_prefix,
                    pat.symbol,
                    tycx.ty_kind(*ty)
                )
            }
            Pattern::StructUnpack(pat) => {
                let concat_symbols = pat
                    .symbols
                    .iter()
                    .map(|pat| {
                        let ty = &workspace.get_binding_info(pat.id).unwrap().ty;
                        format!(
                            "{}let {} <{}>",
                            module_prefix,
                            pat.symbol,
                            tycx.ty_kind(*ty)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{{ {} }}", concat_symbols)
            }
            Pattern::TupleUnpack(pat) => {
                let concat_symbols = pat
                    .symbols
                    .iter()
                    .map(|pat| {
                        let ty = &workspace.get_binding_info(pat.id).unwrap().ty;
                        format!(
                            "{}let {} <{}>",
                            module_prefix,
                            pat.symbol,
                            tycx.ty_kind(*ty)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("({})", concat_symbols)
            }
        });

        if let Some(value) = &self.expr {
            value.print_tree(b, workspace, tycx);
        } else {
            b.add_empty_child("[uninit]".to_string());
        }

        b.end_child();
    }
}

impl PrintTree for ast::Function {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        b.begin_child("fn".to_string());
        self.sig.print_tree(b, workspace, tycx);
        self.body.print_tree(b, workspace, tycx);
        b.end_child();
    }
}

impl PrintTree for ast::Block {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        let ty = match self.exprs.last() {
            Some(e) => tycx.ty_kind(e.ty),
            None => TyKind::Unit,
        };
        b.begin_child(format!("block <{}>", ty));
        self.exprs.print_tree(b, workspace, tycx);
        build_deferred(b, &self.deferred, workspace, tycx);
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

fn build_deferred(
    b: &mut TreeBuilder,
    deferred: &[ast::Expr],
    workspace: &Workspace,
    tycx: &TyCtx,
) {
    if deferred.is_empty() {
        return;
    }
    b.begin_child("deferred".to_string());
    deferred.print_tree(b, workspace, tycx);
    b.end_child();
}

impl PrintTree for ast::Expr {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        match &self.kind {
            ast::ExprKind::Extern(bindings) => bindings.print_tree(b, workspace, tycx),
            ast::ExprKind::Binding(binding) => binding.print_tree(b, workspace, tycx),
            ast::ExprKind::Defer(defer) => {
                b.begin_child("defer".to_string());
                defer.expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Assign(assign) => {
                b.begin_child("assign".to_string());
                assign.lvalue.print_tree(b, workspace, tycx);
                assign.rvalue.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Cast(cast) => {
                b.begin_child("cast".to_string());
                cast.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Builtin(builtin) => {
                match builtin {
                    ast::BuiltinKind::Import(path) => {
                        b.add_empty_child(format!("@import(\"{}\")", path.to_str().unwrap()));
                    }
                    ast::BuiltinKind::LangItem(item) => {
                        b.add_empty_child(format!("@lang_item(\"{}\")", item));
                    }
                    ast::BuiltinKind::SizeOf(expr) => {
                        b.begin_child("@size_of".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::BuiltinKind::AlignOf(expr) => {
                        b.begin_child("@align_of".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::BuiltinKind::Run(expr, result) => {
                        b.begin_child(format!(
                            "@run (resulted in: {:?})",
                            result.as_ref().unwrap()
                        ));
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::BuiltinKind::Panic(expr) => {
                        b.begin_child("@panic".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                };
            }
            ast::ExprKind::Function(closure) => {
                b.begin_child(closure.sig.to_string());
                closure.body.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::While(while_) => {
                b.begin_child("while".to_string());
                while_.cond.print_tree(b, workspace, tycx);
                while_.block.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::For(for_) => {
                b.begin_child(format!(
                    "for ({}, {})",
                    for_.iter_name, for_.iter_index_name
                ));

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
            ast::ExprKind::Break(term) => {
                build_deferred(b, &term.deferred, workspace, tycx);
                b.add_empty_child("break".to_string());
            }
            ast::ExprKind::Continue(term) => {
                build_deferred(b, &term.deferred, workspace, tycx);
                b.add_empty_child("continue".to_string());
            }
            ast::ExprKind::Return(ret) => {
                b.begin_child("return".to_string());
                build_deferred(b, &ret.deferred, workspace, tycx);
                ret.expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::If(if_) => {
                b.begin_child(format!("if <{}>", tycx.ty_kind(self.ty)));
                if_.cond.print_tree(b, workspace, tycx);
                if_.then.print_tree(b, workspace, tycx);

                if let Some(otherwise) = &if_.otherwise {
                    b.begin_child("else".to_string());
                    otherwise.print_tree(b, workspace, tycx);
                    b.end_child();
                }

                b.end_child();
            }
            ast::ExprKind::Block(block) => {
                block.print_tree(b, workspace, tycx);
            }
            ast::ExprKind::Binary(binary) => {
                b.begin_child(format!("{} <{}>", binary.op, tycx.ty_kind(self.ty)));
                binary.lhs.print_tree(b, workspace, tycx);
                binary.rhs.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Unary(unary) => {
                b.begin_child(format!("{} <{}>", unary.op, tycx.ty_kind(self.ty)));
                unary.lhs.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Subscript(sub) => {
                b.begin_child(format!("subscript <{}>", tycx.ty_kind(self.ty)));
                sub.expr.print_tree(b, workspace, tycx);
                sub.index.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Slice(slice) => {
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
            ast::ExprKind::Call(call) => {
                b.begin_child(format!("call <{}>", tycx.ty_kind(self.ty)));

                b.begin_child(format!("callee <{}>", tycx.ty_kind(call.callee.ty)));
                call.callee.print_tree(b, workspace, tycx);
                b.end_child();

                if !call.args.is_empty() {
                    b.begin_child("args".to_string());
                    call.args.print_tree(b, workspace, tycx);
                    b.end_child();
                }

                b.end_child();
            }
            ast::ExprKind::MemberAccess(access) => {
                b.begin_child(format!(
                    "access `{}` <{}>",
                    access.member,
                    tycx.ty_kind(self.ty)
                ));
                access.expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Ident(ident) => {
                b.add_empty_child(format!("`{}` <{}>", ident.symbol, tycx.ty_kind(self.ty)));
            }
            ast::ExprKind::ArrayLiteral(lit) => {
                b.begin_child(format!("array literal <{}>", tycx.ty_kind(self.ty)));

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
            ast::ExprKind::TupleLiteral(lit) => {
                b.begin_child(format!("tuple literal <{}>", tycx.ty_kind(self.ty)));
                lit.elements.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::StructLiteral(lit) => {
                b.begin_child(format!("struct literal <{}>", tycx.ty_kind(self.ty)));
                lit.type_expr.print_tree(b, workspace, tycx);
                for f in &lit.fields {
                    b.begin_child(f.symbol.to_string());
                    f.expr.print_tree(b, workspace, tycx);
                    b.end_child();
                }
                b.end_child();
            }
            ast::ExprKind::Literal(lit) => {
                b.add_empty_child(format!("{} <{}>", lit.kind, tycx.ty_kind(self.ty)));
            }
            ast::ExprKind::PointerType(ast::ExprAndMut { inner, is_mutable }) => {
                b.begin_child(format!(
                    "{}pointer type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    tycx.ty_kind(self.ty)
                ));
                inner.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::MultiPointerType(ast::ExprAndMut { inner, is_mutable }) => {
                b.begin_child(format!(
                    "{}multi-pointer type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    tycx.ty_kind(self.ty)
                ));
                inner.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::ArrayType(at) => {
                b.begin_child(format!("array type <{}>", tycx.ty_kind(self.ty)));

                b.begin_child("type".to_string());
                at.inner.print_tree(b, workspace, tycx);
                b.end_child();

                b.begin_child("size".to_string());
                at.size.print_tree(b, workspace, tycx);
                b.end_child();

                b.end_child();
            }
            ast::ExprKind::SliceType(ast::ExprAndMut { inner, is_mutable }) => {
                b.begin_child(format!(
                    "{}slice type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    tycx.ty_kind(self.ty)
                ));
                inner.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::StructType(ty) => {
                b.begin_child("struct type".to_string());
                for field in &ty.fields {
                    b.begin_child(format!("field {}", field.name));
                    field.ty.print_tree(b, workspace, tycx);
                    b.end_child();
                }
                b.end_child();
            }
            ast::ExprKind::FunctionType(sig) => {
                b.begin_child("fn type".to_string());
                sig.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::SelfType => {
                b.add_empty_child(format!("Self <{}>", tycx.ty_kind(self.ty)));
            }
            ast::ExprKind::Placeholder => {
                b.add_empty_child(format!("_ (type hole) <{}>", tycx.ty_kind(self.ty)));
            }
            ast::ExprKind::ConstValue(const_value) => {
                b.add_empty_child(format!(
                    "const value: {} <{}>",
                    const_value.to_string(),
                    tycx.ty_kind(self.ty)
                ));
            }
            ast::ExprKind::Error => {
                b.add_empty_child("ERROR".to_string());
            }
        }
    }
}

impl PrintTree for ast::Cast {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyCtx) {
        b.begin_child("from".to_string());
        self.expr.print_tree(b, workspace, tycx);
        b.end_child();

        if let Some(type_expr) = &self.ty_expr {
            b.begin_child("to".to_string());
            type_expr.print_tree(b, workspace, tycx);
            b.end_child();
        } else {
            b.add_empty_child(format!("autocast -> {}", self.target_ty.display(tycx)));
        }
    }
}
