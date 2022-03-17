use chili_ast::{ast, pattern::Pattern, workspace::Workspace};
use chili_infer_new::tycx::TyContext;
use ptree::{
    print_config::UTF_CHARS_BOLD, print_tree_with, Color, PrintConfig, Style, TreeBuilder,
};

pub fn print_ast(ast: &ast::Ast, workspace: &Workspace, tycx: &TyContext) {
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
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext);
}

impl<T: PrintTree> PrintTree for Vec<T> {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        for element in self {
            element.print_tree(b, workspace, tycx);
        }
    }
}

impl<T: PrintTree> PrintTree for Option<T> {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        if let Some(e) = self {
            e.print_tree(b, workspace, tycx);
        }
    }
}

impl<T: PrintTree> PrintTree for Box<T> {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        self.as_ref().print_tree(b, workspace, tycx);
    }
}

impl PrintTree for ast::Ast {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        for import in self.imports.iter() {
            b.add_empty_child(format!(
                "use {} as {}",
                import.module_info.name, import.alias
            ));
        }

        self.bindings.print_tree(b, workspace, tycx);
    }
}
impl PrintTree for ast::Fn {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        b.begin_child("fn".to_string());
        self.proto.print_tree(b, workspace, tycx);
        self.body.print_tree(b, workspace, tycx);
        b.end_child();
    }
}

impl PrintTree for ast::Block {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        b.begin_child(format!(
            "block{}",
            if self.yields { " (yields)" } else { "" }
        ));
        self.exprs.print_tree(b, workspace, tycx);
        build_deferred(b, &self.deferred, workspace, tycx);
        b.end_child();
    }
}

impl PrintTree for ast::Proto {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        b.begin_child(format!(
            "{}proto",
            if self.lib_name.is_some() {
                "foreign "
            } else {
                ""
            }
        ));

        b.begin_child("parameters".to_string());

        for param in &self.params {
            b.begin_child(param.pattern.to_string());

            if let Some(ty) = &param.ty {
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

impl PrintTree for ast::Binding {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        b.begin_child(match &self.pattern {
            Pattern::Single(pat) => {
                let ty = &workspace.get_binding_info(pat.binding_info_id).unwrap().ty;
                format!("let {} <{}>", pat.symbol, tycx.ty_kind(*ty))
            }
            Pattern::StructDestructor(pat) => {
                let concat_symbols = pat
                    .symbols
                    .iter()
                    .map(|pat| {
                        let ty = &workspace.get_binding_info(pat.binding_info_id).unwrap().ty;
                        format!("let {} <{}>", pat.symbol, tycx.ty_kind(*ty))
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{{ {} }}", concat_symbols)
            }
            Pattern::TupleDestructor(pat) => {
                let concat_symbols = pat
                    .symbols
                    .iter()
                    .map(|pat| {
                        let ty = &workspace.get_binding_info(pat.binding_info_id).unwrap().ty;
                        format!("let {} <{}>", pat.symbol, tycx.ty_kind(*ty))
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

fn build_deferred(
    b: &mut TreeBuilder,
    deferred: &Vec<ast::Expr>,
    workspace: &Workspace,
    tycx: &TyContext,
) {
    if deferred.is_empty() {
        return;
    }
    b.begin_child("deferred".to_string());
    deferred.print_tree(b, workspace, tycx);
    b.end_child();
}

impl PrintTree for ast::Expr {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        match &self.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports.iter() {
                    b.add_empty_child(format!(
                        "use \"{}\" = {}",
                        import.module_info.file_path, import.alias
                    ));
                }
            }
            ast::ExprKind::Foreign(bindings) => bindings.print_tree(b, workspace, tycx),
            ast::ExprKind::Binding(binding) => binding.print_tree(b, workspace, tycx),
            ast::ExprKind::Defer(expr) => {
                b.begin_child("defer".to_string());
                expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Assign { lvalue, rvalue } => {
                b.begin_child("assign".to_string());
                lvalue.print_tree(b, workspace, tycx);
                rvalue.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Cast(info) => {
                b.begin_child("as".to_string());
                info.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Builtin(builtin) => {
                match builtin {
                    ast::Builtin::SizeOf(ty) => {
                        b.begin_child("@size_of".to_string());
                        ty.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::Builtin::AlignOf(ty) => {
                        b.begin_child("@align_of".to_string());
                        ty.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                    ast::Builtin::Panic(expr) => {
                        b.begin_child("@panic".to_string());
                        expr.print_tree(b, workspace, tycx);
                        b.end_child();
                    }
                };
            }
            ast::ExprKind::Fn(closure) => {
                b.begin_child(closure.proto.to_string());
                closure.body.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::While { cond, expr } => {
                b.begin_child("while".to_string());
                cond.print_tree(b, workspace, tycx);
                expr.print_tree(b, workspace, tycx);
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

                for_.expr.print_tree(b, workspace, tycx);

                b.end_child();
            }
            ast::ExprKind::Break { deferred } => {
                build_deferred(b, deferred, workspace, tycx);
                b.add_empty_child("break".to_string());
            }
            ast::ExprKind::Continue { deferred } => {
                build_deferred(b, deferred, workspace, tycx);
                b.add_empty_child("continue".to_string());
            }
            ast::ExprKind::Return { expr, deferred } => {
                b.begin_child("return".to_string());
                build_deferred(b, deferred, workspace, tycx);
                expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                b.begin_child(format!("if <{}>", tycx.ty_kind(self.ty)));
                cond.print_tree(b, workspace, tycx);
                then_expr.print_tree(b, workspace, tycx);

                if let Some(else_expr) = else_expr {
                    b.begin_child("else".to_string());
                    else_expr.print_tree(b, workspace, tycx);
                    b.end_child();
                }

                b.end_child();
            }
            ast::ExprKind::Block(block) => {
                block.print_tree(b, workspace, tycx);
            }
            ast::ExprKind::Binary { lhs, op, rhs } => {
                b.begin_child(format!("{} <{}>", op, tycx.ty_kind(self.ty)));
                lhs.print_tree(b, workspace, tycx);
                rhs.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Unary { op, lhs } => {
                b.begin_child(format!("{} <{}>", op, tycx.ty_kind(self.ty)));
                lhs.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Subscript { expr, index } => {
                b.begin_child(format!("subscript <{}>", tycx.ty_kind(self.ty)));
                expr.print_tree(b, workspace, tycx);
                index.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Slice { expr, low, high } => {
                b.begin_child("slice".to_string());
                expr.print_tree(b, workspace, tycx);

                if let Some(low) = low {
                    low.print_tree(b, workspace, tycx);
                } else {
                    b.add_empty_child("0".to_string());
                }

                if let Some(high) = high {
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
                    for arg in &call.args {
                        if let Some(symbol) = &arg.symbol {
                            b.begin_child(symbol.value.to_string());
                            arg.value.print_tree(b, workspace, tycx);
                            b.end_child();
                        } else {
                            arg.value.print_tree(b, workspace, tycx);
                        }
                    }
                    b.end_child();
                }

                b.end_child();
            }
            ast::ExprKind::MemberAccess { expr, member } => {
                b.begin_child(format!("access `{}` <{}>", member, tycx.ty_kind(self.ty)));
                expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::Id { symbol, .. } => {
                b.add_empty_child(format!("`{}` <{}>", symbol, tycx.ty_kind(self.ty)));
            }
            ast::ExprKind::ArrayLiteral(kind) => {
                b.begin_child(format!("array literal <{}>", tycx.ty_kind(self.ty)));

                match kind {
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
            ast::ExprKind::TupleLiteral(elements) => {
                b.begin_child(format!("tuple literal <{}>", tycx.ty_kind(self.ty)));
                elements.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                b.begin_child(format!("struct literal <{}>", tycx.ty_kind(self.ty)));
                type_expr.print_tree(b, workspace, tycx);
                for f in fields {
                    b.begin_child(f.symbol.to_string());
                    f.value.print_tree(b, workspace, tycx);
                    b.end_child();
                }
                b.end_child();
            }
            ast::ExprKind::Literal(kind) => {
                b.add_empty_child(format!("{} <{}>", kind.to_string(), tycx.ty_kind(self.ty)));
            }
            ast::ExprKind::PointerType(expr, is_mutable) => {
                b.begin_child(format!(
                    "{}pointer type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    tycx.ty_kind(self.ty)
                ));
                expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::MultiPointerType(expr, is_mutable) => {
                b.begin_child(format!(
                    "{}multi-pointer type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    tycx.ty_kind(self.ty)
                ));
                expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::SliceType(expr, is_mutable) => {
                b.begin_child(format!(
                    "{}slice type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    tycx.ty_kind(self.ty)
                ));
                expr.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::ArrayType(expr, size) => {
                b.begin_child(format!("array type <{}>", tycx.ty_kind(self.ty)));

                b.begin_child("type".to_string());
                expr.print_tree(b, workspace, tycx);
                b.end_child();

                b.begin_child("size".to_string());
                size.print_tree(b, workspace, tycx);
                b.end_child();

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
            ast::ExprKind::FnType(proto) => {
                b.begin_child("fn type".to_string());
                proto.print_tree(b, workspace, tycx);
                b.end_child();
            }
            ast::ExprKind::SelfType => {
                b.add_empty_child("Self".to_string());
            }
            ast::ExprKind::NeverType => {
                b.add_empty_child("! (never)".to_string());
            }
            ast::ExprKind::UnitType => {
                b.add_empty_child("() (unit)".to_string());
            }
            ast::ExprKind::PlaceholderType => {
                b.add_empty_child("_ (type hole)".to_string());
            }
            ast::ExprKind::Noop => {
                b.add_empty_child("noop".to_string());
            }
        }
    }
}

impl PrintTree for ast::Cast {
    fn print_tree(&self, b: &mut TreeBuilder, workspace: &Workspace, tycx: &TyContext) {
        b.begin_child("from".to_string());
        self.expr.print_tree(b, workspace, tycx);
        b.end_child();

        if let Some(type_expr) = &self.type_expr {
            b.begin_child("to".to_string());
            type_expr.print_tree(b, workspace, tycx);
            b.end_child();
        } else {
            b.add_empty_child(format!("autocast -> {}", self.target_ty));
        }
    }
}
