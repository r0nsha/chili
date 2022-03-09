use crate::ast::{
    ArrayLiteralKind, Ast, Binding, Block, Builtin, Cast, Expr, ExprKind, Fn,
    ForIter, Ir, LiteralKind, Proto,
};
use ptree::{
    print_config::UTF_CHARS_BOLD, print_tree_with, Color, PrintConfig, Style,
    TreeBuilder,
};
use std::fmt::Display;

impl Ir {
    pub fn print(&self) {
        let config = {
            let mut config = PrintConfig::from_env();
            config.branch = Style {
                foreground: Some(Color::Green),
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

        let mut b = TreeBuilder::new("program".to_string());

        for (name, module) in &self.modules {
            b.begin_child(format!("module '{}'", name));

            for import in module.imports.iter() {
                b.add_empty_child(format!(
                    "use {} = {}",
                    import.module_info.name, import.alias
                ));
            }

            module.bindings.build(&mut b);

            b.end_child();
        }

        let tree = b.build();

        print_tree_with(&tree, &config).expect("error printing ir tree");

        println!();
    }
}

impl Ast {
    pub fn print(&self) {
        let config = {
            let mut config = PrintConfig::from_env();
            config.branch = Style {
                foreground: Some(Color::Green),
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

        let mut b = TreeBuilder::new("program".to_string());

        b.begin_child(format!(
            "module '{}' ({})",
            self.module_info.name, self.module_info.file_path
        ));

        for import in self.imports.iter() {
            b.add_empty_child(format!(
                "use {} as {}",
                import.module_info.name, import.alias
            ));
        }

        self.bindings.build(&mut b);

        b.end_child();

        let tree = b.build();

        print_tree_with(&tree, &config).expect("error printing ir tree");

        println!();
    }
}

trait BuildNode {
    fn build(&self, b: &mut TreeBuilder);
}

impl<T: BuildNode> BuildNode for Vec<T> {
    fn build(&self, b: &mut TreeBuilder) {
        for element in self {
            element.build(b);
        }
    }
}

impl<T: BuildNode> BuildNode for Option<T> {
    fn build(&self, b: &mut TreeBuilder) {
        if let Some(e) = self {
            e.build(b);
        }
    }
}

impl<T: BuildNode> BuildNode for Box<T> {
    fn build(&self, b: &mut TreeBuilder) {
        self.as_ref().build(b);
    }
}

impl BuildNode for Fn {
    fn build(&self, b: &mut TreeBuilder) {
        b.begin_child("fn".to_string());
        self.proto.build(b);
        self.body.build(b);
        b.end_child();
    }
}

impl BuildNode for Block {
    fn build(&self, b: &mut TreeBuilder) {
        b.begin_child(format!(
            "block{}",
            if self.yields { " (yields)" } else { "" }
        ));
        self.exprs.build(b);
        build_deferred(b, &self.deferred);
        b.end_child();
    }
}

impl BuildNode for Proto {
    fn build(&self, b: &mut TreeBuilder) {
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
                ty.build(b);
            } else {
                b.add_empty_child("inferred".to_string());
            }

            b.end_child();
        }

        b.end_child();

        if let Some(ret) = &self.ret {
            b.begin_child("return".to_string());
            ret.build(b);
            b.end_child();
        }

        b.end_child();
    }
}

impl BuildNode for Binding {
    fn build(&self, b: &mut TreeBuilder) {
        b.begin_child(format!(
            "let {} <{}>",
            self.pattern.to_string(),
            self.ty
        ));

        if let Some(value) = &self.value {
            value.build(b);
        } else {
            b.add_empty_child("[uninit]".to_string());
        }

        b.end_child();
    }
}

fn build_deferred(b: &mut TreeBuilder, deferred: &Vec<Expr>) {
    if deferred.is_empty() {
        return;
    }
    b.begin_child("deferred".to_string());
    deferred.build(b);
    b.end_child();
}

impl BuildNode for Expr {
    fn build(&self, b: &mut ptree::TreeBuilder) {
        match &self.kind {
            ExprKind::Import(imports) => {
                for import in imports.iter() {
                    b.add_empty_child(format!(
                        "use \"{}\" = {}",
                        import.module_info.file_path, import.alias
                    ));
                }
            }
            ExprKind::Foreign(bindings) => bindings.build(b),
            ExprKind::Binding(binding) => binding.build(b),
            ExprKind::Defer(expr) => {
                b.begin_child("defer".to_string());
                expr.build(b);
                b.end_child();
            }
            ExprKind::Assign { lvalue, rvalue } => {
                b.begin_child("assign".to_string());
                lvalue.build(b);
                rvalue.build(b);
                b.end_child();
            }
            ExprKind::Cast(info) => {
                b.begin_child("as".to_string());
                info.build(b);
                b.end_child();
            }
            ExprKind::Builtin(builtin) => {
                match builtin {
                    Builtin::SizeOf(ty) => {
                        b.begin_child("@size_of".to_string());
                        ty.build(b);
                        b.end_child();
                    }
                    Builtin::AlignOf(ty) => {
                        b.begin_child("@align_of".to_string());
                        ty.build(b);
                        b.end_child();
                    }
                    Builtin::Panic(expr) => {
                        b.begin_child("@panic".to_string());
                        expr.build(b);
                        b.end_child();
                    }
                };
            }
            ExprKind::Fn(closure) => {
                b.begin_child(closure.proto.to_string());
                closure.body.build(b);
                b.end_child();
            }
            ExprKind::While { cond, expr } => {
                b.begin_child("while".to_string());

                b.begin_child("cond".to_string());
                cond.build(b);
                b.end_child();

                expr.build(b);

                b.end_child();
            }
            ExprKind::For {
                iter_name,
                iter_index_name,
                iterator,
                expr,
            } => {
                b.begin_child(format!(
                    "for ({}, {})",
                    iter_name, iter_index_name
                ));

                match iterator {
                    ForIter::Range(start, end) => {
                        b.begin_child("start".to_string());
                        start.build(b);
                        b.end_child();

                        b.begin_child("end".to_string());
                        end.build(b);
                        b.end_child();
                    }
                    ForIter::Value(value) => {
                        b.begin_child("iter".to_string());
                        value.build(b);
                        b.end_child();
                    }
                }

                expr.build(b);

                b.end_child();
            }
            ExprKind::Break { deferred } => {
                build_deferred(b, deferred);
                b.add_empty_child("break".to_string());
            }
            ExprKind::Continue { deferred } => {
                build_deferred(b, deferred);
                b.add_empty_child("continue".to_string());
            }
            ExprKind::Return { expr, deferred } => {
                b.begin_child("return".to_string());
                build_deferred(b, deferred);
                expr.build(b);
                b.end_child();
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                b.begin_child(format!("if <{}>", self.ty));
                cond.build(b);
                then_expr.build(b);

                if let Some(else_expr) = else_expr {
                    b.begin_child("else".to_string());
                    else_expr.build(b);
                    b.end_child();
                }

                b.end_child();
            }
            ExprKind::Block(block) => {
                block.build(b);
            }
            ExprKind::Binary { lhs, op, rhs } => {
                b.begin_child(format!("{} <{}>", op, self.ty));
                lhs.build(b);
                rhs.build(b);
                b.end_child();
            }
            ExprKind::Unary { op, lhs } => {
                b.begin_child(format!("{} <{}>", op, self.ty));
                lhs.build(b);
                b.end_child();
            }
            ExprKind::Subscript { expr, index } => {
                b.begin_child(format!("subscript <{}>", self.ty));
                expr.build(b);
                index.build(b);
                b.end_child();
            }
            ExprKind::Slice { expr, low, high } => {
                b.begin_child("slice".to_string());
                expr.build(b);

                if let Some(low) = low {
                    low.build(b);
                } else {
                    b.add_empty_child("0".to_string());
                }

                if let Some(high) = high {
                    high.build(b);
                } else {
                    b.add_empty_child("n".to_string());
                }

                b.end_child();
            }
            ExprKind::Call(call) => {
                b.begin_child(format!("call <{}>", self.ty));

                b.begin_child(format!("callee <{}>", call.callee.ty));
                call.callee.build(b);
                b.end_child();

                if !call.args.is_empty() {
                    b.begin_child("args".to_string());
                    for arg in &call.args {
                        if let Some(symbol) = &arg.symbol {
                            b.begin_child(symbol.value.to_string());
                            arg.value.build(b);
                            b.end_child();
                        } else {
                            arg.value.build(b);
                        }
                    }
                    b.end_child();
                }

                b.end_child();
            }
            ExprKind::MemberAccess { expr, member } => {
                b.begin_child(format!("access `{}` <{}>", member, self.ty));
                expr.build(b);
                b.end_child();
            }
            ExprKind::Id { symbol, .. } => {
                b.add_empty_child(format!("`{}` <{}>", symbol, self.ty));
            }
            ExprKind::ArrayLiteral(kind) => {
                b.begin_child(format!("array literal <{}>", self.ty));

                match kind {
                    ArrayLiteralKind::List(elements) => {
                        b.begin_child("list".to_string());
                        elements.build(b);
                        b.end_child();
                    }
                    ArrayLiteralKind::Fill { expr, len } => {
                        b.begin_child("fill".to_string());
                        len.build(b);
                        expr.build(b);
                        b.end_child();
                    }
                }

                b.end_child();
            }
            ExprKind::TupleLiteral(elements) => {
                b.begin_child(format!("tuple literal <{}>", self.ty));
                elements.build(b);
                b.end_child();
            }
            ExprKind::StructLiteral { type_expr, fields } => {
                b.begin_child(format!("struct literal <{}>", self.ty));
                type_expr.build(b);
                for f in fields {
                    b.begin_child(f.symbol.to_string());
                    f.value.build(b);
                    b.end_child();
                }
                b.end_child();
            }
            ExprKind::Literal(kind) => {
                b.add_empty_child(format!(
                    "{} <{}>",
                    kind.to_string(),
                    self.ty
                ));
            }
            ExprKind::PointerType(expr, is_mutable) => {
                b.begin_child(format!(
                    "{}pointer type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    self.ty
                ));
                expr.build(b);
                b.end_child();
            }
            ExprKind::MultiPointerType(expr, is_mutable) => {
                b.begin_child(format!(
                    "{}multi-pointer type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    self.ty
                ));
                expr.build(b);
                b.end_child();
            }
            ExprKind::SliceType(expr, is_mutable) => {
                b.begin_child(format!(
                    "{}slice type <{}>",
                    if *is_mutable { "mut " } else { "" },
                    self.ty
                ));
                expr.build(b);
                b.end_child();
            }
            ExprKind::ArrayType(expr, size) => {
                b.begin_child(format!("array type <{}>", self.ty));

                b.begin_child("type".to_string());
                expr.build(b);
                b.end_child();

                b.begin_child("size".to_string());
                size.build(b);
                b.end_child();

                b.end_child();
            }
            ExprKind::StructType(ty) => {
                b.begin_child("struct type".to_string());
                for field in &ty.fields {
                    b.begin_child(format!("field {}", field.name));
                    field.ty.build(b);
                    b.end_child();
                }
                b.end_child();
            }
            ExprKind::FnType(proto) => {
                b.begin_child("fn type".to_string());
                proto.build(b);
                b.end_child();
            }
            ExprKind::SelfType => {
                b.add_empty_child("Self".to_string());
            }
            ExprKind::NeverType => {
                b.add_empty_child("! (never)".to_string());
            }
            ExprKind::UnitType => {
                b.add_empty_child("() (unit)".to_string());
            }
            ExprKind::PlaceholderType => {
                b.add_empty_child("_ (type hole)".to_string());
            }
            ExprKind::Noop => {
                b.add_empty_child("noop".to_string());
            }
        }
    }
}

impl Display for Proto {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                "fn: {} ({}{})",
                self.name,
                self.params
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                if self.variadic { ", .." } else { "" }
            )
        )
    }
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralKind::Unit => "()".to_string(),
                LiteralKind::Nil => "nil".to_string(),
                LiteralKind::Bool(v) => v.to_string(),
                LiteralKind::Int(v) => v.to_string(),
                LiteralKind::Float(v) => v.to_string(),
                LiteralKind::Str(v) => format!("\"{}\"", v),
                LiteralKind::Char(v) => format!("'{}'", v),
            }
        )
    }
}

impl BuildNode for Cast {
    fn build(&self, b: &mut TreeBuilder) {
        b.begin_child("from".to_string());
        self.expr.build(b);
        b.end_child();

        if let Some(type_expr) = &self.type_expr {
            b.begin_child("to".to_string());
            type_expr.build(b);
            b.end_child();
        } else {
            b.add_empty_child(format!("autocast -> {}", self.target_ty));
        }
    }
}
