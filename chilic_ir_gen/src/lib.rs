mod defer;
pub mod ir_gen;
mod use_wildcard;

use chilic_error::DiagnosticResult;
use chilic_ir::entity::Entity;
use chilic_ir::expr::{
    ArrayLiteralKind, Builtin, Call, CallArg, Expr, ExprKind, ForIter,
    LiteralKind, StructLiteralField, StructType, StructTypeField, TypeCastInfo,
};
use chilic_ir::foreign_lib::ForeignLib;
use chilic_ir::func::{Fn, FnParam, Proto};
use chilic_ir::ir::Ir;
use chilic_ir::item::{ItemKind, Items};
use chilic_ir::module::Module;
use chilic_ir::op::{BinaryOp, UnaryOp};
use chilic_ir::stmt::{Stmt, StmtKind};
use chilic_span::Span;
use chilic_ty::Ty;
use codespan_reporting::files::SimpleFiles;
use common::env::Env;
use defer::{DeferContext, DeferStackKind};
use ustr::{ustr, Ustr};

use self::use_wildcard::expand_use_wildcards;

struct ForeignLibraryData {
    lib: Ustr,
    module_path: Ustr,
    span: Span,
}

impl ForeignLibraryData {
    fn new(lib: Ustr, module_path: Ustr, span: Span) -> Self {
        Self {
            lib,
            module_path,
            span,
        }
    }
}

struct IrGenContext {
    module_path: Ustr,
    env: Env<()>,
    foreign_libraries: Vec<ForeignLibraryData>,
    defercx: DeferContext,
}

pub fn gen_structured_ir(
    items: &Items,
    files: SimpleFiles<String, String>,
) -> DiagnosticResult<Ir> {
    let mut ctx = IrGenContext {
        module_path: ustr(""),
        env: Env::new(),
        foreign_libraries: vec![],
        defercx: DeferContext::new(),
    };

    let mut ir = Ir::new(files);

    for item in items {
        let module = ir
            .modules
            .entry(item.module_info.name)
            .or_insert_with(|| Module::new(item.module_info));

        ctx.module_path = item.module_info.file_path;
        ctx.env.push_named_scope(item.module_info.name);

        match &item.kind {
            ItemKind::UseDecl(use_) => module.uses.push(use_.clone()),
            ItemKind::Entity(entity) => {
                module.entitys.push(entity.lower(&mut ctx))
            }
        }

        ctx.env.pop_scope();
    }

    for ForeignLibraryData {
        lib,
        module_path,
        span,
    } in &ctx.foreign_libraries
    {
        let lib = ForeignLib::from_str(lib, *module_path, span)?;
        ir.foreign_libraries.insert(lib);
    }

    expand_use_wildcards(&mut ir)?;

    Ok(ir)
}

trait Lower {
    fn lower(&self, ctx: &mut IrGenContext) -> Self;
}

impl<T: Lower> Lower for Vec<T> {
    fn lower(&self, ctx: &mut IrGenContext) -> Vec<T> {
        self.iter().map(|a| a.lower(ctx)).collect()
    }
}

impl Lower for Fn {
    fn lower(&self, ctx: &mut IrGenContext) -> Fn {
        ctx.env.push_named_scope(self.proto.name);
        ctx.defercx.push_stack(DeferStackKind::Fn);

        let body: Vec<Stmt> = self.body.lower(ctx);
        let deferred = ctx.defercx.current_deferred();

        ctx.defercx.pop_stack();
        ctx.env.pop_scope();

        let proto: Proto = self.proto.lower(ctx);

        Fn {
            proto,
            body,
            deferred,
            is_startup: false,
        }
    }
}

impl Lower for Entity {
    fn lower(&self, ctx: &mut IrGenContext) -> Entity {
        Entity {
            kind: self.kind,
            pattern: self.pattern.clone(),
            ty_expr: self.ty_expr.as_ref().map(|ty_expr| ty_expr.lower(ctx)),
            ty: Ty::Unknown,
            value: if let Some(value) = &self.value {
                Some(value.lower(ctx))
            } else {
                None
            },
            visibility: self.visibility,
            const_value: self.const_value.clone(),
            should_codegen: self.should_codegen,
            lib_name: self.lib_name,
        }
    }
}

impl Lower for Stmt {
    fn lower(&self, ctx: &mut IrGenContext) -> Stmt {
        match &self.kind {
            StmtKind::UseDecl(use_) => {
                Stmt::new(StmtKind::UseDecl(use_.clone()), self.span.clone())
            }
            StmtKind::Entity(entity) => Stmt::new(
                StmtKind::Entity(entity.lower(ctx)),
                self.span.clone(),
            ),
            StmtKind::Defer(expr) => {
                ctx.defercx.current_stack_mut().deferred.push(expr.clone());

                Stmt::new(
                    StmtKind::Expr {
                        expr: Expr::typed(
                            ExprKind::Noop,
                            Ty::Unit,
                            self.span.clone(),
                        ),
                        terminated: true,
                    },
                    self.span.clone(),
                )
            }
            StmtKind::Expr { expr, terminated } => Stmt::new(
                StmtKind::Expr {
                    expr: expr.lower(ctx),
                    terminated: *terminated,
                },
                self.span.clone(),
            ),
        }
    }
}

impl Lower for Expr {
    fn lower(&self, ctx: &mut IrGenContext) -> Expr {
        let kind = match &self.kind {
            ExprKind::Assign { lvalue, rvalue } => ExprKind::Assign {
                lvalue: Box::new(lvalue.lower(ctx)),
                rvalue: Box::new(rvalue.lower(ctx)),
            },
            ExprKind::Cast(info) => ExprKind::Cast(info.lower(ctx)),
            ExprKind::Builtin(builtin) => ExprKind::Builtin(match builtin {
                Builtin::SizeOf(expr) => {
                    Builtin::SizeOf(Box::new(expr.lower(ctx)))
                }
                Builtin::AlignOf(expr) => {
                    Builtin::AlignOf(Box::new(expr.lower(ctx)))
                }
                Builtin::Panic(expr) => Builtin::Panic(
                    expr.as_ref().map(|e| Box::new(e.lower(ctx))),
                ),
            }),
            ExprKind::Fn(func) => ExprKind::Fn(func.lower(ctx)),
            ExprKind::While { cond, expr } => ExprKind::While {
                cond: Box::new(cond.lower(ctx)),
                expr: Box::new(expr.lower(ctx)),
            },
            ExprKind::For {
                iter_name,
                iter_index_name,
                iterator,
                expr,
            } => {
                ctx.env.push_scope();
                ctx.defercx.push_stack(DeferStackKind::Loop);

                let _for = ExprKind::For {
                    iter_name: *iter_name,
                    iter_index_name: *iter_index_name,
                    iterator: match iterator {
                        ForIter::Range(start, end) => ForIter::Range(
                            Box::new(start.lower(ctx)),
                            Box::new(end.lower(ctx)),
                        ),
                        ForIter::Value(value) => {
                            ForIter::Value(Box::new(value.lower(ctx)))
                        }
                    },
                    expr: Box::new(expr.lower(ctx)),
                };

                ctx.defercx.pop_stack();
                ctx.env.pop_scope();

                _for
            }
            ExprKind::Break { .. } => {
                let mut deferred = vec![];

                for stack in ctx.defercx.stacks().iter().rev() {
                    if let DeferStackKind::Loop = stack.kind {
                        break;
                    }

                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }

                ExprKind::Break { deferred }
            }
            ExprKind::Continue { .. } => {
                let mut deferred = vec![];

                for stack in ctx.defercx.stacks().iter().rev() {
                    if let DeferStackKind::Loop = stack.kind {
                        break;
                    }

                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }

                ExprKind::Continue { deferred }
            }
            ExprKind::Return { expr, .. } => {
                let mut deferred = vec![];

                for stack in ctx.defercx.stacks().iter().rev() {
                    for expr in stack.deferred.iter().rev() {
                        deferred.push(expr.clone())
                    }
                }

                ExprKind::Return {
                    expr: match expr {
                        Some(expr) => Some(Box::new(expr.lower(ctx))),
                        None => None,
                    },
                    deferred,
                }
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => ExprKind::If {
                cond: Box::new(cond.lower(ctx)),
                then_expr: Box::new(then_expr.lower(ctx)),
                else_expr: match else_expr {
                    Some(else_expr) => Some(Box::new(else_expr.lower(ctx))),
                    None => None,
                },
            },
            ExprKind::Block { stmts, .. } => {
                ctx.env.push_scope();
                ctx.defercx.push_stack(DeferStackKind::Block);

                let stmts: Vec<Stmt> = stmts.lower(ctx);
                let deferred = ctx.defercx.current_deferred();

                ctx.defercx.pop_stack();
                ctx.env.pop_scope();

                ExprKind::Block { stmts, deferred }
            }
            ExprKind::Binary { lhs, op, rhs } => ExprKind::Binary {
                lhs: Box::new(lhs.lower(ctx)),
                op: match op {
                    BinaryOp::Add => BinaryOp::Add,
                    BinaryOp::Sub => BinaryOp::Sub,
                    BinaryOp::Mul => BinaryOp::Mul,
                    BinaryOp::Div => BinaryOp::Div,
                    BinaryOp::Rem => BinaryOp::Rem,
                    BinaryOp::Eq => BinaryOp::Eq,
                    BinaryOp::NEq => BinaryOp::NEq,
                    BinaryOp::Lt => BinaryOp::Lt,
                    BinaryOp::LtEq => BinaryOp::LtEq,
                    BinaryOp::Gt => BinaryOp::Gt,
                    BinaryOp::GtEq => BinaryOp::GtEq,
                    BinaryOp::And => BinaryOp::And,
                    BinaryOp::Or => BinaryOp::Or,
                    BinaryOp::Shl => BinaryOp::Shl,
                    BinaryOp::Shr => BinaryOp::Shr,
                    BinaryOp::BitwiseAnd => BinaryOp::BitwiseAnd,
                    BinaryOp::BitwiseOr => BinaryOp::BitwiseOr,
                    BinaryOp::BitwiseXor => BinaryOp::BitwiseXor,
                },
                rhs: Box::new(rhs.lower(ctx)),
            },
            ExprKind::Unary { op, lhs } => ExprKind::Unary {
                op: match op {
                    UnaryOp::Ref(is_mutable) => UnaryOp::Ref(*is_mutable),
                    UnaryOp::Deref => UnaryOp::Deref,
                    UnaryOp::Neg => UnaryOp::Neg,
                    UnaryOp::Plus => UnaryOp::Plus,
                    UnaryOp::Not => UnaryOp::Not,
                    UnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
                },
                lhs: Box::new(lhs.lower(ctx)),
            },
            ExprKind::Subscript { expr, index } => ExprKind::Subscript {
                expr: Box::new(expr.lower(ctx)),
                index: Box::new(index.lower(ctx)),
            },
            ExprKind::Slice { expr, low, high } => ExprKind::Slice {
                expr: Box::new(expr.lower(ctx)),
                low: match low {
                    Some(low) => Some(Box::new(low.lower(ctx))),
                    None => None,
                },
                high: match high {
                    Some(high) => Some(Box::new(high.lower(ctx))),
                    None => None,
                },
            },
            ExprKind::Call(call) => ExprKind::Call(Call {
                callee: Box::new(call.callee.lower(ctx)),
                args: call
                    .args
                    .iter()
                    .map(|a| CallArg {
                        symbol: a.symbol.clone(),
                        value: a.value.lower(ctx),
                    })
                    .collect(),
            }),
            ExprKind::FieldAccess { expr, field } => ExprKind::FieldAccess {
                expr: Box::new(expr.lower(ctx)),
                field: *field,
            },
            ExprKind::Id {
                symbol,
                is_mutable,
                entity_span,
            } => ExprKind::Id {
                symbol: *symbol,
                is_mutable: *is_mutable,
                entity_span: entity_span.clone(),
            },
            ExprKind::ArrayLiteral(kind) => {
                ExprKind::ArrayLiteral(match kind {
                    ArrayLiteralKind::List(elements) => {
                        ArrayLiteralKind::List(elements.lower(ctx))
                    }
                    ArrayLiteralKind::Fill { expr, len } => {
                        ArrayLiteralKind::Fill {
                            len: Box::new(len.lower(ctx)),
                            expr: Box::new(expr.lower(ctx)),
                        }
                    }
                })
            }
            ExprKind::TupleLiteral(elements) => {
                ExprKind::TupleLiteral(elements.lower(ctx))
            }
            ExprKind::StructLiteral(fields) => ExprKind::StructLiteral(
                fields
                    .iter()
                    .map(|field| StructLiteralField {
                        symbol: field.symbol,
                        value: field.value.lower(ctx),
                        span: field.span.clone(),
                    })
                    .collect(),
            ),
            ExprKind::Literal(kind) => ExprKind::Literal(match kind {
                LiteralKind::Unit => LiteralKind::Unit,
                LiteralKind::Nil => LiteralKind::Nil,
                LiteralKind::Bool(v) => LiteralKind::Bool(*v),
                LiteralKind::Int(v) => LiteralKind::Int(*v),
                LiteralKind::Float(v) => LiteralKind::Float(*v),
                LiteralKind::Str(v) => LiteralKind::Str(v.clone()),
                LiteralKind::Char(v) => LiteralKind::Char(*v),
            }),
            ExprKind::PointerType(expr, is_mutable) => {
                ExprKind::PointerType(Box::new(expr.lower(ctx)), *is_mutable)
            }
            ExprKind::MultiPointerType(expr, is_mutable) => {
                ExprKind::MultiPointerType(
                    Box::new(expr.lower(ctx)),
                    *is_mutable,
                )
            }
            ExprKind::ArrayType(expr, size) => ExprKind::ArrayType(
                Box::new(expr.lower(ctx)),
                Box::new(size.lower(ctx)),
            ),
            ExprKind::SliceType(expr, is_mutable) => {
                ExprKind::SliceType(Box::new(expr.lower(ctx)), *is_mutable)
            }
            ExprKind::StructType(t) => ExprKind::StructType(StructType {
                name: t.name,
                qualified_name: ustr(&format!(
                    "{}.{}",
                    ctx.env.scope_name(),
                    t.name
                )),
                kind: t.kind,
                fields: t
                    .fields
                    .iter()
                    .map(|f| StructTypeField {
                        name: f.name,
                        ty: f.ty.lower(ctx),
                        span: f.span.clone(),
                    })
                    .collect(),
            }),
            ExprKind::FnType(proto) => {
                if let Some(lib_name) = &proto.lib_name {
                    ctx.foreign_libraries.push(ForeignLibraryData::new(
                        *lib_name,
                        ctx.module_path,
                        self.span.clone(),
                    ));
                }
                ExprKind::FnType(proto.lower(ctx))
            }
            ExprKind::SelfType => ExprKind::SelfType,
            ExprKind::NeverType => ExprKind::NeverType,
            ExprKind::UnitType => ExprKind::UnitType,
            ExprKind::PlaceholderType => ExprKind::PlaceholderType,
            ExprKind::Noop => ExprKind::Noop,
        };

        Expr::typed(kind, Ty::Unknown, self.span.clone())
    }
}

impl Lower for Proto {
    fn lower(&self, ctx: &mut IrGenContext) -> Proto {
        Proto {
            name: self.name,
            params: self
                .params
                .iter()
                .map(|param| FnParam {
                    pattern: param.pattern.clone(),
                    ty: param.ty.as_ref().map(|ty| Box::new(ty.lower(ctx))),
                })
                .collect(),
            variadic: self.variadic,
            ret: self.ret.as_ref().map(|ret_ty| Box::new(ret_ty.lower(ctx))),
            lib_name: self.lib_name,
            ty: Ty::Unknown,
        }
    }
}

impl Lower for TypeCastInfo {
    fn lower(&self, ctx: &mut IrGenContext) -> TypeCastInfo {
        TypeCastInfo {
            expr: Box::new(self.expr.lower(ctx)),
            type_expr: self
                .type_expr
                .as_ref()
                .map(|type_expr| Box::new(type_expr.lower(ctx))),
            target_ty: Ty::Unknown,
        }
    }
}
