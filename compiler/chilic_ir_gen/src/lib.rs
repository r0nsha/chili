mod defer;
pub mod ir_gen;
mod use_wildcard;

use chilic_ast::entity::Entity;
use chilic_ast::expr::{
    ArrayLiteralKind, Block, Builtin, Call, CallArg, Expr, ExprKind, ForIter,
    StructLiteralField, StructType, StructTypeField, TypeCastInfo,
};
use chilic_ast::foreign_library::ForeignLibrary;
use chilic_ast::func::{Fn, FnParam, Proto};
use chilic_ast::ir::Ir;
use chilic_ast::item::{ItemKind, Items};
use chilic_ast::module::Module;
use chilic_ast::stmt::{Stmt, StmtKind};
use chilic_error::DiagnosticResult;
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
            ItemKind::Use(use_) => module.uses.push(use_.clone()),
            ItemKind::Entity(entity) => {
                module.entities.push(entity.lower(&mut ctx))
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
        let lib = ForeignLibrary::from_str(lib, *module_path, span)?;
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

impl<T: Lower> Lower for Option<T> {
    fn lower(&self, ctx: &mut IrGenContext) -> Option<T> {
        self.as_ref().map(|s| s.lower(ctx))
    }
}

impl<T: Lower> Lower for Box<T> {
    fn lower(&self, ctx: &mut IrGenContext) -> Box<T> {
        Box::new(self.as_ref().lower(ctx))
    }
}

impl Lower for Fn {
    fn lower(&self, ctx: &mut IrGenContext) -> Fn {
        Fn {
            proto: self.proto.lower(ctx),
            body: self.body.lower(ctx),
            is_startup: false,
        }
    }
}

impl Lower for Block {
    fn lower(&self, ctx: &mut IrGenContext) -> Block {
        ctx.env.push_scope();
        ctx.defercx.push_stack(DeferStackKind::Block);

        let exprs = self.exprs.lower(ctx);
        let deferred = ctx.defercx.collect_deferred();

        ctx.defercx.pop_stack();
        ctx.env.pop_scope();

        Block {
            exprs,
            deferred,
            yields: self.yields,
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
            StmtKind::Use(use_) => {
                Stmt::new(StmtKind::Use(use_.clone()), self.span.clone())
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
            ExprKind::Use(use_) => ExprKind::Use(use_.clone()),
            ExprKind::Foreign(entities) => {
                ExprKind::Foreign(entities.lower(ctx))
            }
            ExprKind::Entity(entity) => ExprKind::Entity(entity.lower(ctx)),
            ExprKind::Defer(expr) => {
                ctx.defercx.current_stack_mut().deferred.push(*expr.clone());
                ExprKind::Noop
            }
            ExprKind::Assign { lvalue, rvalue } => ExprKind::Assign {
                lvalue: lvalue.lower(ctx),
                rvalue: rvalue.lower(ctx),
            },
            ExprKind::Cast(info) => ExprKind::Cast(info.lower(ctx)),
            ExprKind::Builtin(builtin) => ExprKind::Builtin(match builtin {
                Builtin::SizeOf(expr) => Builtin::SizeOf(expr.lower(ctx)),
                Builtin::AlignOf(expr) => Builtin::AlignOf(expr.lower(ctx)),
                Builtin::Panic(expr) => Builtin::Panic(expr.lower(ctx)),
            }),
            ExprKind::Fn(func) => ExprKind::Fn(func.lower(ctx)),
            ExprKind::While { cond, expr } => ExprKind::While {
                cond: cond.lower(ctx),
                expr: expr.lower(ctx),
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
                        ForIter::Range(start, end) => {
                            ForIter::Range(start.lower(ctx), end.lower(ctx))
                        }
                        ForIter::Value(value) => {
                            ForIter::Value(value.lower(ctx))
                        }
                    },
                    expr: expr.lower(ctx),
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
                        Some(expr) => Some(expr.lower(ctx)),
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
                cond: cond.lower(ctx),
                then_expr: then_expr.lower(ctx),
                else_expr: else_expr.lower(ctx),
            },
            ExprKind::Block(block) => ExprKind::Block(block.lower(ctx)),
            ExprKind::Binary { lhs, op, rhs } => ExprKind::Binary {
                lhs: lhs.lower(ctx),
                op: *op,
                rhs: rhs.lower(ctx),
            },
            ExprKind::Unary { op, lhs } => ExprKind::Unary {
                op: *op,
                lhs: lhs.lower(ctx),
            },
            ExprKind::Subscript { expr, index } => ExprKind::Subscript {
                expr: expr.lower(ctx),
                index: index.lower(ctx),
            },
            ExprKind::Slice { expr, low, high } => ExprKind::Slice {
                expr: expr.lower(ctx),
                low: low.lower(ctx),
                high: high.lower(ctx),
            },
            ExprKind::Call(call) => ExprKind::Call(Call {
                callee: call.callee.lower(ctx),
                args: call
                    .args
                    .iter()
                    .map(|a| CallArg {
                        symbol: a.symbol.clone(),
                        value: a.value.lower(ctx),
                    })
                    .collect(),
            }),
            ExprKind::MemberAccess { expr, member } => ExprKind::MemberAccess {
                expr: expr.lower(ctx),
                member: *member,
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
                            len: len.lower(ctx),
                            expr: expr.lower(ctx),
                        }
                    }
                })
            }
            ExprKind::TupleLiteral(elements) => {
                ExprKind::TupleLiteral(elements.lower(ctx))
            }
            ExprKind::StructLiteral { type_expr, fields } => {
                ExprKind::StructLiteral {
                    type_expr: type_expr.lower(ctx),
                    fields: fields
                        .iter()
                        .map(|field| StructLiteralField {
                            symbol: field.symbol,
                            value: field.value.lower(ctx),
                            span: field.span.clone(),
                        })
                        .collect(),
                }
            }
            ExprKind::Literal(kind) => ExprKind::Literal(kind.clone()),
            ExprKind::PointerType(expr, is_mutable) => {
                ExprKind::PointerType(expr.lower(ctx), *is_mutable)
            }
            ExprKind::MultiPointerType(expr, is_mutable) => {
                ExprKind::MultiPointerType(expr.lower(ctx), *is_mutable)
            }
            ExprKind::ArrayType(expr, size) => {
                ExprKind::ArrayType(expr.lower(ctx), size.lower(ctx))
            }
            ExprKind::SliceType(expr, is_mutable) => {
                ExprKind::SliceType(expr.lower(ctx), *is_mutable)
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
                    ty: param.ty.lower(ctx),
                })
                .collect(),
            variadic: self.variadic,
            ret: self.ret.lower(ctx),
            lib_name: self.lib_name,
            ty: Ty::Unknown,
        }
    }
}

impl Lower for TypeCastInfo {
    fn lower(&self, ctx: &mut IrGenContext) -> TypeCastInfo {
        TypeCastInfo {
            expr: self.expr.lower(ctx),
            type_expr: self.type_expr.lower(ctx),
            target_ty: Ty::Unknown,
        }
    }
}
