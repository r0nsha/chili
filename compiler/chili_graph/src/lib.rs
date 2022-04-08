use std::collections::HashSet;

use chili_ast::{
    ast,
    ty::TyKind,
    workspace::{BindingInfoFlags, BindingInfoId, Workspace},
};
use chili_check::{normalize::NormalizeTy, ty_ctx::TyCtx};

pub fn mark_codegen_path(workspace: &mut Workspace, tycx: &TyCtx, ast: &ast::TypedAst) {
    let entry_point_function_id = workspace.entry_point_function_id.unwrap();
    let mut sess = Sess {
        workspace,
        tycx,
        ast,
        visited: HashSet::new(),
    };

    sess.mark_and_visit(entry_point_function_id);
}

struct Sess<'a> {
    workspace: &'a mut Workspace,
    tycx: &'a TyCtx,
    ast: &'a ast::TypedAst,
    visited: HashSet<BindingInfoId>,
}

impl<'a> Sess<'a> {
    fn visit_binding(&mut self, id: BindingInfoId) {
        if let Some(binding) = self.ast.bindings.get(&id) {
            binding.visit(self)
        } else if let Some(import) = self.ast.imports.get(&id) {
            import.visit(self)
        } else {
            // This is a local binding, so it's already visited
        }
    }

    fn mark(&mut self, id: BindingInfoId) {
        let entry_point_function_id = self.workspace.entry_point_function_id.unwrap();
        let info = self.workspace.get_binding_info_mut(id).unwrap();
        let ty = info.ty.normalize(self.tycx);

        let should_codegen =
            info.uses > 0 && !info.kind.is_import() && !ty.is_type() && !ty.is_module();

        if should_codegen || id == entry_point_function_id || !info.scope_level.is_global() {
            info.flags.insert(BindingInfoFlags::SHOULD_CODEGEN);
        }
    }

    fn mark_and_visit(&mut self, id: BindingInfoId) {
        if self.visited.contains(&id) {
            return;
        }

        self.mark(id);
        self.visit_binding(id);

        self.visited.insert(id);
    }
}

trait Visit {
    fn visit(&self, sess: &mut Sess);
}

impl<T: Visit> Visit for Vec<T> {
    fn visit(&self, sess: &mut Sess) {
        for element in self {
            element.visit(sess);
        }
    }
}

impl<T: Visit> Visit for Option<T> {
    fn visit(&self, sess: &mut Sess) {
        if let Some(e) = self {
            e.visit(sess);
        }
    }
}

impl<T: Visit> Visit for Box<T> {
    fn visit(&self, sess: &mut Sess) {
        self.as_ref().visit(sess)
    }
}

impl Visit for ast::Import {
    fn visit(&self, sess: &mut Sess) {
        if let Some(target) = self.target_binding_info_id {
            sess.mark_and_visit(target);
        }
    }
}

impl Visit for ast::Binding {
    fn visit(&self, sess: &mut Sess) {
        self.ty_expr.visit(sess);
        self.expr.visit(sess);
    }
}

impl Visit for ast::Expr {
    fn visit(&self, sess: &mut Sess) {
        match &self.kind {
            ast::ExprKind::Import(imports) => imports.visit(sess),
            ast::ExprKind::Foreign(bindings) => bindings.visit(sess),
            ast::ExprKind::Binding(binding) => binding.visit(sess),
            ast::ExprKind::Defer(expr) => expr.visit(sess),
            ast::ExprKind::Assign(assign) => assign.visit(sess),
            ast::ExprKind::Cast(cast) => cast.visit(sess),
            ast::ExprKind::Builtin(builtin) => builtin.visit(sess),
            ast::ExprKind::Fn(f) => f.visit(sess),
            ast::ExprKind::While(while_) => while_.visit(sess),
            ast::ExprKind::For(for_) => for_.visit(sess),
            ast::ExprKind::Break(e) | ast::ExprKind::Continue(e) => e.deferred.visit(sess),
            ast::ExprKind::Return(ret) => ret.visit(sess),
            ast::ExprKind::If(if_) => if_.visit(sess),
            ast::ExprKind::Block(block) => block.visit(sess),
            ast::ExprKind::Binary(binary) => binary.visit(sess),
            ast::ExprKind::Unary(unary) => unary.visit(sess),
            ast::ExprKind::Subscript(subscript) => subscript.visit(sess),
            ast::ExprKind::Slice(slice) => slice.visit(sess),
            ast::ExprKind::FnCall(fncall) => fncall.visit(sess),
            ast::ExprKind::MemberAccess(access) => access.visit(sess),
            ast::ExprKind::Ident(ident) => ident.visit(sess),
            ast::ExprKind::ArrayLiteral(lit) => lit.visit(sess),
            ast::ExprKind::TupleLiteral(els) => els.visit(sess),
            ast::ExprKind::StructLiteral(lit) => lit.visit(sess),
            ast::ExprKind::PointerType(inner, _)
            | ast::ExprKind::MultiPointerType(inner, _)
            | ast::ExprKind::SliceType(inner, _) => inner.visit(sess),
            ast::ExprKind::ArrayType(inner, size) => {
                inner.visit(sess);
                size.visit(sess);
            }
            ast::ExprKind::StructType(st) => st.visit(sess),
            ast::ExprKind::FnType(sig) => sig.visit(sess),
            ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType => (),
        }
    }
}

impl Visit for ast::Assign {
    fn visit(&self, sess: &mut Sess) {
        self.lvalue.visit(sess);
        self.rvalue.visit(sess);
    }
}

impl Visit for ast::Cast {
    fn visit(&self, sess: &mut Sess) {
        self.expr.visit(sess);
        self.ty_expr.visit(sess);
    }
}

impl Visit for ast::Builtin {
    fn visit(&self, sess: &mut Sess) {
        match self {
            ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => expr.visit(sess),
            ast::Builtin::Panic(expr) => expr.visit(sess),
        }
    }
}

impl Visit for ast::Fn {
    fn visit(&self, sess: &mut Sess) {
        self.sig.visit(sess);
        self.body.visit(sess);
    }
}

impl Visit for ast::FnSig {
    fn visit(&self, sess: &mut Sess) {
        self.params.iter().for_each(|p| p.ty_expr.visit(sess));
        self.ret.visit(sess);
    }
}

impl Visit for ast::While {
    fn visit(&self, sess: &mut Sess) {
        self.cond.visit(sess);
        self.block.visit(sess);
    }
}

impl Visit for ast::For {
    fn visit(&self, sess: &mut Sess) {
        match &self.iterator {
            ast::ForIter::Range(s, e) => {
                s.visit(sess);
                e.visit(sess);
            }
            ast::ForIter::Value(v) => v.visit(sess),
        }

        self.block.visit(sess);
    }
}

impl Visit for ast::Return {
    fn visit(&self, sess: &mut Sess) {
        self.expr.visit(sess);
        self.deferred.visit(sess);
    }
}

impl Visit for ast::If {
    fn visit(&self, sess: &mut Sess) {
        self.cond.visit(sess);
        self.then.visit(sess);
        self.otherwise.visit(sess);
    }
}

impl Visit for ast::Block {
    fn visit(&self, sess: &mut Sess) {
        self.exprs.visit(sess);
        self.deferred.visit(sess);
    }
}

impl Visit for ast::Binary {
    fn visit(&self, sess: &mut Sess) {
        self.lhs.visit(sess);
        self.rhs.visit(sess);
    }
}

impl Visit for ast::Unary {
    fn visit(&self, sess: &mut Sess) {
        self.lhs.visit(sess);
    }
}

impl Visit for ast::Subscript {
    fn visit(&self, sess: &mut Sess) {
        self.expr.visit(sess);
        self.index.visit(sess);
    }
}

impl Visit for ast::Slice {
    fn visit(&self, sess: &mut Sess) {
        self.expr.visit(sess);
        self.low.visit(sess);
        self.high.visit(sess);
    }
}

impl Visit for ast::FnCall {
    fn visit(&self, sess: &mut Sess) {
        self.callee.visit(sess);
        self.args.iter().for_each(|a| a.expr.visit(sess))
    }
}

impl Visit for ast::MemberAccess {
    fn visit(&self, sess: &mut Sess) {
        self.expr.visit(sess);
        match self.expr.ty.normalize(&sess.tycx) {
            TyKind::Module(module_id) => {
                for (&id, binding) in sess.ast.bindings.iter() {
                    let pat = binding.pattern.as_single_ref();

                    if binding.module_id == module_id && pat.symbol == self.member {
                        sess.mark_and_visit(id);
                        return;
                    }
                }

                for (&id, import) in sess.ast.imports.iter() {
                    if import.module_id == module_id && import.alias == self.member {
                        sess.mark_and_visit(id);
                        return;
                    }
                }
            }
            _ => (),
        }
    }
}

impl Visit for ast::Ident {
    fn visit(&self, sess: &mut Sess) {
        sess.mark_and_visit(self.binding_info_id);
    }
}

impl Visit for ast::ArrayLiteralKind {
    fn visit(&self, sess: &mut Sess) {
        match self {
            ast::ArrayLiteralKind::List(els) => els.visit(sess),
            ast::ArrayLiteralKind::Fill { len, expr } => {
                len.visit(sess);
                expr.visit(sess);
            }
        }
    }
}

impl Visit for ast::StructLiteral {
    fn visit(&self, sess: &mut Sess) {
        self.type_expr.visit(sess);
        self.fields.iter().for_each(|f| f.value.visit(sess))
    }
}

impl Visit for ast::StructType {
    fn visit(&self, sess: &mut Sess) {
        self.fields.iter().for_each(|f| f.ty.visit(sess))
    }
}
