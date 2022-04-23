use crate::{
    instruction::Instruction,
    interp::{Env, InterpSess},
    value::Value,
    vm::Bytecode,
};
use chili_ast::{ast, workspace::BindingInfoId};

pub(crate) trait Lower {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode);
}

impl Lower for ast::Expr {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        match &self.kind {
            ast::ExprKind::Import(_) => todo!(),
            ast::ExprKind::Foreign(_) => todo!(),
            ast::ExprKind::Binding(_) => todo!(),
            ast::ExprKind::Defer(_) => todo!(),
            ast::ExprKind::Assign(_) => todo!(),
            ast::ExprKind::Cast(_) => todo!(),
            ast::ExprKind::Builtin(_) => todo!(),
            ast::ExprKind::Fn(_) => todo!(),
            ast::ExprKind::While(_) => todo!(),
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break(_) => todo!(),
            ast::ExprKind::Continue(_) => todo!(),
            ast::ExprKind::Return(_) => todo!(),
            ast::ExprKind::If(_) => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Binary(binary) => binary.lower(sess, code),
            ast::ExprKind::Unary(unary) => unary.lower(sess, code),
            ast::ExprKind::Subscript(_) => todo!(),
            ast::ExprKind::Slice(_) => todo!(),
            ast::ExprKind::Call(call) => call.lower(sess, code),
            ast::ExprKind::MemberAccess(_) => todo!(),
            ast::ExprKind::Ident(ident) => ident.lower(sess, code),
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral(_) => todo!(),
            ast::ExprKind::Literal(lit) => lit.lower(sess, code),
            ast::ExprKind::PointerType(_) => todo!(),
            ast::ExprKind::MultiPointerType(_) => todo!(),
            ast::ExprKind::ArrayType(_) => todo!(),
            ast::ExprKind::SliceType(_) => todo!(),
            ast::ExprKind::StructType(_) => todo!(),
            ast::ExprKind::FnType(_) => todo!(),
            ast::ExprKind::SelfType => todo!(),
            ast::ExprKind::NeverType => todo!(),
            ast::ExprKind::UnitType => todo!(),
            ast::ExprKind::PlaceholderType => todo!(),
            ast::ExprKind::Error => todo!(),
        }
    }
}

impl Lower for ast::Call {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        for arg in self.args.iter() {
            arg.lower(sess, code);
        }
        self.callee.lower(sess, code);
        code.push(Instruction::Call(self.args.len()));
    }
}

impl Lower for ast::Ident {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        let id = self.binding_info_id;
        assert!(id != BindingInfoId::unknown(), "{}", self.symbol);

        if let Some(slot) = sess.env().value(id) {
            code.push(Instruction::GetLocal(*slot))
        } else {
            if let Some(slot) = sess.get_global(id) {
                code.push(Instruction::GetGlobal(slot))
            } else {
                if let Some(binding) = sess.typed_ast.bindings.get(&id) {
                    let slot = lower_top_level_binding(binding, sess);
                    code.push(Instruction::GetGlobal(slot))
                } else {
                    panic!("binding not found!")
                }
            }
        }
    }
}

fn lower_top_level_binding(binding: &ast::Binding, sess: &mut InterpSess) -> usize {
    sess.env_stack.push(Env::default());
    binding.expr.as_ref().unwrap().lower(sess, &mut vec![]);
    sess.env_stack.pop();

    let id = binding.pattern.as_single_ref().binding_info_id;
    assert!(id != BindingInfoId::unknown());

    match sess.interp.constants.pop() {
        Some(value) => sess.insert_global(id, value),
        None => panic!("top level binding doesn't have a defined constant value"),
    }
}

impl Lower for ast::Binary {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        self.lhs.lower(sess, code);
        self.rhs.lower(sess, code);
        code.push(self.op.into())
    }
}

impl Lower for ast::Unary {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        self.lhs.lower(sess, code);
        code.push(self.op.into())
    }
}

impl Lower for ast::Literal {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        sess.push_const(
            code,
            match &self.kind {
                ast::LiteralKind::Unit => Value::Tuple(vec![]),
                ast::LiteralKind::Nil => todo!("nil"),
                ast::LiteralKind::Bool(v) => Value::Bool(*v),
                ast::LiteralKind::Int(v) => Value::Int(*v),
                ast::LiteralKind::Float(v) => Value::Float(*v),
                ast::LiteralKind::Str(v) => todo!("Value::Slice(str)"),
                ast::LiteralKind::Char(v) => Value::Int((*v) as i64),
            },
        )
    }
}
