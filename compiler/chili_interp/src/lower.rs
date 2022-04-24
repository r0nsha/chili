use crate::{
    instruction::{Bytecode, Instruction},
    interp::{Env, InterpSess},
    value::{FatPtr, Func, Value},
};
use chili_ast::{
    ast,
    pattern::Pattern,
    ty::{InferTy, TyKind},
    workspace::BindingInfoId,
};
use chili_infer::normalize::NormalizeTy;
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};

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
            ast::ExprKind::Fn(func) => func.lower(sess, code),
            ast::ExprKind::While(_) => todo!(),
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break(_) => todo!(),
            ast::ExprKind::Continue(_) => todo!(),
            ast::ExprKind::Return(_) => todo!(),
            ast::ExprKind::If(if_) => if_.lower(sess, code),
            ast::ExprKind::Block(block) => block.lower(sess, code),
            ast::ExprKind::Binary(binary) => binary.lower(sess, code),
            ast::ExprKind::Unary(unary) => unary.lower(sess, code),
            ast::ExprKind::Subscript(_) => todo!(),
            ast::ExprKind::Slice(_) => todo!(),
            ast::ExprKind::Call(call) => call.lower(sess, code),
            ast::ExprKind::MemberAccess(access) => {
                access.expr.lower(sess, code);

                match &access.expr.ty.normalize(sess.tycx).maybe_deref_once() {
                    TyKind::Tuple(_) | TyKind::Infer(_, InferTy::PartialTuple(_)) => {
                        let index = access.member.parse::<usize>().unwrap();
                        code.push(Instruction::Index(index));
                    }
                    TyKind::Struct(st) => {
                        todo!("struct access")
                    }
                    TyKind::Infer(_, InferTy::PartialStruct(partial)) => {
                        todo!("partial struct access")
                    }
                    TyKind::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        sess.push_const(code, Value::Int(*size as i64))
                    }
                    TyKind::Slice(..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        code.push(Instruction::Index(1));
                    }
                    TyKind::Slice(..) if access.member.as_str() == BUILTIN_FIELD_DATA => {
                        code.push(Instruction::Index(0));
                    }
                    TyKind::Module(module_id) => {
                        let id = sess.find_symbol(*module_id, access.member);
                        let slot = find_and_lower_top_level_binding(id, sess);
                        code.push(Instruction::GetGlobal(slot));
                    }
                    ty => panic!("invalid type `{}`", ty),
                }

                code.push(Instruction::Access(access.member));
            }
            ast::ExprKind::Ident(ident) => {
                let id = ident.binding_info_id;

                assert!(id != BindingInfoId::unknown(), "{}", ident.symbol);

                match self.ty.normalize(sess.tycx) {
                    // Note (Ron): We do nothing with modules, since they are not an actual value
                    TyKind::Module(_) => (),
                    _ => {
                        if let Some(slot) = sess.env().value(id) {
                            code.push(Instruction::GetLocal(*slot))
                        } else {
                            if let Some(slot) = sess.get_global(id) {
                                code.push(Instruction::GetGlobal(slot))
                            } else {
                                let slot = find_and_lower_top_level_binding(id, sess);
                                code.push(Instruction::GetGlobal(slot))
                            }
                        }
                    }
                }
            }
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral(_) => todo!(),
            ast::ExprKind::Literal(lit) => sess.push_const(
                code,
                match &lit.kind {
                    ast::LiteralKind::Unit => Value::Tuple(vec![]),
                    ast::LiteralKind::Nil => todo!("nil"),
                    ast::LiteralKind::Bool(v) => Value::Bool(*v),
                    ast::LiteralKind::Int(v) => Value::Int(*v),
                    ast::LiteralKind::Float(v) => Value::Float(*v),
                    ast::LiteralKind::Str(v) => Value::Slice(FatPtr {
                        ty: self.ty.normalize(&sess.tycx),
                        ptr: v.to_string().as_mut_ptr(),
                        len: v.len(),
                    }),
                    ast::LiteralKind::Char(v) => Value::Int((*v) as i64),
                },
            ),
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

impl Lower for ast::Fn {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        if let Some(id) = self.binding_info_id {
            let binding_info = sess.workspace.get_binding_info(id).unwrap();
            if binding_info.scope_level.is_global() {
                sess.insert_global(id, Value::unit());
            }
        }

        sess.env_mut().push_scope();

        let mut offset: isize = -1;
        for param in self.sig.params.iter() {
            match &param.pattern {
                Pattern::Single(pat) => {
                    sess.env_mut().insert(pat.binding_info_id, offset);
                    offset -= 1;
                }
                Pattern::StructUnpack(_) => {
                    todo!("struct unpack")
                }
                Pattern::TupleUnpack(_) => {
                    todo!("tuple unpack")
                }
            }
        }

        let mut func_code = Bytecode::new();

        self.body.lower(sess, &mut func_code);

        if !code.ends_with(&[Instruction::Return]) {
            func_code.push(Instruction::Return)
        }

        sess.env_mut().pop_scope();

        let func = Value::Func(Func {
            name: self.sig.name.to_string(),
            param_count: self.sig.params.len(),
            code: func_code,
        });

        sess.push_const(code, func);
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

impl Lower for ast::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        self.cond.lower(sess, code);

        let then_jmp = push_empty_jmpf(code);
        code.push(Instruction::Pop);

        self.then.lower(sess, code);

        let else_jmp = push_empty_jmp(code);
        patch_empty_jmp(code, then_jmp);
        code.push(Instruction::Pop);

        if let Some(otherwise) = &self.otherwise {
            otherwise.lower(sess, code);
        }

        patch_empty_jmp(code, else_jmp);
    }
}

impl Lower for ast::Block {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode) {
        sess.env_mut().push_scope();

        for expr in self.exprs.iter() {
            expr.lower(sess, code);
        }

        // TODO: interpret defers. Note: this complicates block's value yielding
        // for expr in self.deferred.iter() {
        //     expr.lower(sess, code);
        // }

        sess.env_mut().pop_scope();
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

fn push_empty_jmpf(code: &mut Bytecode) -> usize {
    code.push(Instruction::Jmpf(0xffff));
    code.len() - 1
}

fn push_empty_jmp(code: &mut Bytecode) -> usize {
    code.push(Instruction::Jmp(0xffff));
    code.len() - 1
}

fn patch_empty_jmp(code: &mut Bytecode, pos: usize) -> isize {
    let target_offset = (code.len() - 1 - pos) as isize;

    match &mut code[pos] {
        Instruction::Jmp(offset) | Instruction::Jmpt(offset) | Instruction::Jmpf(offset) => {
            *offset = target_offset
        }
        _ => panic!("instruction at address {} is not a jump", pos),
    };

    target_offset
}

#[inline]
fn find_and_lower_top_level_binding(id: BindingInfoId, sess: &mut InterpSess) -> usize {
    if let Some(binding) = sess.typed_ast.bindings.get(&id) {
        lower_top_level_binding(binding, sess)
    } else {
        // dbg!(sess.workspace.get_binding_info(id).unwrap());
        panic!("binding not found!")
    }
}

#[inline]
fn lower_top_level_binding(binding: &ast::Binding, sess: &mut InterpSess) -> usize {
    // TODO: this function is incomplete
    // TODO: it implies that only global bindings resulting in a constant value works

    sess.env_stack.push(Env::default());

    binding
        .expr
        .as_ref()
        .unwrap()
        .lower(sess, &mut Bytecode::new());

    sess.env_stack.pop();

    let id = binding.pattern.as_single_ref().binding_info_id;
    assert!(id != BindingInfoId::unknown());

    match sess.interp.constants.pop() {
        Some(value) => sess.insert_global(id, value),
        None => panic!("top level binding doesn't have a defined constant value"),
    }
}
