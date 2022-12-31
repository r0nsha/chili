use super::{
    interp::{Env, InterpSess, LoopEnv},
    vm::{
        byte_seq::{ByteSeq, PutValue},
        bytecode::{Bytecode, Inst},
        value::{Buffer, ExternFunction, ExternVariable, Function, IntrinsicFunction, Value},
    },
    IS_64BIT, WORD_SIZE,
};
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir::{
        self,
        const_value::{ConstExternVariable, ConstValue},
    },
    infer::normalize::Normalize,
    interp::vm::value::FunctionAddress,
    types::{offset_of::OffsetOf, size_of::SizeOf, FloatType, InferType, IntType, Type, TypeId, UintType},
    workspace::{BindingId, BindingInfoKind},
};
use byteorder::{NativeEndian, WriteBytesExt};
use ustr::ustr;

#[derive(Clone, Copy)]
pub struct LowerContext {
    pub take_ptr: bool,
}

pub trait Lower {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext);
}

impl Lower for hir::Node {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        match self {
            hir::Node::Const(x) => x.lower(sess, code, ctx),
            hir::Node::Binding(x) => x.lower(sess, code, ctx),
            hir::Node::Id(x) => x.lower(sess, code, ctx),
            hir::Node::Assign(x) => x.lower(sess, code, ctx),
            hir::Node::MemberAccess(x) => x.lower(sess, code, ctx),
            hir::Node::Call(x) => x.lower(sess, code, ctx),
            hir::Node::Cast(x) => x.lower(sess, code, ctx),
            hir::Node::Sequence(x) => x.lower(sess, code, ctx),
            hir::Node::Control(x) => x.lower(sess, code, ctx),
            hir::Node::Builtin(x) => x.lower(sess, code, ctx),
            hir::Node::Literal(x) => x.lower(sess, code, ctx),
        }
    }
}

impl Lower for hir::Const {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let value = const_value_to_value(&self.value, self.ty, sess);
        sess.push_const(code, value);
    }
}

impl Lower for hir::Function {
    fn lower(&self, sess: &mut InterpSess, _code: &mut Bytecode, _ctx: LowerContext) {
        if sess.interp.functions.contains_key(&self.id) || !sess.lowered_functions.insert(self.id) {
            return;
        }

        let function_type = self.ty.normalize(sess.tcx).into_function();

        match &self.kind {
            hir::FunctionKind::Orphan { params, body, .. } => {
                sess.env_mut().push_scope();

                let mut function_code = Bytecode::new();

                for (index, param) in params.iter().enumerate() {
                    let offset = -(params.len() as i16) + index as i16;
                    sess.env_mut().insert(param.id, offset);
                }

                body.as_ref()
                    .unwrap()
                    .lower(sess, &mut function_code, LowerContext { take_ptr: false });

                function_code.write_inst(Inst::Return);

                sess.env_mut().pop_scope();

                sess.interp.functions.insert(
                    self.id,
                    Function {
                        id: self.id,
                        name: self.qualified_name,
                        ty: function_type,
                        code: function_code,
                    },
                );
            }
            hir::FunctionKind::Extern { dylib, link_name, .. } => {
                let lib_path = dylib.as_ref().map_or_else(
                    || {
                        sess.diagnostics.push(
                            Diagnostic::error()
                                .with_message(format!(
                                    "must specify a library to use extern function `{}` at compile-time",
                                    self.name
                                ))
                                .with_label(Label::primary(self.span, "cannot use during compile-time"))
                                .with_note("add #[lib = \"your_lib\"] above the declaration"),
                        );

                        ustr("")
                    },
                    |lib| ustr(&lib.path()),
                );

                sess.interp.extern_functions.insert(
                    self.id,
                    ExternFunction {
                        lib_path,
                        name: *link_name,
                        ty: function_type,
                    },
                );
            }
            hir::FunctionKind::Intrinsic(_) => {
                // Noop
            }
        }
    }
}

impl Lower for hir::Binding {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        if let Some(ConstValue::ExternVariable(ConstExternVariable { lib: None, .. })) = self.value.as_const_value() {
            sess.diagnostics.push(
                Diagnostic::error()
                    .with_message(format!(
                        "must specify a library to use extern variable `{}` at compile-time",
                        self.name
                    ))
                    .with_label(Label::primary(self.span, "cannot use during compile-time"))
                    .with_note("add #[lib = \"your_lib\"] above the declaration"),
            );

            return;
        }

        let binding_info = sess.workspace.binding_infos.get(self.id).unwrap();

        match &binding_info.kind {
            BindingInfoKind::LetStatic => {
                lower_static_binding(self, sess);
            }
            _ => {
                self.value.lower(sess, code, LowerContext { take_ptr: false });

                sess.add_local(code, self.id);
                let last_local = code.locals as i32 - 1;
                code.write_inst(Inst::StoreLocal(last_local));
            }
        }

        sess.push_const_unit(code);
    }
}

impl Lower for hir::Id {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        match self.ty.normalize(sess.tcx) {
            Type::Module(_) => {
                // Note (Ron): We do nothing with modules, since they are not an actual value
            }
            _ => {
                if let Some(slot) = sess.env().value(self.id) {
                    let slot = *slot as i32;
                    code.write_inst(if ctx.take_ptr {
                        Inst::PeekPtr(slot)
                    } else {
                        Inst::Peek(slot)
                    });
                } else {
                    let slot = sess
                        .get_global(self.id)
                        .unwrap_or_else(|| find_and_lower_top_level_binding(self.id, sess))
                        as u32;

                    code.write_inst(if ctx.take_ptr {
                        Inst::LoadGlobalPtr(slot)
                    } else {
                        Inst::LoadGlobal(slot)
                    });
                }
            }
        }
    }
}

impl Lower for hir::Assign {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        self.rhs.lower(sess, code, LowerContext { take_ptr: false });
        self.lhs.lower(sess, code, LowerContext { take_ptr: true });
        code.write_inst(Inst::Assign);
        sess.push_const_unit(code);
    }
}

impl Lower for hir::MemberAccess {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        self.value.lower(sess, code, ctx);
        code.write_inst(if ctx.take_ptr {
            Inst::ConstIndexPtr(self.member_index)
        } else {
            Inst::ConstIndex(self.member_index)
        });
    }
}

impl Lower for hir::Call {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        for arg in self.args.iter() {
            arg.lower(sess, code, LowerContext { take_ptr: false });
        }

        self.callee.lower(sess, code, LowerContext { take_ptr: false });

        code.write_inst(Inst::Call(self.args.len() as u32));
    }
}

impl Lower for hir::Cast {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let target_type = self.ty.normalize(sess.tcx);

        match target_type {
            Type::Never | Type::Unit | Type::Bool => {
                self.value.lower(sess, code, LowerContext { take_ptr: false });
            }
            Type::Pointer(_, _) => {
                self.value.lower(sess, code, LowerContext { take_ptr: false });
                sess.push_const(code, Value::Type(target_type));
                code.write_inst(Inst::Cast);
            }
            _ => {
                self.value.lower(sess, code, LowerContext { take_ptr: false });
                sess.push_const(code, Value::Type(target_type));
                code.write_inst(Inst::Cast);
            }
        }
    }
}

impl Lower for hir::Sequence {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        if self.is_scope {
            sess.env_mut().push_scope();
        }

        for (index, expr) in self.statements.iter().enumerate() {
            let is_last = index == self.statements.len() - 1;

            expr.lower(sess, code, if is_last { ctx } else { LowerContext { take_ptr: false } });

            if !is_last {
                code.write_inst(Inst::Pop);
            }
        }

        if self.is_scope {
            sess.env_mut().pop_scope();
        }
    }
}

impl Lower for hir::Control {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        match self {
            hir::Control::If(x) => x.lower(sess, code, ctx),
            hir::Control::While(x) => x.lower(sess, code, ctx),
            hir::Control::Return(x) => x.lower(sess, code, ctx),
            hir::Control::Break(_) => {
                let pos = code.write_inst(Inst::Jmp(INVALID_JMP_OFFSET));
                sess.loop_env_stack.last_mut().unwrap().break_offsets.push(pos);
            }
            hir::Control::Continue(_) => {
                let pos = code.write_inst(Inst::Jmp(INVALID_JMP_OFFSET));
                sess.loop_env_stack.last_mut().unwrap().break_offsets.push(pos);
            }
        }
    }
}

impl Lower for hir::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        lower_conditional(
            sess,
            code,
            |sess, code| self.condition.lower(sess, code, LowerContext { take_ptr: false }),
            |sess, code| self.then.lower(sess, code, LowerContext { take_ptr: false }),
            |sess, code| {
                if let Some(otherwise) = &self.otherwise {
                    otherwise.lower(sess, code, LowerContext { take_ptr: false });
                } else {
                    sess.push_const_unit(code);
                }
            },
        );
    }
}

fn lower_conditional(
    sess: &mut InterpSess,
    code: &mut Bytecode,
    condition: impl FnOnce(&mut InterpSess, &mut Bytecode),
    then: impl FnOnce(&mut InterpSess, &mut Bytecode),
    otherwise: impl FnOnce(&mut InterpSess, &mut Bytecode),
) {
    condition(sess, code);

    let otherwise_jmp = code.write_inst(Inst::Jmpf(INVALID_JMP_OFFSET));

    then(sess, code);

    let exit_jmp = code.write_inst(Inst::Jmp(INVALID_JMP_OFFSET));

    patch_jmp(code, otherwise_jmp);

    otherwise(sess, code);

    patch_jmp(code, exit_jmp);
}

impl Lower for hir::While {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let loop_start = code.len();

        self.condition.lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.write_inst(Inst::Jmpf(INVALID_JMP_OFFSET));

        sess.loop_env_stack.push(LoopEnv::new());

        self.body.lower(sess, code, LowerContext { take_ptr: false });

        let loop_env = sess.loop_env_stack.pop().unwrap();

        code.write_inst(Inst::Pop);

        let offset = code.len() - loop_start;
        code.write_inst(Inst::Jmp(-(offset as i32)));

        patch_jmp(code, exit_jmp);

        // patch all break/continue jmp instructions
        let len = code.len();
        for pos in &loop_env.break_offsets {
            let target_offset = (len - *pos) as i32;
            (&mut code.as_mut_slice()[*pos + 1..])
                .write_i32::<NativeEndian>(target_offset)
                .unwrap();
        }

        for pos in &loop_env.continue_offsets {
            let target_offset = loop_start as i32 - *pos as i32;
            (&mut code.as_mut_slice()[*pos + 1..])
                .write_i32::<NativeEndian>(target_offset)
                .unwrap();
        }

        sess.push_const_unit(code);
    }
}

impl Lower for hir::Return {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        self.value.lower(sess, code, LowerContext { take_ptr: false });
        code.write_inst(Inst::Return);
    }
}

impl Lower for hir::Builtin {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        match self {
            hir::Builtin::Add(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Add);
            }
            hir::Builtin::Sub(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Sub);
            }
            hir::Builtin::Mul(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Mul);
            }
            hir::Builtin::Div(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Div);
            }
            hir::Builtin::Rem(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Rem);
            }
            hir::Builtin::Shl(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Shl);
            }
            hir::Builtin::Shr(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Shr);
            }
            hir::Builtin::And(binary) => {
                lower_conditional(
                    sess,
                    code,
                    |sess, code| {
                        binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                    },
                    |sess, code| {
                        binary.rhs.lower(sess, code, LowerContext { take_ptr: false });
                    },
                    |sess, code| {
                        sess.push_const(code, Value::Bool(false));
                    },
                );
            }
            hir::Builtin::Or(binary) => {
                lower_conditional(
                    sess,
                    code,
                    |sess, code| {
                        binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                    },
                    |sess, code| {
                        sess.push_const(code, Value::Bool(true));
                    },
                    |sess, code| {
                        binary.rhs.lower(sess, code, LowerContext { take_ptr: false });
                    },
                );
            }
            hir::Builtin::Lt(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Lt);
            }
            hir::Builtin::Le(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Le);
            }
            hir::Builtin::Gt(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Gt);
            }
            hir::Builtin::Ge(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Ge);
            }
            hir::Builtin::Eq(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Eq);
            }
            hir::Builtin::Ne(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Ne);
            }
            hir::Builtin::BitAnd(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::And);
            }
            hir::Builtin::BitOr(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Or);
            }
            hir::Builtin::BitXor(binary) => {
                binary.lhs.lower(sess, code, LowerContext { take_ptr: false });
                binary.rhs.lower(sess, code, LowerContext { take_ptr: false });

                code.write_inst(Inst::Xor);
            }
            hir::Builtin::Not(unary) => {
                unary.value.lower(sess, code, LowerContext { take_ptr: false });
                code.write_inst(Inst::Not);
            }
            hir::Builtin::Neg(unary) => {
                unary.value.lower(sess, code, LowerContext { take_ptr: false });
                code.write_inst(Inst::Neg);
            }
            hir::Builtin::Ref(unary) => {
                unary.value.lower(sess, code, LowerContext { take_ptr: true });
            }
            hir::Builtin::Deref(unary) => {
                unary.value.lower(sess, code, LowerContext { take_ptr: false });
                code.write_inst(Inst::Deref);
            }
            hir::Builtin::Offset(offset) => {
                offset.value.lower(sess, code, LowerContext { take_ptr: false });

                let value_type = offset.value.ty().normalize(sess.tcx);

                let elem_size = match value_type {
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(inner) | Type::Str(inner) => {
                            code.write_inst(Inst::ConstIndex(0));
                            inner.size_of(WORD_SIZE)
                        }
                        _ => inner.size_of(WORD_SIZE),
                    },
                    Type::Array(inner, _) => inner.size_of(WORD_SIZE),
                    _ => unreachable!("{:?}", value_type),
                };

                offset.index.lower(sess, code, LowerContext { take_ptr: false });

                sess.push_const(code, Value::Uint(elem_size));
                code.write_inst(Inst::Mul);

                code.write_inst(Inst::Offset);

                if !ctx.take_ptr {
                    code.write_inst(Inst::Deref);
                }
            }
            hir::Builtin::Slice(slice) => {
                let result_type = Type::Pointer(Box::new(slice.ty.normalize(sess.tcx)), true);
                // Size of a fat pointer
                let result_type_size = WORD_SIZE as u32 * 2;

                let elem_size = match &result_type {
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Array(inner, _) => inner.size_of(WORD_SIZE),
                        Type::Slice(inner) | Type::Str(inner) => inner.size_of(WORD_SIZE),
                        _ => inner.size_of(WORD_SIZE),
                    },
                    Type::Array(inner, _) => inner.size_of(WORD_SIZE),
                    Type::Slice(inner) | Type::Str(inner) => inner.size_of(WORD_SIZE),
                    _ => unreachable!("{:?}", result_type),
                };

                match slice.value.ty().normalize(sess.tcx).into_inner() {
                    Type::Slice(_) | Type::Str(_) => {
                        slice.value.lower(sess, code, LowerContext { take_ptr: false });

                        code.write_inst(Inst::Copy(0));
                        code.write_inst(Inst::ConstIndex(1));
                        code.write_inst(Inst::Swap(1));
                        code.write_inst(Inst::ConstIndex(0));

                        sess.push_const(code, Value::Type(result_type));
                        code.write_inst(Inst::BufferAlloc(result_type_size));

                        code.write_inst(Inst::Swap(1));

                        // calculate the new slice's offset
                        slice.low.lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(elem_size));
                        code.write_inst(Inst::Mul);
                        code.write_inst(Inst::Offset);

                        code.write_inst(Inst::BufferPut(0));

                        // calculate the slice length, by doing `high - low`
                        slice.high.lower(sess, code, LowerContext { take_ptr: false });
                        slice.low.lower(sess, code, LowerContext { take_ptr: false });
                        code.write_inst(Inst::Sub);

                        code.write_inst(Inst::BufferPut(WORD_SIZE as u32));
                    }
                    Type::Array(..) => {
                        sess.push_const(code, Value::Type(result_type));
                        code.write_inst(Inst::BufferAlloc(result_type_size));

                        slice.value.lower(sess, code, LowerContext { take_ptr: false });

                        code.write_inst(Inst::Deref);

                        // calculate the new slice's offset
                        slice.low.lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(elem_size));
                        code.write_inst(Inst::Mul);
                        code.write_inst(Inst::Offset);

                        code.write_inst(Inst::BufferPut(0));

                        // calculate the slice length, by doing `high - low`
                        slice.high.lower(sess, code, LowerContext { take_ptr: false });
                        slice.low.lower(sess, code, LowerContext { take_ptr: false });
                        code.write_inst(Inst::Sub);

                        code.write_inst(Inst::BufferPut(WORD_SIZE as u32));
                    }
                    _ => {
                        sess.push_const(code, Value::Type(result_type));
                        code.write_inst(Inst::BufferAlloc(result_type_size));

                        slice.value.lower(sess, code, LowerContext { take_ptr: false });

                        slice.low.lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(elem_size));
                        code.write_inst(Inst::Mul);
                        code.write_inst(Inst::Offset);

                        code.write_inst(Inst::BufferPut(0));

                        slice.high.lower(sess, code, LowerContext { take_ptr: false });
                        slice.low.lower(sess, code, LowerContext { take_ptr: false });
                        code.write_inst(Inst::Sub);

                        code.write_inst(Inst::BufferPut(WORD_SIZE as u32));
                    }
                }
            }
        }
    }
}

impl Lower for hir::Literal {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, ctx: LowerContext) {
        match self {
            hir::Literal::Struct(x) => x.lower(sess, code, ctx),
            hir::Literal::Tuple(x) => x.lower(sess, code, ctx),
            hir::Literal::Array(x) => x.lower(sess, code, ctx),
            hir::Literal::ArrayFill(x) => x.lower(sess, code, ctx),
        }
    }
}

impl Lower for hir::StructLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let struct_type = ty.as_struct();
        let struct_size = struct_type.size_of(WORD_SIZE) as u32;

        sess.push_const(code, Value::Type(ty.clone()));
        code.write_inst(Inst::BufferAlloc(struct_size));

        let mut ordered_fields = self.fields.clone();

        ordered_fields.sort_by(|f1, f2| {
            let index_1 = struct_type.field_position(f1.name).unwrap();
            let index_2 = struct_type.field_position(f2.name).unwrap();
            index_1.cmp(&index_2)
        });

        for (index, field) in ordered_fields.iter().enumerate() {
            field.value.lower(sess, code, LowerContext { take_ptr: false });

            code.write_inst(Inst::BufferPut(struct_type.offset_of(index, WORD_SIZE) as u32));
        }
    }
}

impl Lower for hir::TupleLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let tuple_type = self.ty.normalize(sess.tcx);
        let tuple_size = tuple_type.size_of(WORD_SIZE) as u32;

        sess.push_const(code, Value::Type(tuple_type.clone()));
        code.write_inst(Inst::BufferAlloc(tuple_size));

        for (index, element) in self.elements.iter().enumerate() {
            element.lower(sess, code, LowerContext { take_ptr: false });
            code.write_inst(Inst::BufferPut(tuple_type.offset_of(index, WORD_SIZE) as u32));
        }
    }
}

impl Lower for hir::ArrayLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let inner_ty_size = ty.element_type().unwrap().size_of(WORD_SIZE);

        sess.push_const(code, Value::Type(ty));
        code.write_inst(Inst::BufferAlloc((self.elements.len() * inner_ty_size) as u32));

        for (index, element) in self.elements.iter().enumerate() {
            element.lower(sess, code, LowerContext { take_ptr: false });
            code.write_inst(Inst::BufferPut((index * inner_ty_size) as u32));
        }
    }
}

impl Lower for hir::ArrayFillLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut Bytecode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let inner_ty_size = ty.element_type().unwrap().size_of(WORD_SIZE);

        let size = if let Type::Array(_, size) = ty { size } else { panic!() };

        sess.push_const(code, Value::Type(ty));
        code.write_inst(Inst::BufferAlloc((size * inner_ty_size) as u32));

        self.value.lower(sess, code, LowerContext { take_ptr: false });

        code.write_inst(Inst::BufferFill(size as u32));
    }
}

fn const_value_to_value(const_value: &ConstValue, ty: TypeId, sess: &mut InterpSess) -> Value {
    let ty = ty.normalize(sess.tcx);

    match const_value {
        ConstValue::Unit(_) => Value::unit(),
        ConstValue::Type(ty) => Value::Type(ty.normalize(sess.tcx)),
        ConstValue::Bool(v) => Value::Bool(*v),
        ConstValue::Int(v) => match ty {
            Type::Int(int_ty) => match int_ty {
                IntType::I8 => Value::I8(*v as _),
                IntType::I16 => Value::I16(*v as _),
                IntType::I32 => Value::I32(*v as _),
                IntType::I64 => Value::I64(*v as _),
                IntType::Int => Value::Int(*v as _),
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Value::U8(*v as _),
                UintType::U16 => Value::U16(*v as _),
                UintType::U32 => Value::U32(*v as _),
                UintType::U64 => Value::U64(*v as _),
                UintType::Uint => Value::Uint(*v as _),
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Value::F32(*v as _),
                FloatType::F64 => Value::F64(*v as _),
                FloatType::Float => {
                    if IS_64BIT {
                        Value::F64(*v as _)
                    } else {
                        Value::F32(*v as _)
                    }
                }
            },
            Type::Infer(_, InferType::AnyInt) => Value::Int(*v as _),
            Type::Infer(_, InferType::AnyFloat) => {
                if IS_64BIT {
                    Value::F64(*v as _)
                } else {
                    Value::F32(*v as _)
                }
            }
            _ => panic!("invalid ty {:?}", ty),
        },
        ConstValue::Float(v) => match ty {
            Type::Float(float_ty) => match float_ty {
                FloatType::F16 | FloatType::F32 => Value::F32(*v as _),
                FloatType::F64 => Value::F64(*v as _),
                FloatType::Float => {
                    if IS_64BIT {
                        Value::F64(*v as _)
                    } else {
                        Value::F32(*v as _)
                    }
                }
            },
            Type::Infer(_, InferType::AnyFloat) => {
                if IS_64BIT {
                    Value::F64(*v as _)
                } else {
                    Value::F32(*v as _)
                }
            }
            _ => panic!("invalid ty {:?}", ty),
        },
        ConstValue::Str(str) => Value::Buffer(Buffer::from_ustr(*str)),
        ConstValue::Tuple(elems) => Value::Buffer(Buffer::from_values(
            elems
                .iter()
                .map(|elem| const_value_to_value(&elem.value, elem.ty, sess)),
            ty,
        )),
        ConstValue::Struct(fields) => Value::Buffer(Buffer::from_values(
            fields
                .iter()
                .map(|(_, elem)| const_value_to_value(&elem.value, elem.ty, sess)),
            ty,
        )),
        ConstValue::Array(array) => {
            let array_len = array.values.len();

            let elem_type = array.element_type;
            let elem_type_kind = elem_type.normalize(sess.tcx);
            let elem_size = elem_type_kind.size_of(WORD_SIZE);

            let mut bytes = ByteSeq::new(array_len * elem_size);

            for (index, const_value) in array.values.iter().enumerate() {
                let value = const_value_to_value(const_value, elem_type, sess);
                bytes.offset_mut(index * elem_size).put_value(&value);
            }

            Value::Buffer(Buffer {
                bytes,
                ty: Type::Array(Box::new(elem_type_kind), array_len),
            })
        }
        ConstValue::Function(f) => {
            let function = sess.cache.functions.get(f.id).unwrap();

            match &function.kind {
                hir::FunctionKind::Intrinsic(intrinsic) => Value::Intrinsic(IntrinsicFunction::from(*intrinsic)),
                _ => {
                    function.lower(sess, &mut Bytecode::new(), LowerContext { take_ptr: false });
                    Value::Function(FunctionAddress {
                        id: f.id,
                        is_extern: function.kind.as_extern().is_some(),
                        name: f.name,
                    })
                }
            }
        }
        ConstValue::ExternVariable(variable) => Value::ExternVariable(ExternVariable {
            name: variable.name,
            lib: variable.dylib.clone().unwrap(),
            ty: variable.ty.normalize(sess.tcx),
        }),
    }
}

fn find_and_lower_top_level_binding(id: BindingId, sess: &mut InterpSess) -> usize {
    let binding = sess
        .cache
        .bindings
        .get(&id)
        .unwrap_or_else(|| panic!("binding not found: {:?}", id));

    lower_top_level_binding(binding, sess)
}

fn lower_top_level_binding(binding: &hir::Binding, sess: &mut InterpSess) -> usize {
    if let Some(const_value) = binding.value.as_const_value() {
        let value = const_value_to_value(const_value, binding.value.ty(), sess);
        sess.insert_global(binding.id, value)
    } else {
        lower_static_binding(binding, sess)
    }
}

fn lower_static_binding(binding: &hir::Binding, sess: &mut InterpSess) -> usize {
    let mut code = Bytecode::new();

    sess.env_stack.push((binding.module_id, Env::default()));

    binding.value.lower(sess, &mut code, LowerContext { take_ptr: false });

    sess.env_stack.pop();

    let slot = sess.insert_global(binding.id, Value::default());

    code.write_inst(Inst::StoreGlobal(slot as u32));
    sess.push_const_unit(&mut code);
    code.write_inst(Inst::Return);

    sess.statically_initialized_globals.push(code);

    slot
}

const INVALID_JMP_OFFSET: i32 = i32::MAX;

fn patch_jmp(code: &mut Bytecode, op_pos: usize) {
    // This function assumes that the Op is some sort of Jmp.
    // We patch the op's operand by directly writing to the buffer.
    let target_offset = (code.len() - op_pos) as i32;

    (&mut code.as_mut_slice()[op_pos + 1..])
        .write_i32::<NativeEndian>(target_offset)
        .unwrap();
}
