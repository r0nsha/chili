use super::{
    interp::{Env, InterpSess},
    vm::{
        byte_seq::{ByteSeq, PutValue},
        instruction::{CompiledCode, Inst},
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
    types::{
        offset_of::OffsetOf, size_of::SizeOf, FloatType, InferType, IntType, Type, TypeId, UintType,
    },
    workspace::{BindingId, BindingInfoKind},
};
use ustr::ustr;

#[derive(Clone, Copy)]
pub struct LowerContext {
    pub take_ptr: bool,
}

pub trait Lower {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext);
}

impl Lower for hir::Node {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match self {
            hir::Node::Const(x) => x.lower(sess, code, ctx),
            hir::Node::Binding(x) => x.lower(sess, code, ctx),
            hir::Node::Id(x) => x.lower(sess, code, ctx),
            hir::Node::Assignment(x) => x.lower(sess, code, ctx),
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
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let value = const_value_to_value(&self.value, self.ty, sess);
        sess.push_const(code, value);
    }
}

impl Lower for hir::Function {
    fn lower(&self, sess: &mut InterpSess, _code: &mut CompiledCode, _ctx: LowerContext) {
        if sess.interp.functions.contains_key(&self.id) || !sess.lowered_functions.insert(self.id) {
            return;
        }

        let function_type = self.ty.normalize(sess.tcx).into_function();

        match &self.kind {
            hir::FunctionKind::Orphan { params, body, .. } => {
                sess.env_mut().push_scope();

                let mut function_code = CompiledCode::new();

                for index in 0..function_type.params.len() {
                    let offset = -(function_type.params.len() as i16) + index as i16;
                    sess.env_mut().insert(params[index].id, offset);
                }

                body.as_ref().unwrap().lower(
                    sess,
                    &mut function_code,
                    LowerContext { take_ptr: false },
                );

                if !function_code.instructions.ends_with(&[Inst::Return]) {
                    function_code.push(Inst::Return);
                }

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
            hir::FunctionKind::Extern { lib } => {
                let lib_path = lib.as_ref().map_or_else(
                    || {
                        sess.diagnostics.push(
                            Diagnostic::error()
                                .with_message(format!(
                            "must specify `dylib` to use extern function `{}` at compile-time",
                            self.name
                        ))
                                .with_label(Label::primary(
                                    self.span,
                                    "cannot run during compile-time",
                                )),
                        );

                        ustr("")
                    },
                    |lib| ustr(&lib.path()),
                );

                sess.interp.extern_functions.insert(
                    self.id,
                    ExternFunction {
                        lib_path,
                        name: self.qualified_name,
                        param_tys: function_type.params.iter().map(|p| p.ty.clone()).collect(),
                        return_ty: *function_type.return_type,
                        variadic: function_type.varargs.is_some(),
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
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        if let Some(ConstValue::ExternVariable(ConstExternVariable { lib: None, .. })) =
            self.value.as_const_value()
        {
            sess.diagnostics.push(
                Diagnostic::error()
                    .with_message(format!(
                        "must specify `dylib` to use extern variable `{}` at compile-time",
                        self.name
                    ))
                    .with_label(Label::primary(self.span, "cannot use during compile-time")),
            );

            return;
        }

        let binding_info = sess.workspace.binding_infos.get(self.id).unwrap();

        match &binding_info.kind {
            BindingInfoKind::Static => {
                lower_static_binding(self, sess);
            }
            _ => {
                self.value
                    .lower(sess, code, LowerContext { take_ptr: false });

                sess.add_local(code, self.id);
                code.push(Inst::SetLocal(code.last_local()));
            }
        }

        sess.push_const_unit(code);
    }
}

impl Lower for hir::Id {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match self.ty.normalize(sess.tcx) {
            Type::Module(_) => {
                // Note (Ron): We do nothing with modules, since they are not an actual value
            }
            _ => {
                if let Some(slot) = sess.env().value(self.id) {
                    let slot = *slot as i32;
                    code.push(if ctx.take_ptr {
                        Inst::PeekPtr(slot)
                    } else {
                        Inst::Peek(slot)
                    });
                } else {
                    let slot = sess
                        .get_global(self.id)
                        .unwrap_or_else(|| find_and_lower_top_level_binding(self.id, sess))
                        as u32;

                    code.push(if ctx.take_ptr {
                        Inst::GetGlobalPtr(slot)
                    } else {
                        Inst::GetGlobal(slot)
                    });
                }
            }
        }
    }
}

impl Lower for hir::Assignment {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.rhs.lower(sess, code, LowerContext { take_ptr: false });
        self.lhs.lower(sess, code, LowerContext { take_ptr: true });
        code.push(Inst::Assign);
        sess.push_const_unit(code);
    }
}

impl Lower for hir::MemberAccess {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.value.lower(sess, code, ctx);
        code.push(if ctx.take_ptr {
            Inst::ConstIndexPtr(self.member_index)
        } else {
            Inst::ConstIndex(self.member_index)
        });
    }
}

impl Lower for hir::Call {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        for arg in self.args.iter() {
            arg.lower(sess, code, LowerContext { take_ptr: false });
        }

        self.callee
            .lower(sess, code, LowerContext { take_ptr: false });

        code.push(Inst::Call(self.args.len() as u32));
    }
}

impl Lower for hir::Cast {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.value
            .lower(sess, code, LowerContext { take_ptr: false });

        let target_type = self.ty.normalize(sess.tcx);

        match target_type {
            Type::Never | Type::Unit | Type::Bool => (),
            Type::Pointer(ref inner, _) => match inner.as_ref() {
                Type::Slice(_) => {
                    let value_type = self.value.ty().normalize(sess.tcx);
                    let value_type_size = value_type.size_of(WORD_SIZE) as u32;
                    let inner_type_size = value_type.element_type().unwrap().size_of(WORD_SIZE);

                    sess.push_const(code, Value::Type(value_type.clone()));
                    code.push(Inst::BufferAlloc(value_type_size));

                    self.value
                        .lower(sess, code, LowerContext { take_ptr: true });

                    // calculate the new slice's offset
                    sess.push_const(code, Value::Uint(0));
                    sess.push_const(code, Value::Uint(inner_type_size));
                    code.push(Inst::Mul);
                    code.push(Inst::Offset);

                    code.push(Inst::BufferPut(0));

                    // calculate the slice length, by doing `high - low`
                    match value_type.maybe_deref_once() {
                        Type::Array(_, size) => {
                            sess.push_const(code, Value::Uint(size));
                        }
                        ty => unreachable!("unexpected type `{}`", ty),
                    }

                    sess.push_const(code, Value::Uint(0));
                    code.push(Inst::Sub);

                    code.push(Inst::BufferPut(WORD_SIZE as u32));
                }
                _ => {
                    sess.push_const(code, Value::Type(target_type));
                    code.push(Inst::Cast);
                }
            },
            _ => {
                sess.push_const(code, Value::Type(target_type));
                code.push(Inst::Cast);
            }
        }
    }
}

impl Lower for hir::Sequence {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        if self.is_block {
            sess.env_mut().push_scope();
        }

        for (index, expr) in self.statements.iter().enumerate() {
            let is_last = index == self.statements.len() - 1;

            expr.lower(
                sess,
                code,
                if is_last {
                    ctx
                } else {
                    LowerContext { take_ptr: false }
                },
            );

            if !is_last {
                code.push(Inst::Pop);
            }
        }

        if self.is_block {
            sess.env_mut().pop_scope();
        }
    }
}

impl Lower for hir::Control {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match self {
            hir::Control::If(x) => x.lower(sess, code, ctx),
            hir::Control::While(x) => x.lower(sess, code, ctx),
            hir::Control::Return(x) => x.lower(sess, code, ctx),
            hir::Control::Break(_) => {
                code.push(Inst::Jmp(BREAK_DUMMY_JMP_OFFSET));
            }
            hir::Control::Continue(_) => {
                code.push(Inst::Jmp(CONTINUE_DUMMY_JMP_OFFSET));
            }
        }
    }
}

impl Lower for hir::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        lower_conditional(
            sess,
            code,
            |sess, code| {
                self.condition
                    .lower(sess, code, LowerContext { take_ptr: false })
            },
            |sess, code| {
                self.then
                    .lower(sess, code, LowerContext { take_ptr: false })
            },
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
    code: &mut CompiledCode,
    condition: impl FnOnce(&mut InterpSess, &mut CompiledCode),
    then: impl FnOnce(&mut InterpSess, &mut CompiledCode),
    otherwise: impl FnOnce(&mut InterpSess, &mut CompiledCode),
) {
    condition(sess, code);

    let otherwise_jmp = code.push(Inst::Jmpf(INVALID_JMP_OFFSET));

    then(sess, code);

    let exit_jmp = code.push(Inst::Jmp(INVALID_JMP_OFFSET));

    patch_jmp(code, otherwise_jmp);
    otherwise(sess, code);
    patch_jmp(code, exit_jmp);
}

impl Lower for hir::While {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let loop_start = code.instructions.len();

        self.condition
            .lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.push(Inst::Jmpf(INVALID_JMP_OFFSET));

        let block_start_pos = code.instructions.len();

        self.body
            .lower(sess, code, LowerContext { take_ptr: false });

        code.push(Inst::Pop);

        let offset = code.instructions.len() - loop_start;
        code.push(Inst::Jmp(-(offset as i32)));

        patch_jmp(code, exit_jmp);
        patch_loop_terminators(code, block_start_pos, loop_start);

        sess.push_const_unit(code);
    }
}

impl Lower for hir::Return {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.value
            .lower(sess, code, LowerContext { take_ptr: false });
        code.push(Inst::Return);
    }
}

impl Lower for hir::Builtin {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match self {
            hir::Builtin::Add(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Add);
            }
            hir::Builtin::Sub(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Sub);
            }
            hir::Builtin::Mul(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Mul);
            }
            hir::Builtin::Div(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Div);
            }
            hir::Builtin::Rem(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Rem);
            }
            hir::Builtin::Shl(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Shl);
            }
            hir::Builtin::Shr(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Shr);
            }
            hir::Builtin::And(binary) => {
                lower_conditional(
                    sess,
                    code,
                    |sess, code| {
                        binary
                            .lhs
                            .lower(sess, code, LowerContext { take_ptr: false });
                    },
                    |sess, code| {
                        binary
                            .rhs
                            .lower(sess, code, LowerContext { take_ptr: false });
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
                        binary
                            .lhs
                            .lower(sess, code, LowerContext { take_ptr: false });
                    },
                    |sess, code| {
                        sess.push_const(code, Value::Bool(true));
                    },
                    |sess, code| {
                        binary
                            .rhs
                            .lower(sess, code, LowerContext { take_ptr: false });
                    },
                );
            }
            hir::Builtin::Lt(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Lt);
            }
            hir::Builtin::Le(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Le);
            }
            hir::Builtin::Gt(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Gt);
            }
            hir::Builtin::Ge(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Ge);
            }
            hir::Builtin::Eq(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Eq);
            }
            hir::Builtin::Ne(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Ne);
            }
            hir::Builtin::BitAnd(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::And);
            }
            hir::Builtin::BitOr(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Or);
            }
            hir::Builtin::BitXor(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Xor);
            }
            hir::Builtin::Not(unary) => {
                unary
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Not);
            }
            hir::Builtin::Neg(unary) => {
                unary
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Neg);
            }
            hir::Builtin::Ref(unary) => {
                unary
                    .value
                    .lower(sess, code, LowerContext { take_ptr: true });
            }
            hir::Builtin::Deref(unary) => {
                unary
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Inst::Deref);
            }
            hir::Builtin::Offset(offset) => {
                offset
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                let value_type = offset.value.ty().normalize(sess.tcx);

                let elem_size = match value_type {
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(inner) => {
                            code.push(Inst::ConstIndex(0));
                            inner.size_of(WORD_SIZE)
                        }
                        _ => inner.size_of(WORD_SIZE),
                    },
                    Type::Array(inner, _) => inner.size_of(WORD_SIZE),
                    _ => unreachable!("{}", value_type),
                };

                offset
                    .index
                    .lower(sess, code, LowerContext { take_ptr: false });

                sess.push_const(code, Value::Uint(elem_size));

                code.push(Inst::Mul);

                code.push(if ctx.take_ptr {
                    Inst::IndexPtr
                } else {
                    Inst::Index
                });
            }
            hir::Builtin::Slice(slice) => {
                let value_type = slice.value.ty().normalize(sess.tcx);
                let value_type_size = value_type.size_of(WORD_SIZE) as u32;

                let elem_size = match &value_type {
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(inner) => inner.size_of(WORD_SIZE),
                        _ => inner.size_of(WORD_SIZE),
                    },
                    Type::Array(inner, _) => inner.size_of(WORD_SIZE),
                    _ => unreachable!("{}", value_type),
                };

                match &value_type {
                    Type::Array(..) => {
                        sess.push_const(code, Value::Type(value_type.clone()));
                        code.push(Inst::BufferAlloc(value_type_size));

                        slice
                            .value
                            .lower(sess, code, LowerContext { take_ptr: true });

                        // calculate the new slice's offset
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(elem_size));
                        code.push(Inst::Mul);
                        code.push(Inst::Offset);

                        code.push(Inst::BufferPut(0));

                        // calculate the slice length, by doing `high - low`
                        slice
                            .high
                            .lower(sess, code, LowerContext { take_ptr: false });
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        code.push(Inst::Sub);

                        code.push(Inst::BufferPut(WORD_SIZE as u32));
                    }
                    Type::Pointer(inner, _) => match inner.as_ref() {
                        Type::Slice(_) => {
                            slice
                                .value
                                .lower(sess, code, LowerContext { take_ptr: false });

                            code.push(Inst::Copy(0));
                            code.push(Inst::ConstIndex(1));
                            code.push(Inst::Swap(1));
                            code.push(Inst::ConstIndex(0));

                            sess.push_const(code, Value::Type(value_type.clone()));
                            code.push(Inst::BufferAlloc(value_type_size));

                            code.push(Inst::Swap(1));

                            // calculate the new slice's offset
                            slice
                                .low
                                .lower(sess, code, LowerContext { take_ptr: false });
                            sess.push_const(code, Value::Uint(elem_size));
                            code.push(Inst::Mul);
                            code.push(Inst::Offset);

                            code.push(Inst::BufferPut(0));

                            // calculate the slice length, by doing `high - low`
                            slice
                                .high
                                .lower(sess, code, LowerContext { take_ptr: false });
                            slice
                                .low
                                .lower(sess, code, LowerContext { take_ptr: false });
                            code.push(Inst::Sub);

                            code.push(Inst::BufferPut(WORD_SIZE as u32));
                        }
                        _ => {
                            sess.push_const(code, Value::Type(value_type.clone()));
                            code.push(Inst::BufferAlloc(value_type_size));

                            slice
                                .value
                                .lower(sess, code, LowerContext { take_ptr: false });

                            slice
                                .low
                                .lower(sess, code, LowerContext { take_ptr: false });
                            sess.push_const(code, Value::Uint(elem_size));
                            code.push(Inst::Mul);
                            code.push(Inst::Offset);

                            code.push(Inst::BufferPut(0));

                            slice
                                .high
                                .lower(sess, code, LowerContext { take_ptr: false });
                            slice
                                .low
                                .lower(sess, code, LowerContext { take_ptr: false });
                            code.push(Inst::Sub);

                            code.push(Inst::BufferPut(WORD_SIZE as u32));
                        }
                    },
                    _ => panic!("unexpected type {}", value_type),
                }
            }
        }
    }
}

impl Lower for hir::Literal {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match self {
            hir::Literal::Struct(x) => x.lower(sess, code, ctx),
            hir::Literal::Tuple(x) => x.lower(sess, code, ctx),
            hir::Literal::Array(x) => x.lower(sess, code, ctx),
            hir::Literal::ArrayFill(x) => x.lower(sess, code, ctx),
        }
    }
}

impl Lower for hir::StructLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let struct_type = ty.as_struct();
        let struct_size = struct_type.size_of(WORD_SIZE) as u32;

        sess.push_const(code, Value::Type(ty.clone()));
        code.push(Inst::BufferAlloc(struct_size));

        let mut ordered_fields = self.fields.clone();

        ordered_fields.sort_by(|f1, f2| {
            let index_1 = struct_type.find_field_position(f1.name).unwrap();
            let index_2 = struct_type.find_field_position(f2.name).unwrap();
            index_1.cmp(&index_2)
        });

        for (index, field) in ordered_fields.iter().enumerate() {
            field
                .value
                .lower(sess, code, LowerContext { take_ptr: false });

            code.push(Inst::BufferPut(
                struct_type.offset_of(index, WORD_SIZE) as u32
            ));
        }
    }
}

impl Lower for hir::TupleLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let tuple_type = self.ty.normalize(sess.tcx);
        let tuple_size = tuple_type.size_of(WORD_SIZE) as u32;

        sess.push_const(code, Value::Type(tuple_type.clone()));
        code.push(Inst::BufferAlloc(tuple_size));

        for (index, element) in self.elements.iter().enumerate() {
            element.lower(sess, code, LowerContext { take_ptr: false });
            code.push(Inst::BufferPut(
                tuple_type.offset_of(index, WORD_SIZE) as u32
            ));
        }
    }
}

impl Lower for hir::ArrayLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let inner_ty_size = ty.element_type().unwrap().size_of(WORD_SIZE);

        sess.push_const(code, Value::Type(ty));
        code.push(Inst::BufferAlloc(
            (self.elements.len() * inner_ty_size) as u32,
        ));

        for (index, element) in self.elements.iter().enumerate() {
            element.lower(sess, code, LowerContext { take_ptr: false });
            code.push(Inst::BufferPut((index * inner_ty_size) as u32));
        }
    }
}

impl Lower for hir::ArrayFillLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let inner_ty_size = ty.element_type().unwrap().size_of(WORD_SIZE);

        let size = if let Type::Array(_, size) = ty {
            size
        } else {
            panic!()
        };

        sess.push_const(code, Value::Type(ty));
        code.push(Inst::BufferAlloc((size * inner_ty_size) as u32));

        self.value
            .lower(sess, code, LowerContext { take_ptr: false });

        code.push(Inst::BufferFill(size as u32));
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
            _ => panic!("invalid ty {}", ty),
        },
        ConstValue::Uint(v) => match ty {
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
            _ => panic!("invalid ty {}", ty),
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
            _ => panic!("invalid ty {}", ty),
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

            let elem_type = array.element_ty;
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
                hir::FunctionKind::Intrinsic(intrinsic) => Value::Intrinsic(match intrinsic {
                    hir::Intrinsic::StartWorkspace => IntrinsicFunction::StartWorkspace,
                }),
                _ => {
                    function.lower(
                        sess,
                        &mut CompiledCode::new(),
                        LowerContext { take_ptr: false },
                    );

                    Value::Function(FunctionAddress {
                        id: f.id,
                        name: f.name,
                    })
                }
            }
        }
        ConstValue::ExternVariable(variable) => Value::ExternVariable(ExternVariable {
            name: variable.name,
            lib: variable.lib.clone().unwrap(),
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
    let mut code = CompiledCode::new();

    sess.env_stack.push((binding.module_id, Env::default()));

    binding
        .value
        .lower(sess, &mut code, LowerContext { take_ptr: false });

    sess.env_stack.pop();

    let slot = sess.insert_global(binding.id, Value::default());

    code.push(Inst::SetGlobal(slot as u32));
    sess.push_const_unit(&mut code);
    code.push(Inst::Return);

    sess.statically_initialized_globals.push(code);

    slot
}

const INVALID_JMP_OFFSET: i32 = i32::MAX;
const BREAK_DUMMY_JMP_OFFSET: i32 = i32::MAX - 1;
const CONTINUE_DUMMY_JMP_OFFSET: i32 = i32::MAX - 2;

fn patch_jmp(code: &mut CompiledCode, inst_pos: usize) {
    let target_offset = (code.instructions.len() - inst_pos) as i32;

    match &mut code.instructions[inst_pos] {
        Inst::Jmp(offset) | Inst::Jmpf(offset) if *offset == INVALID_JMP_OFFSET => {
            *offset = target_offset
        }
        _ => panic!("instruction at address {} is not a jmp", inst_pos),
    };
}

// patch all break/continue jmp instructions
fn patch_loop_terminators(code: &mut CompiledCode, block_start_pos: usize, continue_pos: usize) {
    let len = code.instructions.len();

    for inst_pos in block_start_pos..len {
        if let Inst::Jmp(offset) = &mut code.instructions[inst_pos] {
            if *offset == BREAK_DUMMY_JMP_OFFSET {
                *offset = (len - inst_pos) as i32;
            }
            if *offset == CONTINUE_DUMMY_JMP_OFFSET {
                *offset = continue_pos as i32 - inst_pos as i32;
            }
        };
    }
}
