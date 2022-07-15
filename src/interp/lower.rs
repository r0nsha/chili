use super::{
    interp::{Env, InterpSess},
    vm::{
        byte_seq::{ByteSeq, PutValue},
        instruction::{CastInstruction, CompiledCode, Instruction},
        value::{
            Buffer, ExternFunction, ExternVariable, Function, IntrinsicFunction, Value, ValueKind,
        },
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
    types::{align::AlignOf, size::SizeOf, FloatType, InferType, IntType, Type, TypeId, UintType},
    workspace::BindingId,
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

                if !function_code.instructions.ends_with(&[Instruction::Return]) {
                    function_code.push(Instruction::Return);
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
            )
        } else {
            self.value
                .lower(sess, code, LowerContext { take_ptr: false });

            sess.add_local(code, self.id);
            code.push(Instruction::SetLocal(code.last_local()));

            sess.push_const_unit(code);
        }
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
                        Instruction::PeekPtr(slot)
                    } else {
                        Instruction::Peek(slot)
                    });
                } else {
                    let slot = sess
                        .get_global(self.id)
                        .unwrap_or_else(|| find_and_lower_top_level_binding(self.id, sess))
                        as u32;

                    code.push(if ctx.take_ptr {
                        Instruction::GetGlobalPtr(slot)
                    } else {
                        Instruction::GetGlobal(slot)
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
        code.push(Instruction::Assign);
        sess.push_const_unit(code);
    }
}

impl Lower for hir::MemberAccess {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.value.lower(sess, code, ctx);
        code.push(if ctx.take_ptr {
            Instruction::ConstIndexPtr(self.member_index)
        } else {
            Instruction::ConstIndex(self.member_index)
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

        code.push(Instruction::Call(self.args.len() as u32));
    }
}

impl Lower for hir::Cast {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.value
            .lower(sess, code, LowerContext { take_ptr: false });

        match self.ty.normalize(sess.tcx) {
            Type::Never | Type::Unit | Type::Bool => (),
            Type::Int(ty) => {
                code.push(match ty {
                    IntType::I8 => Instruction::Cast(CastInstruction::I8),
                    IntType::I16 => Instruction::Cast(CastInstruction::I16),
                    IntType::I32 => Instruction::Cast(CastInstruction::I32),
                    IntType::I64 => Instruction::Cast(CastInstruction::I64),
                    IntType::Int => Instruction::Cast(CastInstruction::Int),
                });
            }
            Type::Uint(ty) => {
                code.push(match ty {
                    UintType::U8 => Instruction::Cast(CastInstruction::U8),
                    UintType::U16 => Instruction::Cast(CastInstruction::U16),
                    UintType::U32 => Instruction::Cast(CastInstruction::U32),
                    UintType::U64 => Instruction::Cast(CastInstruction::U64),
                    UintType::Uint => Instruction::Cast(CastInstruction::Uint),
                });
            }
            Type::Float(ty) => {
                code.push(match ty {
                    FloatType::F16 | FloatType::F32 => Instruction::Cast(CastInstruction::F32),
                    FloatType::F64 => Instruction::Cast(CastInstruction::F64),
                    FloatType::Float => Instruction::Cast(if IS_64BIT {
                        CastInstruction::F64
                    } else {
                        CastInstruction::F32
                    }),
                });
            }
            Type::Pointer(ty, _) => {
                let cast_inst = CastInstruction::Ptr(ValueKind::from(ty.as_ref()));
                code.push(Instruction::Cast(cast_inst));
            }
            Type::Slice(_, _) => {
                let value_type = self.value.ty().normalize(sess.tcx);
                let value_type_size = value_type.size_of(WORD_SIZE) as u32;
                let inner_type_size = value_type.inner().size_of(WORD_SIZE);

                code.push(Instruction::BufferAlloc(value_type_size));

                self.value
                    .lower(sess, code, LowerContext { take_ptr: true });

                // calculate the new slice's offset
                sess.push_const(code, Value::Uint(0));
                sess.push_const(code, Value::Uint(inner_type_size));
                code.push(Instruction::Mul);
                code.push(Instruction::Offset);

                code.push(Instruction::BufferPut(0));

                // calculate the slice length, by doing `high - low`
                match value_type.maybe_deref_once() {
                    Type::Array(_, len) => {
                        sess.push_const(code, Value::Uint(len));
                    }
                    ty => unreachable!("unexpected type `{}`", ty),
                }

                sess.push_const(code, Value::Uint(0));
                code.push(Instruction::Sub);

                code.push(Instruction::BufferPut(WORD_SIZE as u32));
            }
            Type::Infer(_, InferType::AnyInt) => {
                code.push(Instruction::Cast(CastInstruction::Int));
            }
            Type::Infer(_, InferType::AnyFloat) => {
                code.push(Instruction::Cast(if IS_64BIT {
                    CastInstruction::F64
                } else {
                    CastInstruction::F32
                }));
            }
            ty => panic!("invalid ty {}", ty),
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
                code.push(Instruction::Pop);
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
                code.push(Instruction::Jmp(BREAK_DUMMY_JMP_OFFSET));
            }
            hir::Control::Continue(_) => {
                code.push(Instruction::Jmp(CONTINUE_DUMMY_JMP_OFFSET));
            }
        }
    }
}

impl Lower for hir::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.condition
            .lower(sess, code, LowerContext { take_ptr: false });

        let otherwise_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));

        self.then
            .lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.push(Instruction::Jmp(INVALID_JMP_OFFSET));

        patch_jmp(code, otherwise_jmp);

        if let Some(otherwise) = &self.otherwise {
            otherwise.lower(sess, code, LowerContext { take_ptr: false });
        } else {
            sess.push_const_unit(code);
        }

        patch_jmp(code, exit_jmp);
    }
}

impl Lower for hir::While {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let loop_start = code.instructions.len();

        self.condition
            .lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));

        let block_start_pos = code.instructions.len();

        self.body
            .lower(sess, code, LowerContext { take_ptr: false });

        code.push(Instruction::Pop);

        let offset = code.instructions.len() - loop_start;
        code.push(Instruction::Jmp(-(offset as i32)));

        patch_jmp(code, exit_jmp);
        patch_loop_terminators(code, block_start_pos, loop_start);

        sess.push_const_unit(code);
    }
}

impl Lower for hir::Return {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.value
            .lower(sess, code, LowerContext { take_ptr: false });
        code.push(Instruction::Return);
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

                code.push(Instruction::Add);
            }
            hir::Builtin::Sub(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Sub);
            }
            hir::Builtin::Mul(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Mul);
            }
            hir::Builtin::Div(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Div);
            }
            hir::Builtin::Rem(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Rem);
            }
            hir::Builtin::Shl(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Shl);
            }
            hir::Builtin::Shr(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Shr);
            }
            hir::Builtin::And(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::And);
            }
            hir::Builtin::Or(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Or);
            }
            hir::Builtin::Lt(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Lt);
            }
            hir::Builtin::Le(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Le);
            }
            hir::Builtin::Gt(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Gt);
            }
            hir::Builtin::Ge(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Ge);
            }
            hir::Builtin::Eq(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Eq);
            }
            hir::Builtin::Ne(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Ne);
            }
            hir::Builtin::BitAnd(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::And);
            }
            hir::Builtin::BitOr(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Or);
            }
            hir::Builtin::BitXor(binary) => {
                binary
                    .lhs
                    .lower(sess, code, LowerContext { take_ptr: false });
                binary
                    .rhs
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Xor);
            }
            hir::Builtin::Not(unary) => {
                unary
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Not);
            }
            hir::Builtin::Neg(unary) => {
                unary
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Neg);
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

                code.push(Instruction::Deref);
            }
            hir::Builtin::Offset(offset) => {
                offset
                    .value
                    .lower(sess, code, LowerContext { take_ptr: false });

                let value_type = offset.value.ty().normalize(sess.tcx);
                let value_inner_type_size = value_type.inner().size_of(WORD_SIZE);

                match value_type {
                    Type::Slice(..) => {
                        code.push(Instruction::ConstIndex(0));
                    }
                    _ => (),
                }

                offset
                    .index
                    .lower(sess, code, LowerContext { take_ptr: false });

                sess.push_const(code, Value::Uint(value_inner_type_size));

                code.push(Instruction::Mul);

                code.push(if ctx.take_ptr {
                    Instruction::IndexPtr
                } else {
                    Instruction::Index
                });
            }
            hir::Builtin::Slice(slice) => {
                let value_type = slice.value.ty().normalize(sess.tcx);
                let value_type_size = value_type.size_of(WORD_SIZE) as u32;
                let inner_type_size = value_type.inner().size_of(WORD_SIZE);

                match value_type {
                    Type::Array(..) => {
                        code.push(Instruction::BufferAlloc(value_type_size));

                        slice
                            .value
                            .lower(sess, code, LowerContext { take_ptr: true });

                        // calculate the new slice's offset
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(inner_type_size));
                        code.push(Instruction::Mul);
                        code.push(Instruction::Offset);

                        code.push(Instruction::BufferPut(0));

                        // calculate the slice length, by doing `high - low`
                        slice
                            .high
                            .lower(sess, code, LowerContext { take_ptr: false });
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        code.push(Instruction::Sub);

                        code.push(Instruction::BufferPut(WORD_SIZE as u32));
                    }
                    Type::Slice(..) => {
                        slice
                            .value
                            .lower(sess, code, LowerContext { take_ptr: false });

                        code.push(Instruction::Copy(0));
                        code.push(Instruction::ConstIndex(1));
                        code.push(Instruction::Roll(1));
                        code.push(Instruction::ConstIndex(0));

                        code.push(Instruction::BufferAlloc(value_type_size));

                        code.push(Instruction::Roll(1));

                        // calculate the new slice's offset
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(inner_type_size));
                        code.push(Instruction::Mul);
                        code.push(Instruction::Offset);

                        code.push(Instruction::BufferPut(0));

                        // calculate the slice length, by doing `high - low`
                        slice
                            .high
                            .lower(sess, code, LowerContext { take_ptr: false });
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        code.push(Instruction::Sub);

                        code.push(Instruction::BufferPut(WORD_SIZE as u32));
                    }
                    Type::Pointer(..) => {
                        code.push(Instruction::BufferAlloc(value_type_size));

                        slice
                            .value
                            .lower(sess, code, LowerContext { take_ptr: false });

                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        sess.push_const(code, Value::Uint(inner_type_size));
                        code.push(Instruction::Mul);
                        code.push(Instruction::Offset);

                        code.push(Instruction::BufferPut(0));

                        slice
                            .high
                            .lower(sess, code, LowerContext { take_ptr: false });
                        slice
                            .low
                            .lower(sess, code, LowerContext { take_ptr: false });
                        code.push(Instruction::Sub);

                        code.push(Instruction::BufferPut(WORD_SIZE as u32));
                    }
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
        let struct_align = struct_type.align_of(WORD_SIZE) as u32;

        code.push(Instruction::BufferAlloc(struct_size));

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

            code.push(Instruction::BufferPut(index as u32 * struct_align));
        }
    }
}

impl Lower for hir::TupleLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let tuple_size = ty.size_of(WORD_SIZE) as u32;
        let tuple_align = ty.align_of(WORD_SIZE) as u32;

        code.push(Instruction::BufferAlloc(tuple_size));

        for (index, element) in self.elements.iter().enumerate() {
            element.lower(sess, code, LowerContext { take_ptr: false });
            code.push(Instruction::BufferPut(index as u32 * tuple_align));
        }
    }
}

impl Lower for hir::ArrayLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let inner_ty_size = ty.inner().size_of(WORD_SIZE);

        sess.push_const(code, Value::Type(ty));
        code.push(Instruction::BufferAlloc(
            (self.elements.len() * inner_ty_size) as u32,
        ));

        for (index, element) in self.elements.iter().enumerate() {
            element.lower(sess, code, LowerContext { take_ptr: false });
            code.push(Instruction::BufferPut((index * inner_ty_size) as u32));
        }
    }
}

impl Lower for hir::ArrayFillLiteral {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let ty = self.ty.normalize(sess.tcx);
        let inner_ty_size = ty.inner().size_of(WORD_SIZE);

        let size = if let Type::Array(_, size) = ty {
            size
        } else {
            panic!()
        };

        sess.push_const(code, Value::Type(ty));

        code.push(Instruction::BufferAlloc((size * inner_ty_size) as u32));

        self.value
            .lower(sess, code, LowerContext { take_ptr: false });

        code.push(Instruction::BufferFill(size as u32));
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
        ConstValue::Str(v) => Value::from(*v),
        ConstValue::Tuple(elements) => Value::Buffer(Buffer {
            bytes: ByteSeq::from_values(
                &elements
                    .iter()
                    .map(|el| const_value_to_value(&el.value, el.ty, sess))
                    .collect::<Vec<Value>>(),
                ty.size_of(WORD_SIZE),
                ty.align_of(WORD_SIZE),
            ),
            ty,
        }),
        ConstValue::Struct(fields) => Value::Buffer(Buffer {
            bytes: ByteSeq::from_values(
                &fields
                    .iter()
                    .map(|(_, el)| const_value_to_value(&el.value, el.ty, sess))
                    .collect::<Vec<Value>>(),
                ty.size_of(WORD_SIZE),
                ty.align_of(WORD_SIZE),
            ),
            ty,
        }),
        ConstValue::Array(array) => {
            let array_len = array.values.len();

            let el_ty = array.element_ty;
            let el_ty_kind = el_ty.normalize(sess.tcx);
            let el_size = el_ty_kind.size_of(WORD_SIZE);

            let mut bytes = ByteSeq::new(array_len * el_size);

            for (index, const_value) in array.values.iter().enumerate() {
                let value = const_value_to_value(const_value, el_ty, sess);

                bytes.offset_mut(index * el_size).put_value(&value);
            }

            Value::Buffer(Buffer {
                bytes,
                ty: Type::Array(Box::new(el_ty_kind), array_len),
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
    sess.env_stack.push((binding.module_id, Env::default()));

    let mut code = CompiledCode::new();

    binding
        .value
        .lower(sess, &mut code, LowerContext { take_ptr: false });

    sess.env_stack.pop();

    let slot = sess.insert_global(binding.id, Value::unit());
    code.push(Instruction::SetGlobal(slot as u32));

    sess.push_const_unit(&mut code);

    code.push(Instruction::Return);
    sess.evaluated_globals.push(code);

    slot
}

const INVALID_JMP_OFFSET: i32 = i32::MAX;
const BREAK_DUMMY_JMP_OFFSET: i32 = i32::MAX - 1;
const CONTINUE_DUMMY_JMP_OFFSET: i32 = i32::MAX - 2;

fn patch_jmp(code: &mut CompiledCode, inst_pos: usize) {
    let target_offset = (code.instructions.len() - inst_pos) as i32;

    match &mut code.instructions[inst_pos] {
        Instruction::Jmp(offset) | Instruction::Jmpt(offset) | Instruction::Jmpf(offset)
            if *offset == INVALID_JMP_OFFSET =>
        {
            *offset = target_offset
        }
        _ => panic!("instruction at address {} is not a jmp", inst_pos),
    };
}

// patch all break/continue jmp instructions
fn patch_loop_terminators(code: &mut CompiledCode, block_start_pos: usize, continue_pos: usize) {
    let len = code.instructions.len();

    for inst_pos in block_start_pos..len {
        if let Instruction::Jmp(offset) = &mut code.instructions[inst_pos] {
            if *offset == BREAK_DUMMY_JMP_OFFSET {
                *offset = (len - inst_pos) as i32;
            }
            if *offset == CONTINUE_DUMMY_JMP_OFFSET {
                *offset = continue_pos as i32 - inst_pos as i32;
            }
        };
    }
}
