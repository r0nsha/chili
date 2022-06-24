use super::{
    interp::{Env, InterpSess},
    vm::{
        byte_seq::{ByteSeq, PutValue},
        instruction::{CastInstruction, CompiledCode, Instruction},
        value::{Aggregate, Array, ExternFunction, Function, IntrinsicFunction, Value, ValueKind},
    },
    IS_64BIT, WORD_SIZE,
};
use crate::common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use crate::infer::normalize::Normalize;
use crate::{
    ast::{
        self,
        const_value::ConstValue,
        pattern::{Pattern, UnpackPattern, UnpackPatternKind},
        ty::{
            align::AlignOf, size::SizeOf, FloatType, FunctionType, FunctionTypeKind, InferTy,
            IntType, StructType, Type, TypeId, UintType,
        },
        workspace::BindingId,
        Intrinsic,
    },
    interp::vm::value::FunctionAddress,
};
use ustr::{ustr, Ustr};

#[derive(Clone, Copy)]
pub struct LowerContext {
    pub take_ptr: bool,
}

pub trait Lower {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext);
}

impl Lower for ast::Ast {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match self {
            ast::Ast::Binding(binding) => {
                lower_local_binding(binding, sess, code);
            }
            ast::Ast::Assignment(assignment) => {
                assignment
                    .rvalue
                    .lower(sess, code, LowerContext { take_ptr: false });

                assignment
                    .lvalue
                    .lower(sess, code, LowerContext { take_ptr: true });

                code.push(Instruction::Assign);

                sess.push_const_unit(code);
            }
            ast::Ast::Cast(cast) => cast.lower(sess, code, ctx),
            ast::Ast::Builtin(builtin) => match &builtin.kind {
                ast::BuiltinKind::SizeOf(expr) => match expr.ty().normalize(sess.tycx) {
                    Type::Type(ty) => {
                        sess.push_const(code, Value::Uint(ty.size_of(WORD_SIZE)));
                    }
                    ty => unreachable!("got {}", ty),
                },
                ast::BuiltinKind::AlignOf(expr) => match expr.ty().normalize(sess.tycx) {
                    Type::Type(ty) => {
                        sess.push_const(code, Value::Uint(ty.align_of(WORD_SIZE)));
                    }
                    ty => unreachable!("got {}", ty),
                },
                ast::BuiltinKind::Panic(expr) => {
                    if let Some(expr) = expr {
                        expr.lower(sess, code, ctx);
                    } else {
                        sess.push_const(code, ustr("").into());
                    }

                    code.push(Instruction::Panic);
                }
                ast::BuiltinKind::Run(expr, _) => expr.lower(sess, code, ctx),
                ast::BuiltinKind::Import(_) => sess.push_const_unit(code),
            },
            ast::Ast::Function(_) => panic!("should've been lower to ConstValue::Function"), // func.lower(sess, code, ctx),
            ast::Ast::While(while_) => while_.lower(sess, code, LowerContext { take_ptr: false }),
            ast::Ast::For(for_) => for_.lower(sess, code, ctx),
            ast::Ast::Break(_) => {
                code.push(Instruction::Jmp(INVALID_BREAK_JMP_OFFSET));
            }
            ast::Ast::Continue(_) => {
                code.push(Instruction::Jmp(INVALID_CONTINUE_JMP_OFFSET));
            }
            ast::Ast::Return(ret) => {
                if let Some(expr) = &ret.expr {
                    expr.lower(sess, code, ctx);
                } else {
                    sess.push_const_unit(code);
                }

                code.push(Instruction::Return);
            }
            ast::Ast::If(if_) => if_.lower(sess, code, ctx),
            ast::Ast::Block(block) => lower_block(block, sess, code, ctx),
            ast::Ast::Binary(binary) => binary.lower(sess, code, ctx),
            ast::Ast::Unary(unary) => unary.lower(sess, code, ctx),
            ast::Ast::Subscript(sub) => sub.lower(sess, code, ctx),
            ast::Ast::Slice(slice) => slice.lower(sess, code, ctx),
            ast::Ast::Call(call) => call.lower(sess, code, ctx),
            ast::Ast::MemberAccess(access) => {
                access.expr.lower(sess, code, ctx);

                match &access.expr.ty().normalize(sess.tycx).maybe_deref_once() {
                    Type::Tuple(_) | Type::Infer(_, InferTy::PartialTuple(_)) => {
                        let index = access.member.parse::<usize>().unwrap();

                        code.push(if ctx.take_ptr {
                            Instruction::ConstIndexPtr(index as u32)
                        } else {
                            Instruction::ConstIndex(index as u32)
                        });
                    }
                    Type::Struct(st) => {
                        let index = st.find_field_position(access.member).unwrap();

                        code.push(if ctx.take_ptr {
                            Instruction::ConstIndexPtr(index as u32)
                        } else {
                            Instruction::ConstIndex(index as u32)
                        });
                    }
                    Type::Infer(_, InferTy::PartialStruct(partial)) => {
                        let index = partial
                            .iter()
                            .position(|(field, _)| *field == access.member)
                            .unwrap();

                        code.push(if ctx.take_ptr {
                            Instruction::ConstIndexPtr(index as u32)
                        } else {
                            Instruction::ConstIndex(index as u32)
                        });
                    }
                    Type::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        code.push(Instruction::Pop);
                        sess.push_const(code, Value::Uint(*size as usize));
                    }
                    Type::Slice(..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        code.push(Instruction::ConstIndex(1));
                    }
                    Type::Slice(..) if access.member.as_str() == BUILTIN_FIELD_DATA => {
                        code.push(Instruction::ConstIndex(0));
                    }
                    Type::Module(module_id) => {
                        let id = sess.find_symbol(*module_id, access.member);
                        let slot = find_and_lower_top_level_binding(id, sess);
                        code.push(Instruction::GetGlobal(slot as u32));
                    }
                    ty => panic!("invalid type `{}` or member `{}`", ty, access.member),
                }
            }
            ast::Ast::Ident(ident) => {
                let id = ident.binding_id;

                assert!(id != BindingId::unknown(), "{}", ident.symbol);

                match ident.ty.normalize(sess.tycx) {
                    // Note (Ron): We do nothing with modules, since they are not an actual value
                    Type::Module(_) => (),
                    _ => {
                        if let Some(slot) = sess.env().value(id) {
                            let slot = *slot as i32;
                            code.push(if ctx.take_ptr {
                                Instruction::PeekPtr(slot)
                            } else {
                                Instruction::Peek(slot)
                            });
                        } else {
                            let slot = sess
                                .get_global(id)
                                .unwrap_or_else(|| find_and_lower_top_level_binding(id, sess))
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
            ast::Ast::ArrayLiteral(lit) => {
                let ty = lit.ty.normalize(sess.tycx);
                let inner_ty_size = ty.inner().size_of(WORD_SIZE);

                match &lit.kind {
                    ast::ArrayLiteralKind::List(elements) => {
                        sess.push_const(code, Value::Type(ty));
                        code.push(Instruction::ArrayAlloc(
                            (elements.len() * inner_ty_size) as u32,
                        ));

                        for (index, element) in elements.iter().enumerate() {
                            element.lower(sess, code, LowerContext { take_ptr: false });
                            code.push(Instruction::ArrayPut((index * inner_ty_size) as u32));
                        }
                    }
                    ast::ArrayLiteralKind::Fill { len: _, expr } => {
                        let size = if let Type::Array(_, size) = ty {
                            size
                        } else {
                            panic!()
                        };

                        sess.push_const(code, Value::Type(ty));
                        code.push(Instruction::ArrayAlloc((size * inner_ty_size) as u32));

                        expr.lower(sess, code, LowerContext { take_ptr: false });

                        code.push(Instruction::ArrayFill(size as u32));
                    }
                }
            }
            ast::Ast::TupleLiteral(lit) => {
                code.push(Instruction::AggregateAlloc);

                for element in lit.elements.iter() {
                    element.lower(sess, code, LowerContext { take_ptr: false });
                    code.push(Instruction::AggregatePush);
                }
            }
            ast::Ast::StructLiteral(lit) => {
                code.push(Instruction::AggregateAlloc);

                let ty = self.ty().normalize(sess.tycx);
                let ty = ty.as_struct();

                let mut ordered_fields = lit.fields.clone();

                ordered_fields.sort_by(|f1, f2| {
                    let index_1 = ty.find_field_position(f1.symbol).unwrap();
                    let index_2 = ty.find_field_position(f2.symbol).unwrap();
                    index_1.cmp(&index_2)
                });

                for field in ordered_fields.iter() {
                    field
                        .expr
                        .lower(sess, code, LowerContext { take_ptr: false });
                    code.push(Instruction::AggregatePush);
                }
            }
            ast::Ast::Literal(_) => {
                panic!("Literal expression should have been lowered to a ConstValue")
            }
            ast::Ast::PointerType(_)
            | ast::Ast::MultiPointerType(_)
            | ast::Ast::ArrayType(_)
            | ast::Ast::SliceType(_)
            | ast::Ast::SelfType(_)
            | ast::Ast::Placeholder(_)
            | ast::Ast::StructType(_)
            | ast::Ast::FunctionType(_) => {
                panic!("unexpected type expression should have been lowered to a ConstValue")
            }
            ast::Ast::Const(const_) => {
                let value = const_value_to_value(&const_.value, self.ty(), sess);
                sess.push_const(code, value);
            }
            ast::Ast::Error(_) => sess.push_const_unit(code),
        }
    }
}

fn lower_local_binding(binding: &ast::Binding, sess: &mut InterpSess, code: &mut CompiledCode) {
    match &binding.kind {
        ast::BindingKind::Extern(_) => {
            let (id, value) = lower_extern_function(sess, binding);

            sess.push_const(code, value);
            sess.add_local(code, id);

            code.push(Instruction::SetLocal(code.last_local()));
        }
        ast::BindingKind::Intrinsic(intrinsic) => {
            let pattern = binding.pattern.as_symbol_ref();

            let intrinsic_func = match intrinsic {
                Intrinsic::StartWorkspace => IntrinsicFunction::StartWorkspace,
            };
            sess.push_const(code, Value::Intrinsic(intrinsic_func));

            sess.add_local(code, pattern.id);

            code.push(Instruction::SetLocal(code.last_local()));
        }
        ast::BindingKind::Normal => {
            if let Some(expr) = &binding.expr {
                expr.lower(sess, code, LowerContext { take_ptr: false });
            }

            match &binding.pattern {
                Pattern::Symbol(pattern) => {
                    sess.add_local(code, pattern.id);

                    if binding.expr.is_some() {
                        code.push(Instruction::SetLocal(code.last_local()));
                    }
                }
                Pattern::StructUnpack(pattern) => {
                    let ty = binding.ty.normalize(sess.tycx);

                    match ty.maybe_deref_once() {
                        Type::Module(_) => lower_local_module_unpack(pattern, sess, code),
                        Type::Struct(struct_ty) => {
                            lower_local_struct_unpack(pattern, &struct_ty, sess, code)
                        }
                        _ => panic!("{}", ty),
                    }
                }
                Pattern::TupleUnpack(pattern) => lower_local_tuple_unpack(pattern, sess, code),
                Pattern::Hybrid(pattern) => {
                    if !pattern.symbol.ignore {
                        sess.add_local(code, pattern.symbol.id);

                        if binding.expr.is_some() {
                            code.push(Instruction::Copy(0));
                            code.push(Instruction::SetLocal(code.last_local()));
                        }
                    }

                    match &pattern.unpack {
                        UnpackPatternKind::Struct(pattern) => {
                            let ty = binding.ty.normalize(sess.tycx);

                            match ty.maybe_deref_once() {
                                Type::Module(_) => lower_local_module_unpack(pattern, sess, code),
                                Type::Struct(struct_ty) => {
                                    lower_local_struct_unpack(pattern, &struct_ty, sess, code)
                                }
                                _ => panic!("{}", ty),
                            }
                        }
                        UnpackPatternKind::Tuple(pattern) => {
                            lower_local_tuple_unpack(pattern, sess, code)
                        }
                    }
                }
            }
        }
    }

    sess.push_const_unit(code);
}

fn lower_local_module_unpack(
    pattern: &UnpackPattern,
    sess: &mut InterpSess,
    code: &mut CompiledCode,
) {
    for pattern in pattern.symbols.iter() {
        if pattern.ignore {
            continue;
        }

        let redirect_id = sess
            .workspace
            .binding_infos
            .get(pattern.id)
            .unwrap()
            .redirects_to
            .unwrap();

        let slot = sess
            .get_global(redirect_id)
            .unwrap_or_else(|| find_and_lower_top_level_binding(redirect_id, sess))
            as u32;

        code.push(Instruction::GetGlobal(slot as u32));
        sess.add_local(code, pattern.id);
        code.push(Instruction::SetLocal(code.last_local()));
    }
}

fn lower_local_struct_unpack(
    pattern: &UnpackPattern,
    struct_ty: &StructType,
    sess: &mut InterpSess,
    code: &mut CompiledCode,
) {
    let last_index = pattern.symbols.len() - 1;

    for (index, pattern) in pattern.symbols.iter().enumerate() {
        if pattern.ignore {
            continue;
        }

        if index < last_index {
            code.push(Instruction::Copy(0));
        }

        let field_index = struct_ty.find_field_position(pattern.symbol).unwrap();

        code.push(Instruction::ConstIndex(field_index as u32));
        sess.add_local(code, pattern.id);
        code.push(Instruction::SetLocal(code.last_local()));
    }
}

fn lower_local_tuple_unpack(
    pattern: &UnpackPattern,
    sess: &mut InterpSess,
    code: &mut CompiledCode,
) {
    let last_index = pattern.symbols.len() - 1;

    for (index, pattern) in pattern.symbols.iter().enumerate() {
        if pattern.ignore {
            continue;
        }

        if index < last_index {
            code.push(Instruction::Copy(0));
        }

        code.push(Instruction::ConstIndex(index as u32));
        sess.add_local(code, pattern.id);
        code.push(Instruction::SetLocal(code.last_local()));
    }
}

impl Lower for ast::Cast {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.expr
            .lower(sess, code, LowerContext { take_ptr: false });

        match self.ty.normalize(sess.tycx) {
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
            Type::Pointer(ty, _) | Type::MultiPointer(ty, _) => {
                let cast_inst = CastInstruction::Ptr(ValueKind::from(ty.as_ref()));
                code.push(Instruction::Cast(cast_inst));
            }
            Type::Slice(_, _) => {
                let expr_ty = self.expr.ty().normalize(sess.tycx);
                let inner_ty_size = expr_ty.inner().size_of(WORD_SIZE);

                code.push(Instruction::AggregateAlloc);

                self.expr.lower(sess, code, LowerContext { take_ptr: true });

                // calculate the new slice's offset
                sess.push_const(code, Value::Uint(0));
                sess.push_const(code, Value::Uint(inner_ty_size));
                code.push(Instruction::Mul);
                code.push(Instruction::Offset);

                code.push(Instruction::AggregatePush);

                // calculate the slice length, by doing `high - low`
                match expr_ty.maybe_deref_once() {
                    Type::Array(_, len) => {
                        sess.push_const(code, Value::Uint(len));
                    }
                    ty => unreachable!("unexpected type `{}`", ty),
                }

                sess.push_const(code, Value::Uint(0));
                code.push(Instruction::Sub);

                code.push(Instruction::AggregatePush);
            }
            Type::Infer(_, InferTy::AnyInt) => {
                code.push(Instruction::Cast(CastInstruction::Int));
            }
            Type::Infer(_, InferTy::AnyFloat) => {
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

impl Lower for ast::Function {
    fn lower(&self, sess: &mut InterpSess, _code: &mut CompiledCode, _ctx: LowerContext) {
        if sess.interp.functions.contains_key(&self.id) || !sess.lowered_functions.insert(self.id) {
            return;
        }

        match &self.kind {
            ast::FunctionKind::Orphan { sig, body } => {
                sess.env_mut().push_scope();

                let mut func_code = CompiledCode::new();

                // set up function parameters
                let mut param_offset = -(sig.params.len() as i16);

                for param in sig.params.iter() {
                    match &param.pattern {
                        Pattern::Symbol(pattern) => {
                            if !pattern.ignore {
                                sess.env_mut().insert(pattern.id, param_offset);
                            }
                        }
                        Pattern::StructUnpack(pattern) => {
                            let ty = param.ty.normalize(sess.tycx).maybe_deref_once();
                            let ty = ty.as_struct();

                            for pattern in pattern.symbols.iter() {
                                if pattern.ignore {
                                    continue;
                                }

                                let field_index = ty.find_field_position(pattern.symbol).unwrap();
                                // TODO: we should be able to PeekPtr here - but we get a strange "capacity overflow" error
                                func_code.push(Instruction::Peek(param_offset as i32));
                                func_code.push(Instruction::ConstIndex(field_index as u32));
                                sess.add_local(&mut func_code, pattern.id);
                                func_code.push(Instruction::SetLocal(func_code.last_local()));
                            }
                        }
                        Pattern::TupleUnpack(pattern) => {
                            for (index, pattern) in pattern.symbols.iter().enumerate() {
                                if pattern.ignore {
                                    continue;
                                }

                                func_code.push(Instruction::PeekPtr(param_offset as i32));
                                func_code.push(Instruction::ConstIndex(index as u32));
                                sess.add_local(&mut func_code, pattern.id);
                                func_code.push(Instruction::SetLocal(func_code.last_local()));
                            }
                        }
                        Pattern::Hybrid(pattern) => {
                            if !pattern.symbol.ignore {
                                sess.env_mut().insert(pattern.symbol.id, param_offset);
                            }

                            match &pattern.unpack {
                                UnpackPatternKind::Struct(pattern) => {
                                    let ty = param.ty.normalize(sess.tycx).maybe_deref_once();
                                    let ty = ty.as_struct();

                                    for pattern in pattern.symbols.iter() {
                                        if pattern.ignore {
                                            continue;
                                        }

                                        let field_index =
                                            ty.find_field_position(pattern.symbol).unwrap();
                                        // TODO: we should be able to PeekPtr here - but we get a strange "capacity overflow" error
                                        func_code.push(Instruction::Peek(param_offset as i32));
                                        func_code.push(Instruction::ConstIndex(field_index as u32));
                                        sess.add_local(&mut func_code, pattern.id);
                                        func_code
                                            .push(Instruction::SetLocal(func_code.last_local()));
                                    }
                                }
                                UnpackPatternKind::Tuple(pattern) => {
                                    for (index, pattern) in pattern.symbols.iter().enumerate() {
                                        if pattern.ignore {
                                            continue;
                                        }

                                        func_code.push(Instruction::PeekPtr(param_offset as i32));
                                        func_code.push(Instruction::ConstIndex(index as u32));
                                        sess.add_local(&mut func_code, pattern.id);
                                        func_code
                                            .push(Instruction::SetLocal(func_code.last_local()));
                                    }
                                }
                            }
                        }
                    }
                    param_offset += 1;
                }

                lower_block(
                    body.as_ref().unwrap(),
                    sess,
                    &mut func_code,
                    LowerContext { take_ptr: false },
                );

                if !func_code.instructions.ends_with(&[Instruction::Return]) {
                    func_code.push(Instruction::Return);
                }

                sess.env_mut().pop_scope();

                let sig_ty = sig.ty.normalize(sess.tycx).into_fn();

                sess.interp.functions.insert(
                    self.id,
                    Function {
                        id: self.id,
                        name: sig.name,
                        arg_types: sig_ty.params,
                        return_type: *sig_ty.ret,
                        code: func_code,
                    },
                );
            }
            ast::FunctionKind::Extern { name, lib } => {
                let func_ty = self.ty.normalize(sess.tycx).into_fn();

                sess.interp.extern_functions.insert(
                    self.id,
                    ExternFunction {
                        lib_path: ustr(&lib.as_ref().unwrap().path()),
                        name: *name,
                        param_tys: func_ty.params,
                        return_ty: *func_ty.ret,
                        variadic: func_ty.varargs.is_some(),
                    },
                );
            }
            ast::FunctionKind::Intrinsic(_) => {
                // Noop
            }
        }
    }
}

impl Lower for ast::Call {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        for arg in self.args.iter() {
            arg.lower(sess, code, LowerContext { take_ptr: false });
        }
        self.callee
            .lower(sess, code, LowerContext { take_ptr: false });
        code.push(Instruction::Call(self.args.len() as u32));
    }
}

impl Lower for ast::For {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        // lower iterator index
        sess.push_const(code, Value::Uint(0));

        sess.add_local(
            code,
            self.index_binding
                .as_ref()
                .map_or(BindingId::unknown(), |x| x.id),
        );

        let iter_index_slot = code.last_local();
        code.push(Instruction::SetLocal(iter_index_slot));

        match &self.iterator {
            ast::ForIter::Range(start, end) => {
                start.lower(sess, code, ctx);

                sess.add_local(code, self.iter_binding.id);
                let iter_slot = code.last_local();
                code.push(Instruction::SetLocal(iter_slot));

                // calculate the end index
                end.lower(sess, code, ctx);

                // lower the condition
                let loop_start = code.push(Instruction::Peek(iter_slot));
                code.push(Instruction::Copy(1));
                code.push(Instruction::LtEq);

                let exit_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));

                let block_start_pos = code.instructions.len();

                lower_block(&self.block, sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Pop);

                // increment the iterator
                let continue_pos = code.push(Instruction::PeekPtr(iter_slot));
                code.push(Instruction::Increment);

                // increment the index
                code.push(Instruction::PeekPtr(iter_index_slot));
                code.push(Instruction::Increment);

                let offset = code.instructions.len() - loop_start;
                code.push(Instruction::Jmp(-(offset as i32)));

                patch_jmp(code, exit_jmp);
                patch_loop_terminators(code, block_start_pos, continue_pos);

                // pop the end index
                code.push(Instruction::Pop);
            }
            ast::ForIter::Value(value) => {
                let value_ty = value.ty().normalize(sess.tycx).maybe_deref_once();

                value.lower(sess, code, ctx);

                // set the iterated value to a hidden local, in order to avoid unnecessary copies
                code.locals += 1;
                let value_slot = code.last_local();
                code.push(Instruction::SetLocal(value_slot));

                // calculate the end index
                match value_ty {
                    Type::Array(_, len) => {
                        sess.push_const(code, Value::Uint(len));
                    }
                    Type::Slice(..) => {
                        code.push(Instruction::PeekPtr(value_slot));
                        code.push(Instruction::ConstIndex(1));
                    }
                    ty => unreachable!("unexpected type `{}`", ty),
                };

                if value_ty.is_slice() {
                    code.push(Instruction::Peek(value_slot));
                    code.push(Instruction::ConstIndex(0));
                    code.push(Instruction::SetLocal(value_slot));
                }

                // lower the condition
                let loop_start = code.push(Instruction::Copy(0));
                code.push(Instruction::Peek(iter_index_slot));
                code.push(Instruction::Gt);

                let exit_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));

                // move the iterator to the current index

                code.push(if value_ty.is_slice() {
                    Instruction::Peek(value_slot)
                } else {
                    Instruction::PeekPtr(value_slot)
                });

                code.push(Instruction::Peek(iter_index_slot));
                sess.push_const(code, Value::Uint(value_ty.inner().size_of(WORD_SIZE)));
                code.push(Instruction::Mul);
                code.push(Instruction::Index);

                sess.add_local(code, self.iter_binding.id);
                let iter_slot = code.last_local();
                code.push(Instruction::SetLocal(iter_slot));

                let block_start_pos = code.instructions.len();

                lower_block(&self.block, sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Pop);

                let continue_pos = code.instructions.len() - 1;

                // increment the index
                code.push(Instruction::PeekPtr(iter_index_slot));
                code.push(Instruction::Increment);

                let offset = code.instructions.len() - loop_start;
                code.push(Instruction::Jmp(-(offset as i32)));

                patch_jmp(code, exit_jmp);
                patch_loop_terminators(code, block_start_pos, continue_pos);

                // pop the end index
                code.push(Instruction::Pop);
            }
        }

        sess.push_const_unit(code);
    }
}

impl Lower for ast::While {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let loop_start = code.instructions.len();

        self.cond
            .lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));

        let block_start_pos = code.instructions.len();

        lower_block(&self.block, sess, code, LowerContext { take_ptr: false });

        code.push(Instruction::Pop);

        let offset = code.instructions.len() - loop_start;
        code.push(Instruction::Jmp(-(offset as i32)));

        patch_jmp(code, exit_jmp);
        patch_loop_terminators(code, block_start_pos, loop_start);

        sess.push_const_unit(code);
    }
}

impl Lower for ast::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.cond
            .lower(sess, code, LowerContext { take_ptr: false });

        let else_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));

        self.then
            .lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.push(Instruction::Jmp(INVALID_JMP_OFFSET));

        patch_jmp(code, else_jmp);

        if let Some(otherwise) = &self.otherwise {
            otherwise.lower(sess, code, LowerContext { take_ptr: false });
        } else {
            sess.push_const_unit(code);
        }

        patch_jmp(code, exit_jmp);
    }
}

impl Lower for ast::Binary {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.lhs.lower(sess, code, LowerContext { take_ptr: false });
        self.rhs.lower(sess, code, LowerContext { take_ptr: false });
        code.push(self.op.into());
    }
}

impl Lower for ast::Unary {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.value.lower(
            sess,
            code,
            match self.op {
                ast::UnaryOp::Ref(_) => LowerContext { take_ptr: true },
                ast::UnaryOp::Deref => ctx,
                ast::UnaryOp::Neg | ast::UnaryOp::Plus | ast::UnaryOp::Not => {
                    LowerContext { take_ptr: false }
                }
            },
        );

        // Note (Ron): Ref isn't a real instruction in the VM's context, so we don't push it
        match self.op {
            ast::UnaryOp::Ref(_) => (),
            _ => {
                code.push(self.op.into());
            }
        }
    }
}

impl Lower for ast::Subscript {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.expr.lower(sess, code, LowerContext { take_ptr: true });

        let expr_ty = self.expr.ty().normalize(sess.tycx);

        if expr_ty.is_slice() {
            code.push(Instruction::ConstIndex(0));
        }

        self.index
            .lower(sess, code, LowerContext { take_ptr: false });

        // match expr_ty {
        //     TyKind::Array(inner, _)
        //     | TyKind::Pointer(inner, _)
        //     | TyKind::MultiPointer(inner, _) => {
        // if this is a pointer offset, we need to multiply the index by the pointer size
        sess.push_const(code, Value::Uint(expr_ty.inner().size_of(WORD_SIZE)));
        code.push(Instruction::Mul);
        //     }
        //     _ => (),
        // }

        code.push(if ctx.take_ptr {
            Instruction::IndexPtr
        } else {
            Instruction::Index
        });
    }
}

fn const_value_to_value(const_value: &ConstValue, ty: TypeId, sess: &mut InterpSess) -> Value {
    let ty = ty.normalize(sess.tycx);

    match const_value {
        ConstValue::Unit(_) => Value::unit(),
        ConstValue::Type(ty) => Value::Type(ty.normalize(sess.tycx)),
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
            Type::Infer(_, InferTy::AnyInt) => Value::Int(*v as _),
            Type::Infer(_, InferTy::AnyFloat) => {
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
            Type::Infer(_, InferTy::AnyInt) => Value::Int(*v as _),
            Type::Infer(_, InferTy::AnyFloat) => {
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
            Type::Infer(_, InferTy::AnyFloat) => {
                if IS_64BIT {
                    Value::F64(*v as _)
                } else {
                    Value::F32(*v as _)
                }
            }
            _ => panic!("invalid ty {}", ty),
        },
        ConstValue::Str(v) => Value::from(*v),
        ConstValue::Tuple(elements) => Value::Aggregate(Aggregate {
            elements: elements
                .iter()
                .map(|el| const_value_to_value(&el.value, el.ty, sess))
                .collect(),
            ty,
        }),
        ConstValue::Struct(fields) => Value::Aggregate(Aggregate {
            elements: fields
                .values()
                .map(|el| const_value_to_value(&el.value, el.ty, sess))
                .collect(),
            ty,
        }),
        ConstValue::Array(array) => {
            let array_len = array.values.len();

            let el_ty = array.element_ty;
            let el_ty_kind = el_ty.normalize(sess.tycx);
            let el_size = el_ty_kind.size_of(WORD_SIZE);

            let mut bytes = ByteSeq::new(array_len * el_size);

            for (index, const_value) in array.values.iter().enumerate() {
                let value = const_value_to_value(const_value, el_ty, sess);

                bytes.offset_mut(index * el_size).put_value(&value);
            }

            Value::Array(Array {
                bytes,
                ty: Type::Array(Box::new(el_ty_kind), array_len),
            })
        }
        ConstValue::Function(f) => {
            let function = sess.typed_ast.functions.get(f.id).unwrap();

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

impl Lower for ast::Slice {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let expr_ty = self.expr.ty().normalize(sess.tycx);
        let inner_ty_size = expr_ty.inner().size_of(WORD_SIZE);

        // lower `low`, or push a 0
        fn lower_low(sess: &mut InterpSess, code: &mut CompiledCode, low: &Option<Box<ast::Ast>>) {
            if let Some(low) = low {
                low.lower(sess, code, LowerContext { take_ptr: false });
            } else {
                sess.push_const(code, Value::Uint(0));
            }
        }

        // lower `high`, or push the expression's length
        fn lower_high(
            sess: &mut InterpSess,
            code: &mut CompiledCode,
            high: &Option<Box<ast::Ast>>,
            expr_ty: &Type,
        ) {
            if let Some(high) = high {
                high.lower(sess, code, LowerContext { take_ptr: false });
            } else {
                match expr_ty {
                    Type::Array(_, len) => {
                        sess.push_const(code, Value::Uint(*len));
                    }
                    Type::Slice(..) => {
                        code.push(Instruction::Roll(1));
                    }
                    ty => unreachable!("unexpected type `{}`", ty),
                }
            }
        }

        match expr_ty {
            Type::Array(..) => {
                code.push(Instruction::AggregateAlloc);

                self.expr.lower(sess, code, LowerContext { take_ptr: true });

                // calculate the new slice's offset
                lower_low(sess, code, &self.low);
                sess.push_const(code, Value::Uint(inner_ty_size));
                code.push(Instruction::Mul);
                code.push(Instruction::Offset);

                code.push(Instruction::AggregatePush);

                // calculate the slice length, by doing `high - low`
                lower_high(sess, code, &self.high, &expr_ty);
                lower_low(sess, code, &self.low);
                code.push(Instruction::Sub);

                code.push(Instruction::AggregatePush);
            }
            Type::Slice(..) => {
                self.expr
                    .lower(sess, code, LowerContext { take_ptr: false });

                code.push(Instruction::Copy(0));
                code.push(Instruction::ConstIndex(1));
                code.push(Instruction::Roll(1));
                code.push(Instruction::ConstIndex(0));

                code.push(Instruction::AggregateAlloc);

                code.push(Instruction::Roll(1));

                // calculate the new slice's offset
                lower_low(sess, code, &self.low);
                sess.push_const(code, Value::Uint(inner_ty_size));
                code.push(Instruction::Mul);
                code.push(Instruction::Offset);

                code.push(Instruction::AggregatePush);

                // calculate the slice length, by doing `high - low`
                lower_high(sess, code, &self.high, &expr_ty);
                lower_low(sess, code, &self.low);
                code.push(Instruction::Sub);

                code.push(Instruction::AggregatePush);
            }
            Type::Pointer(..) | Type::MultiPointer(..) => {
                code.push(Instruction::AggregateAlloc);

                self.expr
                    .lower(sess, code, LowerContext { take_ptr: false });

                lower_low(sess, code, &self.low);
                sess.push_const(code, Value::Uint(inner_ty_size));
                code.push(Instruction::Mul);
                code.push(Instruction::Offset);

                code.push(Instruction::AggregatePush);

                lower_high(sess, code, &self.high, &expr_ty);
                lower_low(sess, code, &self.low);
                code.push(Instruction::Sub);

                code.push(Instruction::AggregatePush);
            }
            _ => panic!("unexpected type {}", expr_ty),
        }
    }
}

const INVALID_JMP_OFFSET: i32 = i32::MAX;
const INVALID_BREAK_JMP_OFFSET: i32 = i32::MAX - 1;
const INVALID_CONTINUE_JMP_OFFSET: i32 = i32::MAX - 2;

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
            if *offset == INVALID_BREAK_JMP_OFFSET {
                *offset = (len - inst_pos) as i32;
            }
            if *offset == INVALID_CONTINUE_JMP_OFFSET {
                *offset = continue_pos as i32 - inst_pos as i32;
            }
        };
    }
}

fn find_and_lower_top_level_binding(id: BindingId, sess: &mut InterpSess) -> usize {
    let binding = sess
        .typed_ast
        .get_binding(id)
        .unwrap_or_else(|| panic!("binding not found: {:?}", id));

    lower_top_level_binding(binding, id, sess)
}

fn lower_extern_function(sess: &mut InterpSess, binding: &ast::Binding) -> (BindingId, Value) {
    let pattern = binding.pattern.as_symbol_ref();
    let binding_info = sess.workspace.binding_infos.get(pattern.id).unwrap();
    let ty = binding_info.ty.normalize(sess.tycx);

    if let Some(ConstValue::Function(function)) = &binding_info.const_value {
        let function = sess.typed_ast.functions.get(function.id).unwrap();

        match &function.kind {
            ast::FunctionKind::Extern { name, .. } => {
                let function_type = ty.as_fn();

                let extern_function =
                    function_type_to_extern_function(pattern.symbol, function_type);

                if !sess.interp.extern_functions.contains_key(&function.id) {
                    sess.interp
                        .extern_functions
                        .insert(function.id, extern_function);
                }

                let value = Value::Function(FunctionAddress {
                    id: function.id,
                    name: *name,
                });

                (pattern.id, value)
            }
            kind => panic!("got {:?}", kind),
        }
    } else {
        todo!("lower extern variables")
    }
}

fn lower_top_level_binding(
    binding: &ast::Binding,
    desired_id: BindingId,
    sess: &mut InterpSess,
) -> usize {
    sess.env_stack.push((binding.module_id, Env::default()));

    let desired_slot = match &binding.kind {
        ast::BindingKind::Extern(_) => {
            let (id, value) = lower_extern_function(sess, binding);
            sess.insert_global(id, value)
        }
        ast::BindingKind::Intrinsic(intrinsic) => {
            let pattern = binding.pattern.as_symbol_ref();
            sess.insert_global(pattern.id, Value::Intrinsic((*intrinsic).into()))
        }
        ast::BindingKind::Normal => {
            let mut code = CompiledCode::new();

            binding
                .expr
                .as_ref()
                .unwrap()
                .lower(sess, &mut code, LowerContext { take_ptr: false });

            sess.env_stack.pop();

            let mut desired_slot = usize::MAX;

            match &binding.pattern {
                Pattern::Symbol(pattern) => {
                    let slot = sess.insert_global(pattern.id, Value::unit());
                    code.push(Instruction::SetGlobal(slot as u32));

                    if pattern.id == desired_id {
                        desired_slot = slot;
                    }
                }
                Pattern::StructUnpack(pattern) => {
                    let ty = binding.ty.normalize(sess.tycx);

                    match ty.maybe_deref_once() {
                        Type::Module(_) => lower_top_level_binding_module_unpack(
                            pattern,
                            desired_id,
                            &mut desired_slot,
                            &mut code,
                            sess,
                        ),
                        Type::Struct(struct_ty) => lower_top_level_binding_struct_unpack(
                            pattern,
                            &struct_ty,
                            desired_id,
                            &mut desired_slot,
                            &mut code,
                            sess,
                        ),
                        _ => panic!("{}", ty),
                    }
                }
                Pattern::TupleUnpack(pattern) => lower_top_level_binding_tuple_unpack(
                    pattern,
                    desired_id,
                    &mut desired_slot,
                    &mut code,
                    sess,
                ),
                Pattern::Hybrid(pattern) => {
                    if !pattern.symbol.ignore {
                        let slot = sess.insert_global(pattern.symbol.id, Value::unit());
                        code.push(Instruction::Copy(0));
                        code.push(Instruction::SetGlobal(slot as u32));

                        if pattern.symbol.id == desired_id {
                            desired_slot = slot;
                        }
                    }

                    match &pattern.unpack {
                        UnpackPatternKind::Struct(pattern) => {
                            let ty = binding.ty.normalize(sess.tycx);

                            match ty.maybe_deref_once() {
                                Type::Module(_) => lower_top_level_binding_module_unpack(
                                    pattern,
                                    desired_id,
                                    &mut desired_slot,
                                    &mut code,
                                    sess,
                                ),
                                Type::Struct(struct_ty) => lower_top_level_binding_struct_unpack(
                                    pattern,
                                    &struct_ty,
                                    desired_id,
                                    &mut desired_slot,
                                    &mut code,
                                    sess,
                                ),
                                _ => panic!("{}", ty),
                            }
                        }
                        UnpackPatternKind::Tuple(pattern) => lower_top_level_binding_tuple_unpack(
                            pattern,
                            desired_id,
                            &mut desired_slot,
                            &mut code,
                            sess,
                        ),
                    }
                }
            }

            sess.push_const_unit(&mut code);

            code.push(Instruction::Return);
            sess.evaluated_globals.push(code);

            desired_slot
        }
    };

    assert!(desired_slot != usize::MAX);

    desired_slot
}

fn lower_top_level_binding_module_unpack(
    pattern: &UnpackPattern,
    desired_id: BindingId,
    desired_slot: &mut usize,
    code: &mut CompiledCode,
    sess: &mut InterpSess,
) {
    for pattern in pattern.symbols.iter() {
        if pattern.ignore {
            continue;
        }

        let redirect_id = sess
            .workspace
            .binding_infos
            .get(pattern.id)
            .unwrap()
            .redirects_to
            .unwrap();

        let slot = sess
            .get_global(redirect_id)
            .unwrap_or_else(|| find_and_lower_top_level_binding(redirect_id, sess))
            as u32;

        code.push(Instruction::GetGlobal(slot as u32));

        let slot = sess.insert_global(pattern.id, Value::unit());
        code.push(Instruction::SetGlobal(slot as u32));

        if pattern.id == desired_id {
            *desired_slot = slot;
        }
    }
}

fn lower_top_level_binding_struct_unpack(
    pattern: &UnpackPattern,
    struct_ty: &StructType,
    desired_id: BindingId,
    desired_slot: &mut usize,
    code: &mut CompiledCode,
    sess: &mut InterpSess,
) {
    let last_index = pattern.symbols.len() - 1;

    for (index, pattern) in pattern.symbols.iter().enumerate() {
        if pattern.ignore {
            continue;
        }

        if index < last_index {
            code.push(Instruction::Copy(0));
        }

        let field_index = struct_ty.find_field_position(pattern.symbol).unwrap();

        code.push(Instruction::ConstIndex(field_index as u32));
        let slot = sess.insert_global(pattern.id, Value::unit());
        code.push(Instruction::SetGlobal(slot as u32));

        if pattern.id == desired_id {
            *desired_slot = slot;
        }
    }
}

fn lower_top_level_binding_tuple_unpack(
    pattern: &UnpackPattern,
    desired_id: BindingId,
    desired_slot: &mut usize,
    code: &mut CompiledCode,
    sess: &mut InterpSess,
) {
    let last_index = pattern.symbols.len() - 1;

    for (index, pattern) in pattern.symbols.iter().enumerate() {
        if pattern.ignore {
            continue;
        }

        if index < last_index {
            code.push(Instruction::Copy(0));
        }

        code.push(Instruction::ConstIndex(index as u32));
        let slot = sess.insert_global(pattern.id, Value::unit());
        code.push(Instruction::SetGlobal(slot as u32));

        if pattern.id == desired_id {
            *desired_slot = slot;
        }
    }
}

fn lower_block(
    block: &ast::Block,
    sess: &mut InterpSess,
    code: &mut CompiledCode,
    ctx: LowerContext,
) {
    sess.env_mut().push_scope();

    for (index, expr) in block.statements.iter().enumerate() {
        let is_last = index == block.statements.len() - 1;

        expr.lower(
            sess,
            code,
            if is_last {
                ctx
            } else {
                LowerContext { take_ptr: false }
            },
        );

        if !is_last || !block.yields {
            code.push(Instruction::Pop);
        }
    }

    if block.statements.is_empty() || !block.yields {
        sess.push_const_unit(code);
    }

    sess.env_mut().pop_scope();
}

fn function_type_to_extern_function(name: Ustr, func_ty: &FunctionType) -> ExternFunction {
    let lib_path = match &func_ty.kind {
        FunctionTypeKind::Extern { lib } => lib.as_ref().unwrap().path(),
        _ => panic!(),
    };

    ExternFunction {
        lib_path: ustr(&lib_path),
        name,
        param_tys: func_ty.params.clone(),
        return_ty: *func_ty.ret.clone(),
        variadic: func_ty.varargs.is_some(),
    }
}
