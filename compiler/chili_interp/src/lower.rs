use crate::{
    instruction::{CastInstruction, CompiledCode, Instruction},
    interp::{Env, InterpSess},
    value::{ForeignFunc, Func, Pointer, Value, ValueKind},
    IS_64BIT, WORD_SIZE,
};
use chili_ast::{
    ast,
    pattern::Pattern,
    ty::{align::AlignOf, size::SizeOf, FloatTy, InferTy, IntTy, TyKind, UintTy},
    workspace::BindingInfoId,
};
use chili_infer::normalize::NormalizeTy;
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use ustr::ustr;

#[derive(Clone, Copy)]
pub(crate) struct LowerContext {
    pub(crate) take_ptr: bool,
}

pub(crate) trait Lower {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext);
}

impl Lower for ast::Expr {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        match &self.kind {
            ast::ExprKind::Import(_) => todo!(),
            ast::ExprKind::Foreign(_) => todo!(),
            ast::ExprKind::Binding(binding) => {
                if let Some(expr) = &binding.expr {
                    expr.lower(sess, code, LowerContext { take_ptr: false });
                }

                match &binding.pattern {
                    Pattern::Single(pat) => {
                        if binding.expr.is_some() {
                            code.push(Instruction::SetLocal(code.locals as i32));
                        }

                        sess.add_local(code, pat.binding_info_id);
                    }
                    Pattern::StructUnpack(pat) => {
                        let ty = binding.ty.normalize(sess.tycx);
                        let ty = ty.as_struct();

                        let last_index = pat.symbols.len() - 1;

                        for (index, pat) in pat.symbols.iter().enumerate() {
                            if index < last_index {
                                code.push(Instruction::Copy);
                            }

                            let field_index = ty.field_index(pat.symbol).unwrap();

                            code.push(Instruction::Index(field_index as u32));
                            code.push(Instruction::SetLocal(code.locals as i32));
                            sess.add_local(code, pat.binding_info_id);
                        }
                    }
                    Pattern::TupleUnpack(pat) => {
                        let last_index = pat.symbols.len() - 1;

                        for (index, pat) in pat.symbols.iter().enumerate() {
                            if index < last_index {
                                code.push(Instruction::Copy);
                            }

                            code.push(Instruction::Index(index as u32));
                            code.push(Instruction::SetLocal(code.locals as i32));
                            sess.add_local(code, pat.binding_info_id);
                        }
                    }
                }

                sess.push_const(code, Value::unit());
            }
            ast::ExprKind::Defer(_) => {
                sess.push_const(code, Value::unit());
            }
            ast::ExprKind::Assign(assign) => {
                assign
                    .rvalue
                    .lower(sess, code, LowerContext { take_ptr: false });

                assign
                    .lvalue
                    .lower(sess, code, LowerContext { take_ptr: true });

                code.push(Instruction::Assign);

                sess.push_const(code, Value::unit());
            }
            ast::ExprKind::Cast(cast) => cast.lower(sess, code, ctx),
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) => match expr.ty.normalize(sess.tycx) {
                    TyKind::Type(ty) => sess.push_const(code, Value::Uint(ty.size_of(WORD_SIZE))),
                    ty => unreachable!("got {}", ty),
                },
                ast::Builtin::AlignOf(expr) => match expr.ty.normalize(sess.tycx) {
                    TyKind::Type(ty) => sess.push_const(code, Value::Uint(ty.align_of(WORD_SIZE))),
                    ty => unreachable!("got {}", ty),
                },
                ast::Builtin::Panic(_) => todo!(),
                ast::Builtin::Run(expr) => expr.lower(sess, code, ctx),
            },
            ast::ExprKind::Fn(func) => func.lower(sess, code, ctx),
            ast::ExprKind::While(while_) => {
                while_.lower(sess, code, LowerContext { take_ptr: false })
            }
            ast::ExprKind::For(for_) => for_.lower(sess, code, ctx),
            ast::ExprKind::Break(term) => {
                for expr in term.deferred.iter() {
                    expr.lower(sess, code, LowerContext { take_ptr: false });
                }
                code.push(Instruction::Jmp(INVALID_BREAK_JMP_OFFSET));
            }
            ast::ExprKind::Continue(term) => {
                for expr in term.deferred.iter() {
                    expr.lower(sess, code, LowerContext { take_ptr: false });
                    code.push(Instruction::Pop);
                }
                code.push(Instruction::Jmp(INVALID_CONTINUE_JMP_OFFSET));
            }
            ast::ExprKind::Return(ret) => {
                for expr in ret.deferred.iter() {
                    expr.lower(sess, code, LowerContext { take_ptr: false });
                    code.push(Instruction::Pop);
                }

                if let Some(expr) = &ret.expr {
                    expr.lower(sess, code, LowerContext { take_ptr: false });
                }

                code.push(Instruction::Return);
            }
            ast::ExprKind::If(if_) => if_.lower(sess, code, ctx),
            ast::ExprKind::Block(block) => block.lower(sess, code, ctx),
            ast::ExprKind::Binary(binary) => binary.lower(sess, code, ctx),
            ast::ExprKind::Unary(unary) => unary.lower(sess, code, ctx),
            ast::ExprKind::Subscript(sub) => {
                // println!("{}", self.span.start.index);
                todo!()
            }
            ast::ExprKind::Slice(_) => todo!(),
            ast::ExprKind::Call(call) => call.lower(sess, code, ctx),
            ast::ExprKind::MemberAccess(access) => {
                access.expr.lower(sess, code, ctx);

                match &access.expr.ty.normalize(sess.tycx).maybe_deref_once() {
                    TyKind::Tuple(_) | TyKind::Infer(_, InferTy::PartialTuple(_)) => {
                        let index = access.member.parse::<usize>().unwrap();

                        code.push(if ctx.take_ptr {
                            Instruction::IndexPtr(index as u32)
                        } else {
                            Instruction::Index(index as u32)
                        });
                    }
                    TyKind::Struct(st) => {
                        let index = st.field_index(access.member).unwrap();

                        code.push(if ctx.take_ptr {
                            Instruction::IndexPtr(index as u32)
                        } else {
                            Instruction::Index(index as u32)
                        });
                    }
                    TyKind::Infer(_, InferTy::PartialStruct(partial)) => {
                        let index = partial
                            .iter()
                            .position(|(field, _)| *field == access.member)
                            .unwrap();

                        code.push(if ctx.take_ptr {
                            Instruction::IndexPtr(index as u32)
                        } else {
                            Instruction::Index(index as u32)
                        });
                    }
                    TyKind::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        sess.push_const(code, Value::Uint(*size as usize))
                    }
                    TyKind::Slice(ty, ..) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        // TODO: put real offset when i switch to aggregates
                        code.push(Instruction::Index(1));
                    }
                    TyKind::Slice(..) if access.member.as_str() == BUILTIN_FIELD_DATA => {
                        // TODO: put real offset when i switch to aggregates
                        code.push(Instruction::Index(0));
                    }
                    TyKind::Module(module_id) => {
                        let id = sess.find_symbol(*module_id, access.member);
                        let slot = find_and_lower_top_level_binding(id, sess);
                        code.push(Instruction::GetGlobal(slot as u32));
                    }
                    ty => panic!("invalid type `{}` or member `{}`", ty, access.member),
                }
            }
            ast::ExprKind::Ident(ident) => {
                let id = ident.binding_info_id;

                assert!(id != BindingInfoId::unknown(), "{}", ident.symbol);

                match self.ty.normalize(sess.tycx) {
                    // Note (Ron): We do nothing with modules, since they are not an actual value
                    TyKind::Module(_) => (),
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
            ast::ExprKind::ArrayLiteral(lit) => match &lit.kind {
                ast::ArrayLiteralKind::List(elements) => {
                    code.push(Instruction::AggregateAlloc);

                    for element in elements.iter() {
                        element.lower(sess, code, LowerContext { take_ptr: false });
                        code.push(Instruction::AggregatePush);
                    }
                }
                ast::ArrayLiteralKind::Fill { len: _, expr } => {
                    let ty = self.ty.normalize(sess.tycx);

                    let len = if let TyKind::Array(_, len) = ty {
                        len
                    } else {
                        panic!()
                    };

                    code.push(Instruction::AggregateAlloc);

                    // TODO: This is very inefficient.
                    // TODO: We should precalculate the expression, and just push its result to the array
                    for _ in 0..len {
                        expr.lower(sess, code, LowerContext { take_ptr: false });
                        code.push(Instruction::AggregatePush);
                    }
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                code.push(Instruction::AggregateAlloc);

                for element in lit.elements.iter() {
                    element.lower(sess, code, LowerContext { take_ptr: false });
                    code.push(Instruction::AggregatePush);
                }
            }
            ast::ExprKind::StructLiteral(lit) => {
                code.push(Instruction::AggregateAlloc);

                let ty = self.ty.normalize(sess.tycx);
                let ty = ty.as_struct();

                let mut ordered_fields = lit.fields.clone();

                ordered_fields.sort_by(|f1, f2| {
                    let index_1 = ty.field_index(f1.symbol).unwrap();
                    let index_2 = ty.field_index(f2.symbol).unwrap();
                    index_1.cmp(&index_2)
                });

                for field in ordered_fields.iter() {
                    field
                        .expr
                        .lower(sess, code, LowerContext { take_ptr: false });
                    code.push(Instruction::AggregatePush);
                }
            }
            ast::ExprKind::Literal(lit) => {
                let ty = self.ty.normalize(&sess.tycx);
                sess.push_const(
                    code,
                    match &lit.kind {
                        ast::LiteralKind::Unit => Value::unit(),
                        ast::LiteralKind::Nil => todo!("nil"),
                        ast::LiteralKind::Bool(v) => Value::Bool(*v),
                        ast::LiteralKind::Int(v) => match ty {
                            TyKind::Int(ty) => match ty {
                                IntTy::I8 => Value::I8(*v as _),
                                IntTy::I16 => Value::I16(*v as _),
                                IntTy::I32 => Value::I32(*v as _),
                                IntTy::I64 => Value::I64(*v as _),
                                IntTy::Int => Value::Int(*v as _),
                            },
                            TyKind::Uint(ty) => match ty {
                                UintTy::U8 => Value::U8(*v as _),
                                UintTy::U16 => Value::U16(*v as _),
                                UintTy::U32 => Value::U32(*v as _),
                                UintTy::U64 => Value::U64(*v as _),
                                UintTy::Uint => Value::Uint(*v as _),
                            },
                            TyKind::Float(ty) => match ty {
                                FloatTy::F16 | FloatTy::F32 => Value::F32(*v as _),
                                FloatTy::F64 => Value::F64(*v as _),
                                FloatTy::Float => {
                                    if IS_64BIT {
                                        Value::F64(*v as _)
                                    } else {
                                        Value::F32(*v as _)
                                    }
                                }
                            },
                            TyKind::Infer(_, InferTy::AnyInt) => Value::I32(*v as _),
                            TyKind::Infer(_, InferTy::AnyFloat) => {
                                if IS_64BIT {
                                    Value::F64(*v as _)
                                } else {
                                    Value::F32(*v as _)
                                }
                            }
                            _ => panic!("invalid ty {}", ty),
                        },
                        ast::LiteralKind::Float(v) => match ty {
                            TyKind::Float(ty) => match ty {
                                FloatTy::F16 | FloatTy::F32 => Value::F32(*v as _),
                                FloatTy::F64 => Value::F64(*v as _),
                                FloatTy::Float => {
                                    if IS_64BIT {
                                        Value::F64(*v as _)
                                    } else {
                                        Value::F32(*v as _)
                                    }
                                }
                            },
                            TyKind::Infer(_, InferTy::AnyFloat) => {
                                if IS_64BIT {
                                    Value::F64(*v as _)
                                } else {
                                    Value::F32(*v as _)
                                }
                            }
                            _ => panic!("invalid ty {}", ty),
                        },
                        ast::LiteralKind::Str(v) => Value::Aggregate(vec![
                            Value::Pointer(Pointer::U8(v.as_char_ptr() as *mut u8)),
                            Value::Uint(v.len()),
                        ]),
                        ast::LiteralKind::Char(v) => Value::U8(*v as u8),
                    },
                )
            }
            ast::ExprKind::PointerType(_) => todo!(),
            ast::ExprKind::MultiPointerType(_) => todo!(),
            ast::ExprKind::ArrayType(_) => todo!(),
            ast::ExprKind::SliceType(_) => todo!(),
            ast::ExprKind::StructType(_) => todo!(),
            ast::ExprKind::FnType(sig) => sig.lower(sess, code, LowerContext { take_ptr: false }),
            ast::ExprKind::SelfType => todo!(),
            ast::ExprKind::NeverType => todo!(),
            ast::ExprKind::UnitType => todo!(),
            ast::ExprKind::PlaceholderType => todo!(),
            ast::ExprKind::Error => todo!(),
        }
    }
}

impl Lower for ast::Cast {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.expr
            .lower(sess, code, LowerContext { take_ptr: false });

        match self.target_ty.normalize(sess.tycx) {
            TyKind::Never | TyKind::Unit | TyKind::Bool => (),
            TyKind::Int(ty) => {
                code.push(match ty {
                    IntTy::I8 => Instruction::Cast(CastInstruction::I8),
                    IntTy::I16 => Instruction::Cast(CastInstruction::I16),
                    IntTy::I32 => Instruction::Cast(CastInstruction::I32),
                    IntTy::I64 => Instruction::Cast(CastInstruction::I64),
                    IntTy::Int => Instruction::Cast(CastInstruction::Int),
                });
            }
            TyKind::Uint(ty) => {
                code.push(match ty {
                    UintTy::U8 => Instruction::Cast(CastInstruction::U8),
                    UintTy::U16 => Instruction::Cast(CastInstruction::U16),
                    UintTy::U32 => Instruction::Cast(CastInstruction::U32),
                    UintTy::U64 => Instruction::Cast(CastInstruction::U64),
                    UintTy::Uint => Instruction::Cast(CastInstruction::Uint),
                });
            }
            TyKind::Float(ty) => {
                code.push(match ty {
                    FloatTy::F16 | FloatTy::F32 => Instruction::Cast(CastInstruction::F32),
                    FloatTy::F64 => Instruction::Cast(CastInstruction::F64),
                    FloatTy::Float => Instruction::Cast(if IS_64BIT {
                        CastInstruction::F64
                    } else {
                        CastInstruction::F32
                    }),
                });
            }
            TyKind::Pointer(ty, _) | TyKind::MultiPointer(ty, _) => {
                let cast_inst = CastInstruction::Ptr(ValueKind::from(ty.as_ref()));
                code.push(Instruction::Cast(cast_inst));
            }
            TyKind::Slice(_, _) => todo!(), // TODO: cast pointer to array to a slice (slice coercion)
            TyKind::Infer(_, InferTy::AnyInt) => {
                code.push(Instruction::Cast(CastInstruction::I32));
            }
            TyKind::Infer(_, InferTy::AnyFloat) => {
                code.push(Instruction::Cast(CastInstruction::F32));
            }
            ty => panic!("invalid ty {}", ty),
        }
    }
}

impl Lower for ast::Fn {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        if let Some(id) = self.binding_info_id {
            let binding_info = sess.workspace.get_binding_info(id).unwrap();
            if binding_info.scope_level.is_global() {
                sess.insert_global(id, Value::unit());
            }
        }

        sess.env_mut().push_scope();

        let mut offset: i16 = -1;
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

        let mut func_code = CompiledCode::new();

        self.body
            .lower(sess, &mut func_code, LowerContext { take_ptr: false });

        if !func_code.instructions.ends_with(&[Instruction::Return]) {
            func_code.push(Instruction::Return);
        }

        sess.env_mut().pop_scope();

        let func = Value::Func(Func {
            name: self.sig.name,
            param_count: self.sig.params.len(),
            code: func_code,
        });

        sess.push_const(code, func);
    }
}

impl Lower for ast::FnSig {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        if let Some(lib_name) = self.lib_name {
            let func_ty = self.ty.normalize(sess.tycx).into_fn();

            let module_path = sess
                .workspace
                .get_module_info(sess.module_id())
                .unwrap()
                .file_path;

            let lib_path = ast::ForeignLibrary::from_str(&lib_name.to_string(), &module_path)
                .unwrap()
                .path();

            let lib_path = lib_path.trim_end_matches(".lib");

            let foreign_func = ForeignFunc {
                lib_path: ustr(lib_path),
                name: self.name,
                param_tys: func_ty.params,
                ret_ty: *func_ty.ret,
                variadic: self.variadic,
            };

            sess.push_const(code, Value::ForeignFunc(foreign_func));
        } else {
            // lowering a non-foreign function signature is a no-op (until types will be considered values)
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
        let (iter_slot, iter_index_slot, end_slot) = match &self.iterator {
            ast::ForIter::Range(start, end) => {
                start.lower(sess, code, ctx);

                let iter_slot = code.locals as i32;
                code.push(Instruction::SetLocal(code.locals as i32));

                sess.add_local(code, self.iter_id);

                let iter_index_slot = code.locals as i32;
                code.push(Instruction::SetLocal(code.locals as i32));

                sess.add_local(code, self.iter_index_id);

                end.lower(sess, code, ctx);
                let end_slot = code.instructions.len() as i32 - 1;

                (iter_slot, iter_index_slot, end_slot)
            }
            ast::ForIter::Value(value) => {
                todo!()
            }
        };

        let loop_start = code.instructions.len() - 1;

        code.push(Instruction::Peek(iter_index_slot));
        code.push(Instruction::Peek(end_slot));
        code.push(Instruction::LtEq);

        let exit_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));
        code.push(Instruction::Pop);

        let start_inst_pos = code.instructions.len();

        self.block
            .lower(sess, code, LowerContext { take_ptr: false });

        // set the iterated value
        match &self.iterator {
            ast::ForIter::Range(start, end) => {
                code.push(Instruction::PeekPtr(iter_slot));
                code.push(Instruction::Increment);
            }
            ast::ForIter::Value(value) => {
                todo!()
            }
        }

        // increment the index
        code.push(Instruction::PeekPtr(iter_index_slot));
        code.push(Instruction::Increment);

        code.push(Instruction::Pop);

        let offset = code.instructions.len() - loop_start;
        code.push(Instruction::Jmp(-(offset as i32)));

        patch_jmp(code, exit_jmp);
        code.push(Instruction::Pop);

        patch_loop_terminators(code, start_inst_pos, loop_start);

        sess.push_const(code, Value::unit());
    }
}

impl Lower for ast::While {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        let loop_start = code.instructions.len() - 1;

        self.cond
            .lower(sess, code, LowerContext { take_ptr: false });

        let exit_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));
        code.push(Instruction::Pop);

        let start_inst_pos = code.instructions.len();

        self.block
            .lower(sess, code, LowerContext { take_ptr: false });

        code.push(Instruction::Pop);

        let offset = code.instructions.len() - loop_start;
        code.push(Instruction::Jmp(-(offset as i32)));

        patch_jmp(code, exit_jmp);
        code.push(Instruction::Pop);

        patch_loop_terminators(code, start_inst_pos, loop_start);

        sess.push_const(code, Value::unit());
    }
}

// patch all break/continue jmp instructions
fn patch_loop_terminators(code: &mut CompiledCode, start_inst_pos: usize, loop_start: usize) {
    let len = code.instructions.len();

    for inst_pos in start_inst_pos..len {
        match &mut code.instructions[inst_pos] {
            Instruction::Jmp(offset) => {
                if *offset == INVALID_BREAK_JMP_OFFSET {
                    *offset = (len - 1 - inst_pos) as i32;
                }
                if *offset == INVALID_CONTINUE_JMP_OFFSET {
                    *offset = loop_start as i32 - inst_pos as i32;
                }
            }
            _ => (),
        };
    }
}

impl Lower for ast::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, _ctx: LowerContext) {
        self.cond
            .lower(sess, code, LowerContext { take_ptr: false });

        let then_jmp = code.push(Instruction::Jmpf(INVALID_JMP_OFFSET));
        code.push(Instruction::Pop);

        self.then
            .lower(sess, code, LowerContext { take_ptr: false });

        let else_jmp = code.push(Instruction::Jmp(INVALID_JMP_OFFSET));
        patch_jmp(code, then_jmp);
        code.push(Instruction::Pop);

        if let Some(otherwise) = &self.otherwise {
            otherwise.lower(sess, code, LowerContext { take_ptr: false });
        }

        patch_jmp(code, else_jmp);
    }
}

impl Lower for ast::Block {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        sess.env_mut().push_scope();

        for (index, expr) in self.exprs.iter().enumerate() {
            let is_last = index == self.exprs.len() - 1;

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

        for expr in self.deferred.iter() {
            expr.lower(sess, code, LowerContext { take_ptr: false });
            code.push(Instruction::Pop);
        }

        sess.env_mut().pop_scope();
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
        self.lhs.lower(
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

const INVALID_JMP_OFFSET: i32 = i32::MAX;
const INVALID_BREAK_JMP_OFFSET: i32 = i32::MAX - 1;
const INVALID_CONTINUE_JMP_OFFSET: i32 = i32::MAX - 2;

fn patch_jmp(code: &mut CompiledCode, inst_pos: usize) {
    let target_offset = (code.instructions.len() - 1 - inst_pos) as i32;

    match &mut code.instructions[inst_pos] {
        Instruction::Jmp(offset) | Instruction::Jmpt(offset) | Instruction::Jmpf(offset)
            if *offset == INVALID_JMP_OFFSET =>
        {
            *offset = target_offset
        }
        _ => panic!("instruction at address {} is not a jmp", inst_pos),
    };
}

#[inline]
fn find_and_lower_top_level_binding(id: BindingInfoId, sess: &mut InterpSess) -> usize {
    if let Some(binding) = sess.typed_ast.bindings.get(&id) {
        lower_top_level_binding(binding, sess)
    } else if let Some(import) = sess.typed_ast.imports.get(&id) {
        find_and_lower_top_level_binding(import.target_binding_info_id.unwrap(), sess)
    } else {
        panic!("binding not found: {:?}", id)
    }
}

#[inline]
fn lower_top_level_binding(binding: &ast::Binding, sess: &mut InterpSess) -> usize {
    // TODO: this function is incomplete
    // TODO: it implies that only global bindings resulting in a constant value works

    sess.env_stack.push((binding.module_id, Env::default()));

    binding.expr.as_ref().unwrap().lower(
        sess,
        &mut CompiledCode::new(),
        LowerContext { take_ptr: false },
    );

    sess.env_stack.pop();

    let id = binding.pattern.as_single_ref().binding_info_id;
    assert!(id != BindingInfoId::unknown());

    match sess.interp.constants.pop() {
        Some(value) => sess.insert_global(id, value),
        None => panic!("top level binding doesn't have a defined constant value"),
    }
}
