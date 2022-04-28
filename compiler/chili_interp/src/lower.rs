use crate::{
    instruction::{CastInstruction, CompiledCode, Instruction},
    interp::{Env, InterpSess},
    value::{ForeignFunc, Func, Slice, Value, ValuePtr},
};
use chili_ast::{
    ast,
    pattern::Pattern,
    ty::{FloatTy, InferTy, IntTy, TyKind, UIntTy},
    workspace::BindingInfoId,
};
use chili_infer::normalize::NormalizeTy;
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use std::mem;
use ustr::ustr;

const IS_64BIT: bool = mem::size_of::<usize>() == 8;

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
                    code.push(Instruction::SetLocal(code.locals as i32));
                }

                match &binding.pattern {
                    Pattern::Single(pat) => {
                        sess.env_mut()
                            .insert(pat.binding_info_id, code.locals as i16);

                        code.locals += 1;
                    }
                    Pattern::StructUnpack(_) => todo!(),
                    Pattern::TupleUnpack(_) => todo!(),
                }
            }
            ast::ExprKind::Defer(_) => todo!(),
            ast::ExprKind::Assign(assign) => {
                assign
                    .rvalue
                    .lower(sess, code, LowerContext { take_ptr: false });

                assign
                    .lvalue
                    .lower(sess, code, LowerContext { take_ptr: true });

                code.push(Instruction::Assign);
            }
            ast::ExprKind::Cast(cast) => cast.lower(sess, code, ctx),
            ast::ExprKind::Builtin(_) => todo!(),
            ast::ExprKind::Fn(func) => func.lower(sess, code, ctx),
            ast::ExprKind::While(_) => todo!(),
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break(_) => todo!(),
            ast::ExprKind::Continue(_) => todo!(),
            ast::ExprKind::Return(_) => todo!(),
            ast::ExprKind::If(if_) => if_.lower(sess, code, ctx),
            ast::ExprKind::Block(block) => block.lower(sess, code, ctx),
            ast::ExprKind::Binary(binary) => binary.lower(sess, code, ctx),
            ast::ExprKind::Unary(unary) => unary.lower(sess, code, ctx),
            ast::ExprKind::Subscript(_) => todo!(),
            ast::ExprKind::Slice(_) => todo!(),
            ast::ExprKind::Call(call) => call.lower(sess, code, ctx),
            ast::ExprKind::MemberAccess(access) => {
                access.expr.lower(sess, code, ctx);

                match &access.expr.ty.normalize(sess.tycx).maybe_deref_once() {
                    TyKind::Tuple(_) | TyKind::Infer(_, InferTy::PartialTuple(_)) => {
                        let index = access.member.parse::<usize>().unwrap();
                        todo!()
                        // code.push(Instruction::Index(index as u32));
                    }
                    TyKind::Struct(st) => {
                        todo!("struct access")
                    }
                    TyKind::Infer(_, InferTy::PartialStruct(partial)) => {
                        todo!("partial struct access")
                    }
                    TyKind::Array(_, size) if access.member.as_str() == BUILTIN_FIELD_LEN => {
                        sess.push_const(code, Value::UInt(*size as usize))
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
                                Instruction::GetLocalPtr(slot)
                            } else {
                                Instruction::GetLocal(slot)
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
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral(_) => todo!(),
            ast::ExprKind::Literal(lit) => {
                let ty = self.ty.normalize(&sess.tycx);
                sess.push_const(
                    code,
                    match &lit.kind {
                        ast::LiteralKind::Unit => Value::Tuple(vec![]),
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
                            TyKind::UInt(ty) => match ty {
                                UIntTy::U8 => Value::U8(*v as _),
                                UIntTy::U16 => Value::U16(*v as _),
                                UIntTy::U32 => Value::U32(*v as _),
                                UIntTy::U64 => Value::U64(*v as _),
                                UIntTy::UInt => Value::UInt(*v as _),
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
                        ast::LiteralKind::Str(v) => Value::Slice(Slice {
                            ptr: ValuePtr::U8(v.as_char_ptr() as *mut u8),
                            len: v.len(),
                        }),
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
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
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
            TyKind::UInt(ty) => {
                code.push(match ty {
                    UIntTy::U8 => Instruction::Cast(CastInstruction::U8),
                    UIntTy::U16 => Instruction::Cast(CastInstruction::U16),
                    UIntTy::U32 => Instruction::Cast(CastInstruction::U32),
                    UIntTy::U64 => Instruction::Cast(CastInstruction::U64),
                    UIntTy::UInt => Instruction::Cast(CastInstruction::UInt),
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
            TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => {
                code.push(Instruction::Cast(CastInstruction::Ptr));
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
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
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
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
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
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        for arg in self.args.iter() {
            arg.lower(sess, code, LowerContext { take_ptr: false });
        }
        self.callee
            .lower(sess, code, LowerContext { take_ptr: false });
        code.push(Instruction::Call(self.args.len() as u32));
    }
}

impl Lower for ast::If {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.cond
            .lower(sess, code, LowerContext { take_ptr: false });

        let then_jmp = push_empty_jmpf(code);
        code.push(Instruction::Pop);

        self.then
            .lower(sess, code, LowerContext { take_ptr: false });

        let else_jmp = push_empty_jmp(code);
        patch_empty_jmp(code, then_jmp);
        code.push(Instruction::Pop);

        if let Some(otherwise) = &self.otherwise {
            otherwise.lower(sess, code, LowerContext { take_ptr: false });
        }

        patch_empty_jmp(code, else_jmp);
    }
}

impl Lower for ast::Block {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        sess.env_mut().push_scope();

        for (index, expr) in self.exprs.iter().enumerate() {
            let is_last = index != self.exprs.len() - 1;

            expr.lower(
                sess,
                code,
                if is_last {
                    ctx
                } else {
                    LowerContext { take_ptr: false }
                },
            );
        }

        // for expr in self.deferred.iter() {
        //     expr.lower(sess, code, LowerContext { take_ptr: false });
        //     code.push(Instruction::Pop);
        // }

        sess.env_mut().pop_scope();
    }
}

impl Lower for ast::Binary {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.lhs.lower(sess, code, LowerContext { take_ptr: false });
        self.rhs.lower(sess, code, LowerContext { take_ptr: false });
        code.push(self.op.into());
    }
}

impl Lower for ast::Unary {
    fn lower(&self, sess: &mut InterpSess, code: &mut CompiledCode, ctx: LowerContext) {
        self.lhs.lower(sess, code, LowerContext { take_ptr: false });
        code.push(self.op.into());
    }
}

fn push_empty_jmpf(code: &mut CompiledCode) -> usize {
    code.push(Instruction::Jmpf(0xffff))
}

fn push_empty_jmp(code: &mut CompiledCode) -> usize {
    code.push(Instruction::Jmp(0xffff))
}

fn patch_empty_jmp(code: &mut CompiledCode, pos: usize) -> i32 {
    let target_offset = (code.instructions.len() - 1 - pos) as i32;

    match &mut code.instructions[pos] {
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
