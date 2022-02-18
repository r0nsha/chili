// use crate::{
//     dump_bytecode_to_file, ffi,
//     instruction::Instruction,
//     run,
//     value::{Function, Value},
//     Bytecode, Constants, Globals,
// };
// use common::scoped_env::ScopedEnv;
// use chilic_parse::{
//     expr::{Expr, ExprKind, LiteralValue},
//     item::Item,
//     item::ItemKind,
//     map_ast,
//     op::{BinaryOp, BinaryOp, BinaryOp, UnaryOp},
//     stmt::{Stmt, StmtKind},
//     Ast, Defer, Proto,
// };
// use ustr::{Ustr, UstrMap};

// pub struct Interp<'i> {
//     top_level_items: UstrMap<&'i Item>,
//     constants: Constants,
//     globals: Globals,
//     env: ScopedEnv<isize>,
//     loop_exits: Vec<isize>,
// }

// impl<'i> Interp<'i> {
//     pub fn new(ast: &'i Ast) -> Self {
//         Self {
//             top_level_items: map_ast(ast),
//             constants: Constants::new(),
//             globals: Globals::default(),
//             env: ScopedEnv::new(),
//             loop_exits: vec![],
//         }
//     }

//     pub fn push_const(&mut self, code: &mut Bytecode, value: Value) {
//         self.constants.push(value);
//         code.push(Instruction::Const(self.constants.len() - 1));
//     }
// }

// pub fn search_and_interp_run_directives(ast: &Ast) {
//     for item in ast {
//         match &item.data {
//             ItemKind::RunDirective(expr) => {
//                 let mut interp = Interp::new(ast);

//                 let mut code = vec![];

//                 interp.env.push_scope();
//                 interp_expr(&mut interp, &mut code, expr);
//                 code.push(Instruction::Return);
//                 interp.env.pop_scope();

//                 dump_bytecode_to_file(&interp.globals, &interp.constants, &code);

//                 println!();
//                 let result = run(&mut interp.globals, &mut interp.constants, code);
//                 println!("\nresult: {}", result);
//             }
//             _ => (),
//         }
//     }
// }

// fn interp_item(interp: &mut Interp, item: &Item) -> Ustr {
//     match &item.data {
//         ItemKind::RunDirective(_) => panic!("interp_item: unexpected run directive"),
//         ItemKind::ForeignFunc(foreign_proto) => {
//             let foreign_func = unsafe { foreign_proto_to_ffi(foreign_proto) };

//             interp
//                 .globals
//                 .insert(foreign_proto.name, Value::ForeignFunc(foreign_func));

//             foreign_proto.name
//         }
//         ItemKind::Func(func) => {
//             let mut code = interp_func_start(interp, &func.proto);
//             for stmt in &func.body {
//                 interp_stmt(interp, &mut code, stmt);
//             }
//             interp_func_end(interp, &func.proto, code);
//             func.proto.name
//         }
//         ItemKind::FuncExpr(func) => {
//             let mut code = interp_func_start(interp, &func.proto);
//             interp_expr(interp, &mut code, &func.body);
//             interp_func_end(interp, &func.proto, code);
//             func.proto.name
//         }
//         ItemKind::Let {
//             name,
//             ty: _,
//             value,
//             is_mutable: _,
//         } => {
//             let mut code = vec![];

//             interp_expr(interp, &mut code, value);

//             match interp.constants.pop() {
//                 Some(constant) => interp.globals.insert(*name, constant),
//                 None => panic!("global let item doesn't have a defined value"),
//             };

//             *name
//         }
//     }
// }

// fn interp_func_start(interp: &mut Interp, proto: &Proto) -> Bytecode {
//     interp.globals.insert(proto.name, Value::());

//     interp.env.push_scope();

//     for (i, param) in proto.params.iter().enumerate() {
//         interp.env.insert(param.data.name, -((i + 1) as isize))
//     }

//     vec![]
// }

// fn interp_func_end(interp: &mut Interp, proto: &Proto, mut code: Bytecode) {
//     if !code.ends_with(&[Instruction::Return]) {
//         // TODO: dafuq???
//         interp.push_const(&mut code, Value::());
//         code.push(Instruction::Return);
//     }

//     interp.env.pop_scope();

//     interp.globals.insert(
//         proto.name,
//         Value::Func(Function {
//             name: proto.name,
//             arg_count: proto.params.len(),
//             code,
//         }),
//     );
// }

// fn interp_stmt(interp: &mut Interp, code: &mut Bytecode, stmt: &Stmt) {
//     match &stmt.data {
//         StmtKind::Assign { name, value } => {
//             interp_expr(interp, code, value);

//             if let Some(slot) = interp.env.get(name) {
//                 code.push(Instruction::SetLocal(*slot));
//             } else if let Some(_) = interp.globals.get(name) {
//                 code.push(Instruction::SetGlobal(*name));
//             } else if let Some(item) = interp.top_level_items.get(name) {
//                 let name = interp_item(interp, item);
//                 code.push(Instruction::SetGlobal(name));
//             } else {
//                 panic!("undefined symbol `{}`", name)
//             }
//         }
//         StmtKind::Let {
//             name,
//             ty: _,
//             value,
//             is_mutable: _,
//         } => {
//             interp_expr(interp, code, value);
//             interp.env.insert(*name, (code.len() - 1) as isize);
//         }
//         StmtKind::While { cond, stmt } => {
//             let loop_start = code.len();

//             interp_expr(interp, code, cond);

//             let exit_jump = push_jmpf(code);
//             code.push(Instruction::Pop);

//             interp_stmt(interp, code, stmt);

//             let offset = code.len() - loop_start;
//             code.push(Instruction::Jmp(-(offset as isize)));

//             let loop_exit = patch_jump(code, exit_jump);
//             code.push(Instruction::Pop);

//             interp.loop_exits.push(loop_exit);
//         }
//         StmtKind::Break {
//             start_defer,
//             end_defer,
//         } => {}
//         StmtKind::Continue {
//             start_defer,
//             end_defer,
//         } => todo!(),
//         StmtKind::Return { expr, defer } => {
//             if let Some(expr) = expr {
//                 interp_expr(interp, code, expr);
//             } else {
//                 interp.push_const(code, Value::Int(0)); // push () here instead of int
//             }

//             interp_defer(interp, code, &None, defer);

//             code.push(Instruction::Return);
//         }
//         StmtKind::Defer(_) => (),
//         StmtKind::Expr(expr) => interp_expr(interp, code, expr),
//     }
// }

// fn interp_expr(interp: &mut Interp, code: &mut Bytecode, expr: &Expr) {
//     match &expr.data {
//         ExprKind::Closure(_) => todo!(),
//         ExprKind::ClosureExpr(_) => todo!(),
//         ExprKind::If {
//             cond,
//             then_expr,
//             else_expr,
//         } => {
//             interp_expr(interp, code, cond);

//             let then_jump = push_jmpf(code);
//             code.push(Instruction::Pop);

//             interp_stmt(interp, code, then_expr);

//             let else_jump = push_jmp(code);

//             patch_jump(code, then_jump);

//             code.push(Instruction::Pop);

//             if let Some(else_expr) = else_expr {
//                 interp_stmt(interp, code, else_expr);
//             }

//             patch_jump(code, else_jump);
//         }
//         ExprKind::Block {
//             stmts,
//             start_defer,
//             end_defer,
//         } => {
//             interp.env.push_scope();

//             for stmt in stmts {
//                 interp_stmt(interp, code, stmt);
//             }

//             interp_defer(interp, code, start_defer, end_defer);

//             interp.env.pop_scope();
//         }
//         ExprKind::Logic { lhs, op, rhs } => {
//             interp_expr(interp, code, lhs);
//             interp_expr(interp, code, rhs);

//             code.push(match op {
//                 BinaryOp::And => Instruction::BAnd,
//                 BinaryOp::Or => Instruction::BOr,
//             })
//         }
//         ExprKind::Comp { lhs, op, rhs } => {
//             interp_expr(interp, code, lhs);
//             interp_expr(interp, code, rhs);

//             code.push(match op {
//                 BinaryOp::Eq => Instruction::Eq,
//                 BinaryOp::NEq => Instruction::NEq,
//                 BinaryOp::Lt => Instruction::Lt,
//                 BinaryOp::LtEq => Instruction::LtEq,
//                 BinaryOp::Gt => Instruction::Gt,
//                 BinaryOp::GtEq => Instruction::GtEq,
//             })
//         }
//         ExprKind::Binary { lhs, op, rhs } => {
//             interp_expr(interp, code, lhs);
//             interp_expr(interp, code, rhs);

//             code.push(match op {
//                 BinaryOp::Add => Instruction::Add,
//                 BinaryOp::Sub => Instruction::Sub,
//                 BinaryOp::Mul => Instruction::Mul,
//                 BinaryOp::Div => Instruction::Div,
//                 BinaryOp::Mod => Instruction::Mod,
//             })
//         }
//         ExprKind::Unary { op, rhs } => {
//             interp_expr(interp, code, rhs);
//             code.push(match op {
//                 UnaryOp::Ref => todo!(),
//                 UnaryOp::Deref => todo!(),
//                 UnaryOp::Minus => Instruction::Neg,
//                 UnaryOp::Not => Instruction::Not,
//             })
//         }
//         ExprKind::Call(call) => {
//             for arg in &call.args {
//                 interp_expr(interp, code, arg);
//             }

//             interp_expr(interp, code, &call.callee);

//             code.push(Instruction::Call(call.args.len()));
//         }
//         ExprKind::Var(name) => {
//             if let Some(slot) = interp.env.get(name) {
//                 code.push(Instruction::GetLocal(*slot));
//             } else if let Some(_) = interp.globals.get(name) {
//                 code.push(Instruction::GetGlobal(*name));
//             } else if let Some(item) = interp.top_level_items.get(name) {
//                 let name = interp_item(interp, item);
//                 code.push(Instruction::GetGlobal(name));
//             } else {
//                 panic!("undefined symbol `{}`", name)
//             }
//         }
//         ExprKind::Literal(value) => {
//             interp.push_const(
//                 code,
//                 match value {
//                     &LiteralValue::() => Value::(),
//                     &LiteralValue::Bool(v) => Value::Bool(v),
//                     &LiteralValue::Int(v) => Value::Int(v),
//                     LiteralValue::Str(v) => Value::Str(v.clone()),
//                 },
//             );
//         }
//     }
// }

// // pub fn codegen_defers(
// //     fx: &mut FuncCtx,
// //     mut current: Option<Defer>,
// //     last: Option<Defer>,
// // ) -> StmtResult {
// //     while current != last && !fx.bcx.is_filled() {
// //         if let Some(_curr) = current {
// //             codegen_expr(fx, _curr.expr.as_ref().clone())?;
// //             current = _curr.prev_defer.map(|d| d.as_ref().clone());
// //         } else {
// //             break;
// //         }
// //     }

// //     Ok(None)
// // }

// fn interp_defer(
//     interp: &mut Interp,
//     code: &mut Bytecode,
//     start_defer: &Option<Defer>,
//     end_defer: &Option<Defer>,
// ) {
//     if end_defer == start_defer {
//         return;
//     }

//     match end_defer {
//         Some(end_defer) => {
//             interp_expr(interp, code, &end_defer.expr);
//             let prev_defer = &end_defer.prev_defer.as_ref().map(|d| d.as_ref().clone());
//             interp_defer(interp, code, start_defer, prev_defer);
//         }
//         None => return,
//     }

//     // let mut current: Option<Defer> = end_defer.clone();
//     //
//     // while current != *start_defer {
//     //     match current {
//     //         Some(_curr) => {
//     //             interp_expr(interp, code, &_curr.expr);
//     //             match &_curr.prev_defer {
//     //                 Some(defer) => current = Some(defer.as_ref().clone()),
//     //                 None => return,
//     //             }
//     //             current = _curr.prev_defer.as_ref().map(|d| d.as_ref().clone());
//     //         }
//     //         None => return,
//     //     }
//     // }
// }

// fn push_jmpf(code: &mut Bytecode) -> usize {
//     push(code, Instruction::Jmpf(0xffff))
// }

// fn push_jmp(code: &mut Bytecode) -> usize {
//     push(code, Instruction::Jmp(0xffff))
// }

// fn push(code: &mut Bytecode, inst: Instruction) -> usize {
//     code.push(inst);
//     code.len() - 1
// }

// fn patch_jump(code: &mut Bytecode, pos: usize) -> isize {
//     let target_offset = (code.len() - 1 - pos) as isize;

//     match code[pos] {
//         Instruction::Jmp(ref mut offset) => *offset = target_offset,
//         Instruction::Jmpt(ref mut offset) => *offset = target_offset,
//         Instruction::Jmpf(ref mut offset) => *offset = target_offset,
//         _ => panic!("instruction at address {} is not a jump", pos),
//     };

//     target_offset
// }

// unsafe fn foreign_proto_to_ffi(proto: &Proto) -> ffi::ForeignFunction {
//     if let Some(lib) = proto.lib {
//         ffi::ForeignFunction {
//             lib,
//             name: proto.name,
//             param_tys: proto.params.iter().map(|p| p.data.ty.clone()).collect(),
//             ret_ty: proto.ret_ty.clone(),
//             variadic: proto.variadic,
//         }
//     } else {
//         panic!("foreign_proto_to_ffi: proto without a lib")
//     }
// }
