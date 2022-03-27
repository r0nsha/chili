// use chili_ast::{
//     ast::{self, Ast, Binding},
//     workspace::{BindingInfoFlags, BindingInfoId, Workspace},
// };

// pub fn mark_bindings_for_codegen(workspace: &mut Workspace, asts: &Vec<Ast>) {
//     let mut sess = Sess {
//         marked_bindings: Default::default(),
//     };

//     sess.marked_bindings
//         .push(workspace.entry_point_function_id.unwrap());

//     while !sess.done() {
//         sess.commit(workspace);
//         sess.mark(workspace, asts);
//         sess.clear();
//     }
// }

// fn find_binding(asts: &Vec<Ast>, id: BindingInfoId) -> &Binding {
//     for ast in asts.iter() {
//         if let Some(binding) = ast.find_binding(id) {
//             return binding;
//         }
//     }
//     unreachable!()
// }

// struct Sess {
//     marked_bindings: Vec<BindingInfoId>,
// }

// impl Sess {
//     fn done(&self) -> bool {
//         self.marked_bindings.is_empty()
//     }

//     fn commit(&self, workspace: &mut Workspace) {
//         self.marked_bindings.iter().for_each(|id| {
//             workspace.get_binding_info_mut(*id).unwrap().flags &= BindingInfoFlags::SHOULD_CODEGEN;
//         });
//     }

//     fn mark(&mut self, workspace: &mut Workspace, asts: &Vec<Ast>) {
//         for id in self.marked_bindings.clone() {
//             let binding = find_binding(asts, id);
//             binding.mark_codegen(self, workspace);
//         }
//     }

//     fn clear(&mut self) {
//         self.marked_bindings.clear()
//     }
// }

// trait MarkCodegen<'w> {
//     fn mark_codegen(&self, sess: &mut Sess, workspace: &mut Workspace);
// }

// impl<'w, T: MarkCodegen<'w>> MarkCodegen<'w> for Vec<T> {
//     fn mark_codegen(&self, sess: &mut Sess, workspace: &mut Workspace) {
//         for element in self {
//             element.mark_codegen(sess, workspace);
//         }
//     }
// }

// impl<'w, T: MarkCodegen<'w>> MarkCodegen<'w> for Option<T> {
//     fn mark_codegen(&self, sess: &mut Sess, workspace: &mut Workspace) {
//         if let Some(e) = self {
//             e.mark_codegen(sess, workspace);
//         }
//     }
// }

// impl<'w, T: MarkCodegen<'w>> MarkCodegen<'w> for Box<T> {
//     fn mark_codegen(&self, sess: &mut Sess, workspace: &mut Workspace) {
//         self.as_ref().mark_codegen(sess, workspace);
//     }
// }

// impl<'w> MarkCodegen<'w> for ast::Binding {
//     fn mark_codegen(&self, sess: &mut Sess, workspace: &mut Workspace) {}
// }
