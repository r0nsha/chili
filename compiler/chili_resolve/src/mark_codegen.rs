use chili_ast::{
    ast::{Ast, Binding},
    workspace::{self, BindingInfoIdx, Workspace},
};

pub fn mark_bindings_for_codegen(workspace: &mut Workspace, asts: &Vec<Ast>) {
    let entry_point_function = find_binding(asts, workspace.entry_point_function_idx.unwrap());
    let entry_point_function = workspace
        .get_binding_info(workspace.entry_point_function_idx)
        .unwrap();
}

fn find_binding(asts: &Vec<Ast>, idx: BindingInfoIdx) -> &Binding {
    for ast in asts.iter() {
        for binding in ast.bindings.iter() {
            if binding.pattern.into_single_ref().binding_info_idx == idx {
                return binding;
            }
        }
    }

    unreachable!()
}

struct Sess {
    marked_bindings: Vec<BindingInfoIdx>,
}

pub(crate) trait MarkCodegen {
    fn mark_codegen(&self, sess: Sess);
}

impl<T: MarkCodegen> MarkCodegen for Vec<T> {
    fn mark_codegen(&self, sess: Sess) {
        for element in self {
            element.mark_codegen(sess, workspace);
        }
    }
}

impl<T: MarkCodegen> MarkCodegen for Option<T> {
    fn mark_codegen(&self, sess: Sess) {
        if let Some(e) = self {
            e.mark_codegen(sess, workspace);
        }
    }
}

impl<T: MarkCodegen> MarkCodegen for Box<T> {
    fn mark_codegen(&self, sess: Sess) {
        self.as_ref().mark_codegen(sess, workspace);
    }
}
