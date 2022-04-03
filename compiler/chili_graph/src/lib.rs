use std::collections::HashSet;

use chili_ast::{
    ast,
    workspace::{BindingInfoId, Workspace},
};

pub fn mark_codegen_path(ast: &ast::TypedAst, workspace: &mut Workspace) {
    let mut marker = Marker {
        marked: HashSet::new(),
    };
    ast.mark_codegen_path(&mut marker, workspace)
}

struct Marker {
    marked: HashSet<BindingInfoId>,
}

impl Marker {
    fn mark(&mut self, id: BindingInfoId) {
        self.marked.insert(id);
    }

    fn collect(&mut self) -> HashSet<BindingInfoId> {
        let res = self.marked.clone();
        self.marked.clear();
        res
    }
}

trait MarkCodegenPath {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace);
}

impl<T: MarkCodegenPath> MarkCodegenPath for Vec<T> {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace) {
        for element in self {
            element.mark_codegen_path(marker, workspace);
        }
    }
}

impl<T: MarkCodegenPath> MarkCodegenPath for Option<T> {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace) {
        if let Some(e) = self {
            e.mark_codegen_path(marker, workspace);
        }
    }
}

impl<T: MarkCodegenPath> MarkCodegenPath for Box<T> {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace) {
        self.as_ref().mark_codegen_path(marker, workspace)
    }
}

impl MarkCodegenPath for ast::TypedAst {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace) {
        for import in self.imports.values() {
            import.mark_codegen_path(marker, workspace);
        }

        for binding in self.bindings.values() {
            binding.mark_codegen_path(marker, workspace);
        }
    }
}

impl MarkCodegenPath for ast::Import {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace) {}
}

impl MarkCodegenPath for ast::Binding {
    fn mark_codegen_path(&self, marker: &mut Marker, workspace: &mut Workspace) {}
}
