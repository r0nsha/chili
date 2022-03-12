use chili_ast::workspace::Workspace;

pub(crate) trait Infer<'w> {
    fn infer(&mut self, workspace: Workspace<'w>);
}
