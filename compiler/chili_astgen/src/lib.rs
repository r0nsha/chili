mod multi_threaded;
mod single_threaded;
mod util;

use chili_ast::{ast, workspace::Workspace};

#[derive(Debug, Clone, Copy)]
pub struct AstGenerationStats {
    pub total_lines: usize,
}

#[derive(Clone, Copy)]
pub enum AstGenerationMode {
    SingleThreaded,
    MultiThreaded,
}

pub type AstGenerationResult = Option<(Vec<ast::Ast>, AstGenerationStats)>;

pub fn generate_ast(workspace: &mut Workspace, mode: AstGenerationMode) -> AstGenerationResult {
    match mode {
        AstGenerationMode::SingleThreaded => single_threaded::AstGenerator::new(workspace).start(),
        AstGenerationMode::MultiThreaded => todo!(), //multi_threaded::AstGenerator::new(workspace).start(),
    }
}
