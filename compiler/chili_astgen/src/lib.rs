mod multi_threaded;
mod single_threaded;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;

#[derive(Debug, Clone, Copy)]
pub struct AstGenerationStats {
    pub total_lines: usize,
}

pub type AstGenerationResult = DiagnosticResult<(Vec<ast::Ast>, AstGenerationStats)>;

#[derive(Clone, Copy)]
pub enum AstGenerationMode {
    SingleThreaded,
    MultiThreaded,
}

pub fn generate_ast(workspace: &mut Workspace, mode: AstGenerationMode) -> AstGenerationResult {
    match mode {
        AstGenerationMode::SingleThreaded => single_threaded::AstGenerator::new(workspace).start(),
        AstGenerationMode::MultiThreaded => multi_threaded::AstGenerator::new(workspace).start(),
    }
}
