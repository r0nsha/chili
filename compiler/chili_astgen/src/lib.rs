mod multi_threaded;
mod single_threaded;
mod util;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;

// use alloc_counter::{count_alloc, AllocCounterSystem};
// #[global_allocator]
// static A: AllocCounterSystem = AllocCounterSystem;

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
    // let (count, result) = count_alloc(|| match mode {
    //     AstGenerationMode::SingleThreaded => single_threaded::AstGenerator::new(workspace).start(),
    //     AstGenerationMode::MultiThreaded => multi_threaded::AstGenerator::new(workspace).start(),
    // });
    // println!(
    //     "alloc: {}\nrealloc: {}\ndealloc: {}",
    //     count.0, count.1, count.2
    // );
    // result
    match mode {
        AstGenerationMode::SingleThreaded => single_threaded::AstGenerator::new(workspace).start(),
        AstGenerationMode::MultiThreaded => multi_threaded::AstGenerator::new(workspace).start(),
    }
}
