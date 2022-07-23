use super::bytecode::Inst;

#[derive(Debug, Clone)]
pub struct CompiledCode {
    pub instructions: Vec<Inst>,
    pub locals: u16,
}

impl Default for CompiledCode {
    fn default() -> Self {
        Self::new()
    }
}

impl CompiledCode {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            locals: 0,
        }
    }

    #[inline]
    pub fn push(&mut self, inst: Inst) -> usize {
        self.instructions.push(inst);
        self.instructions.len() - 1
    }
}
