use super::codegen::{Codegen, Generator};
use crate::hir;
use inkwell::values::GlobalValue;

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn initialize_static(&mut self, global_value: GlobalValue<'ctx>, value: &hir::Node) {
        let prev_block = self.builder.get_insert_block();

        let mut state = self.startup_function_state.clone().unwrap();

        match state.current_block.get_first_instruction() {
            Some(inst) => self.builder.position_at(state.current_block, &inst),
            None => self.builder.position_at_end(state.current_block),
        }

        let value = value.codegen(self, &mut state);
        self.build_store(global_value.as_pointer_value(), value);

        if let Some(prev_block) = prev_block {
            self.builder.position_at_end(prev_block);
        }
    }
}
