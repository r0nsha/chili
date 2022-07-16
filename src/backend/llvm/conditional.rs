use super::codegen::{FunctionState, Generator};
use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValueEnum, IntValue},
};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub(super) fn gen_conditional<
        Cond: FnOnce(&mut Generator<'g, 'ctx>, &mut FunctionState<'ctx>) -> IntValue<'ctx>,
        Then: FnOnce(&mut Generator<'g, 'ctx>, &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx>,
        Else: FnOnce(&mut Generator<'g, 'ctx>, &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx>,
    >(
        &mut self,
        state: &mut FunctionState<'ctx>,
        condition: Cond,
        then: Then,
        otherwise: Option<Else>,
    ) -> BasicValueEnum<'ctx> {
        let then_block = self.append_basic_block(state, "if_then");
        let otherwise_block = self.append_basic_block(state, "if_otherwise");

        let mut merge_block: Option<BasicBlock<'ctx>> = None;

        let condition = condition(self, state);

        self.builder
            .build_conditional_branch(condition, then_block, otherwise_block);

        self.start_block(state, then_block);

        let then_value = then(self, state);

        let then_value = if otherwise.is_some() {
            then_value
        } else {
            self.unit_value()
        };

        let then_has_terminator = self.current_block().get_terminator().is_some();
        if !then_has_terminator {
            if merge_block.is_none() {
                merge_block = Some(self.append_basic_block(state, "if_merge"));
            }
            self.builder
                .build_unconditional_branch(merge_block.unwrap());
        }

        let then_block = self.current_block();

        self.start_block(state, otherwise_block);

        let otherwise_value = if let Some(otherwise) = otherwise {
            otherwise(self, state)
        } else {
            self.unit_value()
        };

        let otherwise_has_terminator = self.current_block().get_terminator().is_some();
        let otherwise_block = self.current_block();

        if !then_has_terminator && !otherwise_has_terminator {
            let then_type = then_value.get_type();
            let otherwise_value = self.build_transmute(state, otherwise_value, then_type);

            if merge_block.is_none() {
                merge_block = Some(self.append_basic_block(state, "if_merge"));
            }
            self.builder
                .build_unconditional_branch(merge_block.unwrap());
            if let Some(merge_block) = merge_block {
                self.start_block(state, merge_block);
            }

            let phi = self.builder.build_phi(then_type, "if_result");
            phi.add_incoming(&[
                (&then_value, then_block),
                (&otherwise_value, otherwise_block),
            ]);
            phi.as_basic_value()
        } else {
            if !otherwise_has_terminator {
                if merge_block.is_none() {
                    merge_block = Some(self.append_basic_block(state, "if_merge"));
                }
                self.builder
                    .build_unconditional_branch(merge_block.unwrap());
            }

            if let Some(merge_block) = merge_block {
                self.start_block(state, merge_block);
            }

            match (then_has_terminator, otherwise_has_terminator) {
                (true, true) => self.unit_value(),
                (true, false) => otherwise_value,
                (false, true) => then_value,
                _ => panic!(),
            }
        }
    }
}
