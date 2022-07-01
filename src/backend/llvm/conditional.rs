use super::codegen::{FunctionState, Generator};
use crate::ast;
use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValueEnum, IntValue},
};

impl<'g, 'ctx> Generator<'g, 'ctx> {
    pub fn gen_if_expr(
        &mut self,
        state: &mut FunctionState<'ctx>,
        if_: &ast::If,
    ) -> BasicValueEnum<'ctx> {
        todo!();
        // let condition = self.gen_expr(state, &if_.condition, true).into_int_value();

        // let then = |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
        //     generator.gen_expr(state, &if_.then, true)
        // };

        // let else_ = if let Some(otherwise) = &if_.otherwise {
        //     Some(
        //         |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
        //             generator.gen_expr(state, otherwise, true)
        //         },
        //     )
        // } else {
        //     None
        // };

        // self.gen_conditional(state, condition, then, else_)
    }

    pub fn gen_conditional<
        Then: FnOnce(&mut Generator<'g, 'ctx>, &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx>,
        Else: FnOnce(&mut Generator<'g, 'ctx>, &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx>,
    >(
        &mut self,
        state: &mut FunctionState<'ctx>,
        condition: IntValue<'ctx>,
        then: Then,
        otherwise: Option<Else>,
    ) -> BasicValueEnum<'ctx> {
        let then_block = self.append_basic_block(state, "if_then");
        let else_block = self.append_basic_block(state, "if_else");

        let mut merge_block: Option<BasicBlock<'ctx>> = None;

        self.builder
            .build_conditional_branch(condition, then_block, else_block);

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

        self.start_block(state, else_block);

        let else_value = if let Some(else_) = otherwise {
            else_(self, state)
        } else {
            self.unit_value()
        };

        let else_has_terminator = self.current_block().get_terminator().is_some();
        let else_block = self.current_block();

        if !then_has_terminator && !else_has_terminator {
            let then_type = then_value.get_type();
            let else_value = self.build_transmute(state, else_value, then_type);

            if merge_block.is_none() {
                merge_block = Some(self.append_basic_block(state, "if_merge"));
            }
            self.builder
                .build_unconditional_branch(merge_block.unwrap());
            if let Some(merge_block) = merge_block {
                self.start_block(state, merge_block);
            }

            let phi = self.builder.build_phi(then_type, "if_result");
            phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
            phi.as_basic_value()
        } else {
            if !else_has_terminator {
                if merge_block.is_none() {
                    merge_block = Some(self.append_basic_block(state, "if_merge"));
                }
                self.builder
                    .build_unconditional_branch(merge_block.unwrap());
            }

            if let Some(merge_block) = merge_block {
                self.start_block(state, merge_block);
            }

            match (then_has_terminator, else_has_terminator) {
                (true, true) => self.unit_value(),
                (true, false) => else_value,
                (false, true) => then_value,
                _ => panic!(),
            }
        }
    }
}
