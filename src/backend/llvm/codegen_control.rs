use super::codegen::{Codegen, FunctionState, Generator, LoopBlock};
use crate::hir;
use inkwell::values::BasicValueEnum;

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Control {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            hir::Control::If(x) => x.codegen(generator, state),
            hir::Control::While(x) => x.codegen(generator, state),
            hir::Control::Return(x) => x.codegen(generator, state),
            hir::Control::Break(_) => {
                let exit_block = state.loop_blocks.last().unwrap().exit;
                generator.builder.build_unconditional_branch(exit_block);
                generator.const_unit()
            }
            hir::Control::Continue(_) => {
                let head_block = state.loop_blocks.last().unwrap().head;
                generator.builder.build_unconditional_branch(head_block);
                generator.const_unit()
            }
        }
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::If {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        generator.gen_conditional(
            state,
            |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
                self.condition.codegen(generator, state).into_int_value()
            },
            |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| self.then.codegen(generator, state),
            self.otherwise.as_ref().map(|otherwise| {
                |generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>| {
                    otherwise.codegen(generator, state)
                }
            }),
        )
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::While {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let loop_head = generator.append_basic_block(state, "loop_head");
        let loop_body = generator.append_basic_block(state, "loop_body");
        let loop_exit = generator.append_basic_block(state, "loop_exit");

        generator.builder.build_unconditional_branch(loop_head);
        generator.start_block(state, loop_head);

        let condition = self.condition.codegen(generator, state).into_int_value();

        generator
            .builder
            .build_conditional_branch(condition, loop_body, loop_exit);

        generator.start_block(state, loop_body);

        state.loop_blocks.push(LoopBlock {
            head: loop_head,
            exit: loop_exit,
        });

        self.body.codegen(generator, state);

        state.loop_blocks.pop();

        if generator.current_block().get_terminator().is_none() {
            generator.builder.build_unconditional_branch(loop_head);
        }

        generator.start_block(state, loop_exit);

        generator.const_unit()
    }
}

impl<'g, 'ctx> Codegen<'g, 'ctx> for hir::Return {
    fn codegen(&self, generator: &mut Generator<'g, 'ctx>, state: &mut FunctionState<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.value.codegen(generator, state);
        generator.gen_return(state, Some(value));
        generator.const_unit()
    }
}
