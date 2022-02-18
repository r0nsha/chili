use crate::codegen::{Codegen, CodegenState};
use chilic_ir::expr::Expr;
use inkwell::{basic_block::BasicBlock, values::BasicValueEnum};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_if_expr(
        &mut self,
        state: &mut CodegenState<'ctx>,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Option<Box<Expr>>,
    ) -> BasicValueEnum<'ctx> {
        let cond = self.gen_expr(state, cond, true).into_int_value();

        let then_block = self.append_basic_block(state, "if_then");
        let else_block = self.append_basic_block(state, "if_else");

        let mut merge_block: Option<BasicBlock<'ctx>> = None;

        self.builder
            .build_conditional_branch(cond, then_block, else_block);

        self.start_block(state, then_block);

        let then_value = self.gen_expr(state, then_expr, true);

        let then_value = if else_expr.is_some() {
            then_value
        } else {
            self.gen_unit()
        };

        let then_has_terminator =
            self.current_block().get_terminator().is_some();
        if !then_has_terminator {
            if merge_block.is_none() {
                merge_block = Some(self.append_basic_block(state, "if_merge"));
            }
            self.builder
                .build_unconditional_branch(merge_block.unwrap());
        }

        let then_block = self.current_block();

        self.start_block(state, else_block);

        let else_value = if let Some(else_expr) = else_expr {
            self.gen_expr(state, else_expr, true)
        } else {
            self.gen_unit()
        };

        let else_has_terminator =
            self.current_block().get_terminator().is_some();
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
            phi.add_incoming(&[
                (&then_value, then_block),
                (&else_value, else_block),
            ]);
            phi.as_basic_value()
        } else {
            if !else_has_terminator {
                if merge_block.is_none() {
                    merge_block =
                        Some(self.append_basic_block(state, "if_merge"));
                }
                self.builder
                    .build_unconditional_branch(merge_block.unwrap());
            }

            if let Some(merge_block) = merge_block {
                self.start_block(state, merge_block);
            }

            match (then_has_terminator, else_has_terminator) {
                (true, true) => self.gen_unit(),
                (true, false) => else_value,
                (false, true) => then_value,
                _ => panic!(),
            }
        }
    }
    // pub(super) fn gen_conditional(
    //     &mut self,
    //     state: &mut CodegenState<'ctx>,
    //     cond: IntValue<'ctx>,
    //     then: Box<dyn FnMut() -> BasicValueEnum<'ctx>>,
    //     else_: Option<dyn FnMut() -> BasicValueEnum<'ctx>>,
    // ) -> BasicValueEnum<'ctx> {
    //     let then_block = self.append_basic_block(state, "if_then");
    //     let else_block = self.append_basic_block(state, "if_else");

    //     let mut merge_block: Option<BasicBlock<'ctx>> = None;

    //     self.builder
    //         .build_conditional_branch(cond, then_block, else_block);

    //     self.start_block(state, then_block);

    //     let then_value = then();

    //     let then_value = if else_.is_some() {
    //         then_value
    //     } else {
    //         self.gen_unit()
    //     };

    //     let then_has_terminator =
    //         self.current_block().get_terminator().is_some();
    //     if !then_has_terminator {
    //         if merge_block.is_none() {
    //             merge_block = Some(self.append_basic_block(state,
    // "if_merge"));         }
    //         self.builder
    //             .build_unconditional_branch(merge_block.unwrap());
    //     }

    //     let then_block = self.current_block();

    //     self.start_block(state, else_block);

    //     let else_value = if let Some(else_) = else_ {
    //         else_()
    //     } else {
    //         self.gen_unit()
    //     };

    //     let else_has_terminator =
    //         self.current_block().get_terminator().is_some();
    //     let else_block = self.current_block();

    //     if !then_has_terminator && !else_has_terminator {
    //         let then_type = then_value.get_type();
    //         let else_value = self.build_transmute(state, else_value,
    // then_type);

    //         if merge_block.is_none() {
    //             merge_block = Some(self.append_basic_block(state,
    // "if_merge"));         }
    //         self.builder
    //             .build_unconditional_branch(merge_block.unwrap());
    //         if let Some(merge_block) = merge_block {
    //             self.start_block(state, merge_block);
    //         }

    //         let phi = self.builder.build_phi(then_type, "if_result");
    //         phi.add_incoming(&[
    //             (&then_value, then_block),
    //             (&else_value, else_block),
    //         ]);
    //         phi.as_basic_value()
    //     } else {
    //         if !else_has_terminator {
    //             if merge_block.is_none() {
    //                 merge_block =
    //                     Some(self.append_basic_block(state, "if_merge"));
    //             }
    //             self.builder
    //                 .build_unconditional_branch(merge_block.unwrap());
    //         }

    //         if let Some(merge_block) = merge_block {
    //             self.start_block(state, merge_block);
    //         }

    //         match (then_has_terminator, else_has_terminator) {
    //             (true, true) => self.gen_unit(),
    //             (true, false) => else_value,
    //             (false, true) => then_value,
    //             _ => panic!(),
    //         }
    //     }
    // }
}
