use super::codegen::{Codegen, CodegenState};
use crate::span::Span;
use inkwell::{
    values::{IntValue, PointerValue},
    IntPredicate,
};

macro_rules! release_guard {
    ($cg: expr) => {{
        if $cg.workspace.build_options.opt_level.is_release() {
            return;
        }
    }};
}

impl<'w, 'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub fn gen_runtime_check_division_by_zero(
        &mut self,
        state: &mut CodegenState<'ctx>,
        divisor: IntValue<'ctx>,
        span: Span,
    ) {
        release_guard!(self);

        const NAME: &str = "__runtime_check_division_by_zero";
        let cond = self.builder.build_int_compare(
            IntPredicate::EQ,
            divisor,
            divisor.get_type().const_zero(),
            "",
        );
        let message = self.gen_global_str(NAME, "attempt to divide by zero", true);
        self.gen_conditional_panic(state, NAME, cond, message, span)
    }

    pub fn gen_runtime_check_null_pointer_deref(
        &mut self,
        state: &mut CodegenState<'ctx>,
        ptr: PointerValue<'ctx>,
        span: Span,
    ) {
        release_guard!(self);

        const NAME: &str = "__runtime_check_null_pointer_dereference";
        let cond = self.builder.build_is_null(ptr, "");
        let message = self.gen_global_str(NAME, "attempt to dereference a null pointer", true);
        self.gen_conditional_panic(state, NAME, cond, message, span)
    }

    pub fn gen_runtime_check_overflow(
        &mut self,
        state: &mut CodegenState<'ctx>,
        cond: IntValue<'ctx>,
        span: Span,
        op: &str,
    ) {
        release_guard!(self);

        let name = format!("__runtime_check_overflow_{}", op);
        let message = self.gen_global_str(&name, format!("attempt to {} with overflow", op), true);
        self.gen_conditional_panic(state, &name, cond, message, span);
    }

    pub fn gen_runtime_check_index_out_of_bounds(
        &mut self,
        state: &mut CodegenState<'ctx>,
        index: IntValue<'ctx>,
        len: IntValue<'ctx>,
        span: Span,
    ) {
        release_guard!(self);

        const NAME: &str = "__runtime_check_index_out_of_bounds";

        let message = self.gen_global_str(
            &NAME,
            "index out of bounds: the len is (len) but the index is (index)",
            true,
        );

        let is_lower_than_zero = self.builder.build_int_compare(
            IntPredicate::ULT,
            index,
            index.get_type().const_zero(),
            "",
        );

        let is_larger_than_len = self
            .builder
            .build_int_compare(IntPredicate::UGE, index, len, "");

        let cond = self
            .builder
            .build_or(is_lower_than_zero, is_larger_than_len, "");

        self.gen_conditional_panic(state, &NAME, cond, message, span);
    }

    pub fn gen_runtime_check_slice_end_before_start(
        &mut self,
        state: &mut CodegenState<'ctx>,
        low: IntValue<'ctx>,
        high: IntValue<'ctx>,
        span: Span,
    ) {
        release_guard!(self);

        const NAME: &str = "__runtime_check_slice_end_before_start";

        let message = self.gen_global_str(
            &NAME,
            "slice index starts at (start) but ends at (end)",
            true,
        );

        let cond = self
            .builder
            .build_int_compare(IntPredicate::ULT, high, low, "");

        self.gen_conditional_panic(state, &NAME, cond, message, span);
    }

    pub fn gen_runtime_check_slice_range_out_of_bounds(
        &mut self,
        state: &mut CodegenState<'ctx>,
        low: IntValue<'ctx>,
        high: IntValue<'ctx>,
        len: IntValue<'ctx>,
        span: Span,
    ) {
        release_guard!(self);

        const NAME: &str = "__runtime_check_slice_range_out_of_bounds";

        let message = self.gen_global_str(
            &NAME,
            "slice range (start)..(end) is out of range for slice of length (len)",
            true,
        );

        let is_low_less_than_zero =
            self.builder
                .build_int_compare(IntPredicate::ULT, low, low.get_type().const_zero(), "");

        let is_high_larger_than_len =
            self.builder
                .build_int_compare(IntPredicate::UGT, high, len, "");

        let cond = self
            .builder
            .build_or(is_low_less_than_zero, is_high_larger_than_len, "");

        self.gen_conditional_panic(state, &NAME, cond, message, span);
    }
}
