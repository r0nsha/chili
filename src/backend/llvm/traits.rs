use inkwell::{
    types::{AnyType, AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValue, BasicValueEnum, InstructionOpcode},
};

pub(super) trait IsALoadInst {
    fn is_a_load_inst(&self) -> bool;
}

impl<'ctx> IsALoadInst for BasicValueEnum<'ctx> {
    fn is_a_load_inst(&self) -> bool {
        self.as_instruction_value()
            .map(|inst| matches!(inst.get_opcode(), InstructionOpcode::Load))
            .unwrap_or_default()
    }
}

impl<'ctx> IsALoadInst for AnyValueEnum<'ctx> {
    fn is_a_load_inst(&self) -> bool {
        if self.is_instruction_value() {
            matches!(self.into_instruction_value().get_opcode(), InstructionOpcode::Load)
        } else {
            false
        }
    }
}

pub(super) trait IsAggregateType {
    fn is_aggregate_type(&self) -> bool;
}

impl<'ctx> IsAggregateType for AnyTypeEnum<'ctx> {
    fn is_aggregate_type(&self) -> bool {
        match self {
            AnyTypeEnum::ArrayType(_) | AnyTypeEnum::StructType(_) => true,
            _ => false,
        }
    }
}

impl<'ctx> IsAggregateType for BasicTypeEnum<'ctx> {
    fn is_aggregate_type(&self) -> bool {
        self.as_any_type_enum().is_aggregate_type()
    }
}
