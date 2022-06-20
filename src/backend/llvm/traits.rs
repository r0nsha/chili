use inkwell::{
    types::{AnyType, AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValue, BasicValueEnum, InstructionOpcode},
};
use ustr::Ustr;

use crate::ast::{ast::FunctionSig, pattern::SymbolPattern, ty::FunctionTypeKind};

pub(super) trait IsALoadInst {
    fn is_a_load_inst(&self) -> bool;
}

impl<'ctx> IsALoadInst for BasicValueEnum<'ctx> {
    fn is_a_load_inst(&self) -> bool {
        self.as_instruction_value().map_or(false, |inst| {
            matches!(inst.get_opcode(), InstructionOpcode::Load)
        })
    }
}

impl<'ctx> IsALoadInst for AnyValueEnum<'ctx> {
    fn is_a_load_inst(&self) -> bool {
        if self.is_instruction_value() {
            matches!(
                self.into_instruction_value().get_opcode(),
                InstructionOpcode::Load
            )
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

pub(super) trait LlvmName {
    fn llvm_name(&self, module_name: impl AsRef<str>) -> String;
}

impl LlvmName for FunctionSig {
    fn llvm_name(&self, module_name: impl AsRef<str>) -> String {
        // let module_name
        match &self.kind {
            FunctionTypeKind::Extern { .. } => self.name.to_string(),
            _ => self.name.llvm_name(module_name),
        }
    }
}

impl LlvmName for SymbolPattern {
    fn llvm_name(&self, module_name: impl AsRef<str>) -> String {
        self.symbol.llvm_name(module_name)
    }
}

impl LlvmName for Ustr {
    fn llvm_name(&self, module_name: impl AsRef<str>) -> String {
        let module_name = module_name.as_ref();

        if module_name == "" {
            format!("root#{}", self)
        } else {
            format!("{}.{}", module_name, self)
        }
    }
}
