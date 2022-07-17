use super::{
    vm::{byte_seq::GetValue, value::Value},
    WORD_SIZE,
};
use crate::types::offset_of::OffsetOf;

#[derive(Debug, Clone)]
pub struct WorkspaceValue {
    pub name: String,
    pub build_options: BuildOptionsValue,
}

impl From<&Value> for WorkspaceValue {
    fn from(value: &Value) -> Self {
        let buf = value.as_buffer();
        let struct_field_types = &buf.ty.as_struct().fields;

        let name = buf.bytes.offset(0).get_value(&struct_field_types[0].ty);

        let build_options = BuildOptionsValue::from(
            &buf.bytes
                .offset(buf.ty.offset_of(1, WORD_SIZE))
                .get_value(&struct_field_types[1].ty),
        );

        Self {
            name: name.as_buffer().as_str().to_string(),
            build_options,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuildOptionsValue {
    pub input_file: String,
    pub output_file: String,
    pub target: BuildTargetValue,
    pub optimization_level: OptimizationLevelValue,
}

impl From<&Value> for BuildOptionsValue {
    fn from(value: &Value) -> Self {
        let buf = value.as_buffer();
        let field_types = &buf.ty.as_struct().fields;

        let input_file = buf
            .bytes
            .offset(0)
            .get_value(&field_types[0].ty)
            .as_buffer()
            .as_str()
            .to_string();

        let output_file = buf
            .bytes
            .offset(buf.ty.offset_of(1, WORD_SIZE))
            .get_value(&field_types[1].ty)
            .as_buffer()
            .as_str()
            .to_string();

        let target = BuildTargetValue::from(
            &buf.bytes
                .offset(buf.ty.offset_of(2, WORD_SIZE))
                .get_value(&field_types[2].ty),
        );
        let optimization_level = OptimizationLevelValue::from(
            &buf.bytes
                .offset(buf.ty.offset_of(3, WORD_SIZE))
                .get_value(&field_types[3].ty),
        );

        Self {
            input_file,
            output_file,
            target,
            optimization_level,
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum BuildTargetValue {
    Auto = 0,
    Linux = 1,
    Windows = 2,
}

impl From<&Value> for BuildTargetValue {
    fn from(value: &Value) -> Self {
        match value.as_u8() {
            0 => Self::Auto,
            1 => Self::Linux,
            2 => Self::Windows,
            x => panic!("{}", x),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum OptimizationLevelValue {
    Debug = 0,
    Release = 1,
}

impl From<&Value> for OptimizationLevelValue {
    fn from(value: &Value) -> Self {
        match value.as_u8() {
            0 => Self::Debug,
            1 => Self::Release,
            x => panic!("{}", x),
        }
    }
}
