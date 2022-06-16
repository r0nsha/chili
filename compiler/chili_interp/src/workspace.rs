use crate::vm::value::Value;

pub struct WorkspaceValue<'a> {
    name: &'a str,
    build_options: BuildOptionsValue<'a>,
}

impl<'a> From<Value> for WorkspaceValue<'a> {
    fn from(value: Value) -> Self {
        let aggregate = value.into_aggregate();
        todo!("{}", aggregate)
        // match value {
        //     Value::Aggregate(_) => todo!("yay!"),
        //     _ => panic!(),
        // }
    }
}

pub struct BuildOptionsValue<'a> {
    input_file: &'a str,
    output_file: &'a str,
}

impl<'a> From<Value> for BuildOptionsValue<'a> {
    fn from(value: Value) -> Self {
        let aggregate = value.into_aggregate();
        todo!("{}", aggregate)
        // match value {
        //     Value::Aggregate(_) => todo!("yay!"),
        //     _ => panic!(),
        // }
    }
}

#[repr(u8)]
pub enum BuildTargetValue {
    Auto = 0,
    Linux = 1,
    Windows = 2,
}

impl From<Value> for BuildTargetValue {
    fn from(value: Value) -> Self {
        match value.into_u8() {
            0 => Self::Auto,
            1 => Self::Linux,
            2 => Self::Windows,
            x => panic!("{}", x),
        }
    }
}

#[repr(u8)]
pub enum OptLevelValue {
    Debug = 0,
    Release = 1,
}

impl From<Value> for OptLevelValue {
    fn from(value: Value) -> Self {
        match value.into_u8() {
            0 => Self::Debug,
            1 => Self::Release,
            x => panic!("{}", x),
        }
    }
}
