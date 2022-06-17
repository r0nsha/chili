use super::vm::value::Value;

#[derive(Debug, Clone)]
pub struct WorkspaceValue<'a> {
    pub name: &'a str,
    pub build_options: BuildOptionsValue<'a>,
}

impl<'a> From<&'a Value> for WorkspaceValue<'a> {
    fn from(value: &'a Value) -> Self {
        let aggregate = value.as_aggregate();

        let name = unsafe { aggregate.elements[0].as_aggregate().as_str() };
        let build_options = BuildOptionsValue::from(&aggregate.elements[1]);

        Self {
            name,
            build_options,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuildOptionsValue<'a> {
    pub input_file: &'a str,
    pub output_file: &'a str,
    pub target: BuildTargetValue,
    pub opt_level: OptLevelValue,
    pub run_executable: bool,
}

impl<'a> From<&'a Value> for BuildOptionsValue<'a> {
    fn from(value: &'a Value) -> Self {
        let aggregate = value.as_aggregate();

        let input_file = unsafe { aggregate.elements[0].as_aggregate().as_str() };
        let output_file = unsafe { aggregate.elements[1].as_aggregate().as_str() };
        let target = BuildTargetValue::from(&aggregate.elements[2]);
        let opt_level = OptLevelValue::from(&aggregate.elements[3]);
        let run_executable = *aggregate.elements[4].as_bool();

        Self {
            input_file,
            output_file,
            target,
            opt_level,
            run_executable,
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
pub enum OptLevelValue {
    Debug = 0,
    Release = 1,
}

impl From<&Value> for OptLevelValue {
    fn from(value: &Value) -> Self {
        match value.as_u8() {
            0 => Self::Debug,
            1 => Self::Release,
            x => panic!("{}", x),
        }
    }
}
