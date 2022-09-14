pub const SELF: &str = "self";
pub const SUPER: &str = "super";

pub const UNIT: &str = "unit";
pub const BOOL: &str = "bool";
pub const I8: &str = "i8";
pub const I16: &str = "i16";
pub const I32: &str = "i32";
pub const I64: &str = "i64";
pub const INT: &str = "int";
pub const U8: &str = "u8";
pub const U16: &str = "u16";
pub const U32: &str = "u32";
pub const U64: &str = "u64";
pub const UINT: &str = "uint";
pub const F16: &str = "f16";
pub const F32: &str = "f32";
pub const F64: &str = "f64";
pub const FLOAT: &str = "float";
pub const STR: &str = "str";
pub const NEVER: &str = "never";

pub const TRACK_CALLER_LOCATION_PARAM: &str = "track_caller@location";

pub fn is_implicitly_generated_param(name: &str) -> bool {
    name == TRACK_CALLER_LOCATION_PARAM
}
