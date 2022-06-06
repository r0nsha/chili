use ustr::{ustr, Ustr};

pub const BUILTIN_FIELD_LEN: &str = "len";
pub const BUILTIN_FIELD_DATA: &str = "data";

pub fn default_iter_name() -> Ustr {
    ustr("it")
}

pub fn default_index_name() -> Ustr {
    ustr("it_index")
}

pub fn implicit_fn_param_name() -> Ustr {
    ustr("it")
}
