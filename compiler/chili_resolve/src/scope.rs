use chili_ast::workspace::BindingInfoId;
use ustr::UstrMap;

pub(crate) struct Scope {
    pub(crate) name: String,
    pub(crate) bindings: UstrMap<BindingInfoId>,
}

impl Scope {
    pub(crate) fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            bindings: Default::default(),
        }
    }
}
