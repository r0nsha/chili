use chili_ast::workspace::BindingInfoId;
use ustr::UstrMap;

pub(crate) struct Scope {
    pub(crate) bindings: UstrMap<BindingInfoId>,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Self {
            bindings: Default::default(),
        }
    }
}
