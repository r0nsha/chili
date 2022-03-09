use chili_ast::workspace::BindingInfoId;
use ustr::UstrMap;

pub(crate) struct Scope {
    pub(crate) name: String,
    pub(crate) bindings: UstrMap<ScopeSymbol<BindingInfoId>>,
}

impl Scope {
    pub(crate) fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            bindings: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ScopeSymbol<Id: Copy> {
    pub(crate) id: Id,
    shadowable: bool,
}

impl<Id: Copy> ScopeSymbol<Id> {
    pub(crate) fn persistent(id: Id) -> Self {
        Self {
            id,
            shadowable: false,
        }
    }

    pub(crate) fn shadowable(id: Id) -> Self {
        Self {
            id,
            shadowable: true,
        }
    }

    pub(crate) fn is_shadowable(&self) -> bool {
        self.shadowable
    }
}
