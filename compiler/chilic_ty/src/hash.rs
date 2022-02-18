use crate::*;

impl Hash for StructTy {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // self.name.hash(state);
        self.qualified_name.hash(state);
        // self.fields.hash(state);
        // self.kind.hash(state);
    }
}

impl Hash for StructTyField {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // self.symbol.hash(state);
        self.ty.hash(state);
    }
}

impl Hash for FnTy {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.params.hash(state);
        self.ret.hash(state);
        self.variadic.hash(state);
        self.lib_name.hash(state);
    }
}

impl Hash for FnTyParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
    }
}
