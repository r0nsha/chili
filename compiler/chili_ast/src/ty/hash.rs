use super::*;

impl Hash for StructTy {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.binding_info_id.hash(state);
        self.fields.hash(state);
        self.kind.hash(state);
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
