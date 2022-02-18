use ustr::{ustr, Ustr, UstrMap};

#[derive(Debug, Clone)]
pub struct Env<T> {
    scopes: Vec<Scope<T>>,
}

#[derive(Debug, Clone)]
pub struct Scope<T> {
    pub name: Ustr,
    pub symbols: UstrMap<T>,
}

impl<T> Scope<T> {
    pub fn new(name: Ustr) -> Self {
        Self {
            name,
            symbols: UstrMap::default(),
        }
    }
}

impl<T> Default for Scope<T> {
    fn default() -> Self {
        Self::new(ustr("_"))
    }
}

impl<T> Env<T> {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn push_named_scope(&mut self, name: Ustr) {
        self.scopes.push(Scope::new(name));
    }

    pub fn pop_scope(&mut self) -> Option<Scope<T>> {
        self.scopes.pop()
    }

    pub fn get(&self, symbol: &Ustr) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.symbols.get(symbol) {
                return Some(decl);
            }
        }

        None
    }

    pub fn get_with_depth(&self, symbol: &Ustr) -> Option<(&T, usize)> {
        let mut depth = self.depth();
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.symbols.get(symbol) {
                return Some((decl, depth));
            }

            depth -= 1;
        }

        None
    }

    pub fn insert(&mut self, symbol: Ustr, value: T) -> Option<T> {
        self.scopes
            .last_mut()
            .unwrap()
            .symbols
            .insert(symbol, value)
    }

    pub fn remove(&mut self, symbol: Ustr) -> Option<T> {
        self.scopes.last_mut().unwrap().symbols.remove(&symbol)
    }

    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    pub fn scope_name(&self) -> String {
        self.scopes
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<&str>>()
            .join(".")
    }

    pub fn clear(&mut self) {
        self.scopes.clear()
    }

    pub fn print_symbols(&self) {
        if let Some(scope) = self.scopes.last() {
            println!(
                "[{}]",
                scope
                    .symbols
                    .keys()
                    .map(|k| k.as_str())
                    .collect::<Vec<&str>>()
                    .join(", ")
            );
        } else {
            println!("no scope")
        }
    }
}
