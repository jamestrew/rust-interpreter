use std::collections::HashMap;

use super::object::Object;
use crate::ast::Identifier;

#[derive(Debug, Default)]
pub struct Environment {
    store: HashMap<Identifier, Object>,
}

impl Environment {
    pub fn get(&self, ident: &Identifier) -> Option<&Object> {
        self.store.get(ident)
    }

    pub fn set(&mut self, ident: Identifier, obj: Object) {
        self.store.insert(ident, obj);
    }
}
