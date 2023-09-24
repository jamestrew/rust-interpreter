use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::object::Object;
use crate::ast::Identifier;

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, Default)]
pub struct Environment {
    store: HashMap<Identifier, Rc<Object>>,
}

impl Environment {
    pub fn get(&self, ident: &Identifier) -> Option<Rc<Object>> {
        self.store.get(ident).cloned()
    }

    pub fn set(&mut self, ident: &Identifier, obj: Rc<Object>) {
        self.store.insert(ident.clone(), obj);
    }
}
