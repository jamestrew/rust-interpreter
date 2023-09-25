use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::object::Object;
use crate::ast::Identifier;

pub type Env = Rc<RefCell<Environment>>;

pub fn new_env(outer: Option<Environment>) -> Env {
    let env = match outer {
        Some(env) => env,
        None => Environment::default(),
    };
    Rc::new(RefCell::new(env))
}

#[derive(Debug, Default)]
pub struct Environment {
    store: HashMap<Identifier, Rc<Object>>,
    outer: Option<Env>,
}

impl Environment {
    pub fn new_enclosed(outer: &Env) -> Self {
        Self {
            store: Default::default(),
            outer: Some(Rc::clone(outer)),
        }
    }

    pub fn get(&self, ident: &Identifier) -> Option<Rc<Object>> {
        self.store
            .get(ident)
            .cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(ident))
    }

    pub fn set(&mut self, ident: &Identifier, obj: Rc<Object>) {
        self.store.insert(ident.clone(), obj);
    }
}
