use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::builtin::Builtin;
use super::object::Object;

pub type Env = Rc<RefCell<Environment>>;

pub fn new_env(outer: Option<Environment>) -> Env {
    let env = match outer {
        Some(env) => env,
        None => Environment::default(),
    };
    Rc::new(RefCell::new(env))
}

#[derive(Debug)]
pub struct Environment {
    store: HashMap<Rc<str>, Rc<Object>>,
    outer: Option<Env>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut env = Self {
            store: Default::default(),
            outer: Default::default(),
        };
        Builtin::register(&mut env);
        env
    }
}

impl Environment {
    pub fn new_enclosed(outer: &Env) -> Self {
        Self {
            store: Default::default(),
            outer: Some(Rc::clone(outer)),
        }
    }

    pub fn get(&self, ident: Rc<str>) -> Option<Rc<Object>> {
        self.store
            .get(&ident)
            .cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(ident))
    }

    pub fn set(&mut self, ident: Rc<str>, obj: Rc<Object>) {
        self.store.insert(ident, obj);
    }
}
