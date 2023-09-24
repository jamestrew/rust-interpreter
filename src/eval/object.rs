use std::rc::Rc;
#[derive(Debug, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Return(Rc<Object>),
    Error(String),
    Nil,
}

impl Object {
    pub fn new_bool(val: bool) -> Object {
        if val {
            TRUE
        } else {
            FALSE
        }
    }

    pub fn bool_value(&self) -> anyhow::Result<bool> {
        match self {
            Object::Int(val) => Ok(*val != 0),
            Object::Bool(val) => Ok(*val),
            Object::String(val) => Ok(!val.is_empty()),
            Object::Nil => Ok(false),
            _ => Err(anyhow::anyhow!("not a bool value")),
        }
    }

    pub fn new_eror(err: anyhow::Error) -> Object {
        Self::Error(err.to_string())
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Object::Int(_) => "INTEGER",
            Object::Bool(_) => "BOOLEAN",
            Object::String(_) => "STRING",
            Object::Return(_) => "RETURN",
            Object::Error(_) => "ERROR",
            Object::Nil => "NIL",
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Return(value) => write!(f, "{}", value),
            Object::Error(value) => write!(f, "{}", value),
            Object::Nil => write!(f, "nil"),
        }
    }
}

pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);
pub const NIL: Object = Object::Nil;
