use std::rc::Rc;

use super::builtin::Builtin;
use super::environment::Env;
use crate::ast;

#[derive(Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Return(Rc<Object>),
    Function(Function),
    Error(String),
    Builtin(Builtin),
    Array(Vec<Object>),
    Nil,
    Empty,
}

pub const TRUE: Object = Object::Bool(true);
pub const FALSE: Object = Object::Bool(false);
pub const NIL: Object = Object::Nil;
pub const EMPTY: Object = Object::Empty;

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
            Object::Function(_) => "FUNCTION",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Error(_) => "ERROR",
            Object::Nil => "NIL",
            Object::Empty => "EMPTY",
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Return(value) => write!(f, "{}", value),
            Object::Function(value) => write!(f, "{}", value),
            Object::Builtin(value) => write!(f, "{}", value),
            Object::Array(val) => write!(
                f,
                "[{}]",
                val.iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Object::Error(value) => write!(f, "{}", value),
            Object::Nil => write!(f, "nil"),
            Object::Empty => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    params: Vec<ast::Identifier>,
    body: ast::Block,
    env: Env,
}

impl Function {
    pub fn new(func: &ast::Function, env: &Env) -> Self {
        Self {
            params: func.params().to_vec(),
            body: func.body().clone(),
            env: env.clone(),
        }
    }

    pub fn params(&self) -> &[ast::Identifier] {
        &self.params
    }

    pub fn body(&self) -> &ast::Block {
        &self.body
    }

    pub fn env(&self) -> &Env {
        &self.env
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (idx, param) in self.params.iter().enumerate() {
            if idx != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ") \n{}\n", self.body)
    }
}
