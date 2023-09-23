#[cfg(test)]
mod test;

use std::fmt::Display;

use crate::ast::*;
use crate::lexer::Token;

#[derive(Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Return(Box<Object>),
    Error(String),
    Nil,
}

impl Object {
    fn new_bool(val: bool) -> Object {
        if val {
            TRUE
        } else {
            FALSE
        }
    }

    fn bool_value(&self) -> anyhow::Result<bool> {
        match self {
            Object::Int(val) => Ok(*val != 0),
            Object::Bool(val) => Ok(*val),
            Object::String(val) => Ok(!val.is_empty()),
            Object::Nil => Ok(false),
            _ => Err(anyhow::anyhow!("not a bool value")),
        }
    }

    fn new_eror(err: anyhow::Error) -> Object {
        Self::Error(err.to_string())
    }

    fn type_str(&self) -> &'static str {
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

impl Display for Object {
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

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NIL: Object = Object::Nil;

pub fn eval_program(program: &Program) -> Object {
    let mut ret = NIL;
    for stmt in &program.statements {
        let stmt = eval_statement(stmt);
        match stmt {
            Ok(stmt) => {
                match stmt {
                    Object::Return(val) => return *val,
                    Object::Error(_) => return stmt,
                    _ => (),
                }
                ret = stmt;
            }
            Err(err) => return Object::new_eror(err),
        }
    }
    ret
}

fn eval_statement(stmt: &Statement) -> anyhow::Result<Object> {
    match stmt {
        Statement::Let(_) => todo!(),
        Statement::Return(val) => eval_return(val),
        Statement::Block(val) => eval_block(val),
        Statement::ExpressionStatement(val) => eval_expression(val),
    }
}

fn eval_block(stmt: &Block) -> anyhow::Result<Object> {
    let mut ret = NIL;
    for stmt in stmt.statements() {
        ret = eval_statement(stmt)?;
        if let Object::Return(_) = ret {
            return Ok(ret);
        }
    }
    Ok(ret)
}

fn eval_return(stmt: &Return) -> anyhow::Result<Object> {
    let expr = eval_expression(stmt.value())?;
    Ok(Object::Return(Box::new(expr)))
}

fn eval_expression(expr: &Expression) -> anyhow::Result<Object> {
    match expr {
        Expression::Identifier(_val) => todo!(),
        Expression::Primative(val) => Ok(eval_primative(val)),
        Expression::StringLiteral(val) => Ok(Object::String(val.to_string())),
        Expression::Prefix(val) => eval_prefix(val),
        Expression::Infix(val) => eval_infix(val),
        Expression::If(val) => eval_if(val),
    }
}

fn eval_primative(expr: &Primative) -> Object {
    match expr {
        Primative::Int(val) => Object::Int(*val),
        Primative::Bool(val) => Object::new_bool(*val),
    }
}

fn eval_prefix(expr: &Prefix) -> anyhow::Result<Object> {
    match expr.operator() {
        Token::Bang => eval_bang_prefix(expr.right()),
        Token::Minus => eval_minus_prefix(expr.right()),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_bang_prefix(right: &Expression) -> anyhow::Result<Object> {
    let right = eval_expression(right)?;
    Ok(Object::new_bool(!right.bool_value()?))
}

fn eval_minus_prefix(right: &Expression) -> anyhow::Result<Object> {
    let right = eval_expression(right)?;
    if let Object::Int(value) = right {
        return Ok(Object::Int(-value));
    }
    Err(anyhow::anyhow!("unknown operator: -{}", right.type_str()))
}

fn eval_infix(expr: &Infix) -> anyhow::Result<Object> {
    let left = eval_expression(expr.left())?;
    let right = eval_expression(expr.right())?;
    let op = expr.operator();

    let ret = match (&left, &right) {
        (Object::Int(left), Object::Int(right)) => eval_integer_infix(*left, *right, op)?,
        (Object::String(left), Object::String(right)) => eval_string_infix(left, right, op),
        (Object::Bool(left), Object::Bool(right)) => eval_bool_infix(*left, *right, op)?,
        _ => {
            return Err(anyhow::anyhow!(
                "type mismatch: {} {} {}",
                left.type_str(),
                expr.operator_str(),
                right.type_str()
            ))
        }
    };
    Ok(ret)
}

fn eval_integer_infix(left: i64, right: i64, operator: &Token) -> anyhow::Result<Object> {
    let ret = match operator {
        Token::Minus => Object::Int(left - right),
        Token::Plus => Object::Int(left + right),
        Token::Asterisk => Object::Int(left * right),
        Token::ForwardSlash => {
            if right == 0 {
                return Err(anyhow::anyhow!("Error: divide by zero"));
            }
            Object::Int(left / right)
        }
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        Token::LT => Object::new_bool(left < right),
        Token::GT => Object::new_bool(left > right),
        token => Object::Error(format!("unknown operator: INTEGER {} INTEGER", token)),
    };
    Ok(ret)
}

fn eval_string_infix(left: &str, right: &str, operator: &Token) -> Object {
    match operator {
        Token::Plus => Object::String(format!("{}{}", left, right)),
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        token => Object::Error(format!("unknown operator: STRING {} STRING", token)),
    }
}

fn eval_bool_infix(left: bool, right: bool, operator: &Token) -> anyhow::Result<Object> {
    let ret = match operator {
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        token => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", token)),
    };
    Ok(ret)
}

fn eval_if(expr: &If) -> anyhow::Result<Object> {
    let condition = eval_expression(expr.condition())?;

    if condition.bool_value()? {
        return eval_block(expr.consequence());
    }

    if let Some(alternative) = expr.alternative() {
        return eval_block(alternative);
    }

    Ok(NIL)
}
