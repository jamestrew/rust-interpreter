mod environment;
mod object;
#[cfg(test)]
mod test;

use std::rc::Rc;

use object::*;

use self::environment::Env;
use crate::ast::*;
use crate::lexer::Token;

type ObjResult = anyhow::Result<Rc<Object>>;

// not sure about returning Object vs ObjResult here
pub fn eval_program(program: &Program, env: &Env) -> ObjResult {
    let mut ret = NIL.into();
    for stmt in &program.statements {
        let stmt: anyhow::Result<Rc<Object>> = eval_statement(stmt, env);
        match stmt {
            Ok(stmt) => {
                let obj = stmt.as_ref();
                match obj {
                    Object::Return(val) => return Ok(val.clone()),
                    Object::Error(_) => return Ok(stmt),
                    _ => (),
                }
                ret = stmt;
            }
            Err(err) => return Ok(Object::new_eror(err).into()),
        }
    }
    Ok(ret)
}

fn eval_statement(stmt: &Statement, env: &Env) -> ObjResult {
    match stmt {
        Statement::Let(val) => eval_let(val, env),
        Statement::Return(val) => eval_return(val, env),
        Statement::Block(val) => eval_block(val, env),
        Statement::ExpressionStatement(val) => eval_expression(val, env),
    }
}

fn eval_let(stmt: &Let, env: &Env) -> ObjResult {
    let value = eval_expression(stmt.value(), env)?;
    env.borrow_mut().set(stmt.name(), value.clone());
    Ok(value)
}

fn eval_block(stmt: &Block, env: &Env) -> ObjResult {
    let mut ret = NIL.into();
    for stmt in stmt.statements() {
        ret = eval_statement(stmt, env)?;
        if let Object::Return(_) = ret.as_ref() {
            return Ok(ret);
        }
    }
    Ok(ret)
}

fn eval_return(stmt: &Return, env: &Env) -> ObjResult {
    let expr = eval_expression(stmt.value(), env)?;
    Ok(Object::Return(expr.clone()).into())
}

fn eval_expression(expr: &Expression, env: &Env) -> ObjResult {
    match expr {
        Expression::Identifier(val) => eval_identifier(val, env),
        Expression::Primative(val) => Ok(eval_primative(val)),
        Expression::StringLiteral(val) => Ok(Object::String(val.to_string()).into()),
        Expression::Prefix(val) => eval_prefix(val, env),
        Expression::Infix(val) => eval_infix(val, env),
        Expression::If(val) => eval_if(val, env),
        Expression::Function(_val) => todo!(),
        Expression::Call(_val) => todo!(),
    }
}

fn eval_identifier(expr: &Identifier, env: &Env) -> ObjResult {
    match env.borrow().get(expr) {
        Some(obj) => Ok(obj.clone()),
        None => Err(anyhow::anyhow!("identifier not found: {}", expr)),
    }
}

fn eval_primative(expr: &Primative) -> Rc<Object> {
    match expr {
        Primative::Int(val) => Object::Int(*val),
        Primative::Bool(val) => Object::new_bool(*val),
    }
    .into()
}

fn eval_prefix(expr: &Prefix, env: &Env) -> ObjResult {
    match expr.operator() {
        Token::Bang => eval_bang_prefix(expr.right(), env),
        Token::Minus => eval_minus_prefix(expr.right(), env),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_bang_prefix(right: &Expression, env: &Env) -> ObjResult {
    let right = eval_expression(right, env)?;
    Ok(Object::new_bool(!right.bool_value()?).into())
}

fn eval_minus_prefix(right: &Expression, env: &Env) -> ObjResult {
    let right = eval_expression(right, env)?;
    if let Object::Int(value) = right.as_ref() {
        return Ok(Object::Int(-value).into());
    }
    Err(anyhow::anyhow!("unknown operator: -{}", right.type_str()))
}

fn eval_infix(expr: &Infix, env: &Env) -> ObjResult {
    let left = eval_expression(expr.left(), env)?;
    let right = eval_expression(expr.right(), env)?;
    let op = expr.operator();

    let ret = match (left.as_ref(), right.as_ref()) {
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

fn eval_integer_infix(left: i64, right: i64, operator: &Token) -> ObjResult {
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
    Ok(ret.into())
}

fn eval_string_infix(left: &str, right: &str, operator: &Token) -> Rc<Object> {
    match operator {
        Token::Plus => Object::String(format!("{}{}", left, right)),
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        token => Object::Error(format!("unknown operator: STRING {} STRING", token)),
    }
    .into()
}

fn eval_bool_infix(left: bool, right: bool, operator: &Token) -> ObjResult {
    let ret = match operator {
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        token => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", token)),
    };
    Ok(ret.into())
}

fn eval_if(expr: &If, env: &Env) -> ObjResult {
    let condition = eval_expression(expr.condition(), env)?;

    if condition.bool_value()? {
        return eval_block(expr.consequence(), env);
    }

    if let Some(alternative) = expr.alternative() {
        return eval_block(alternative, env);
    }

    Ok(NIL.into())
}
