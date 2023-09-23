mod environment;
mod object;
#[cfg(test)]
mod test;

use object::*;

use self::environment::Environment;
use crate::ast::*;
use crate::lexer::Token;

pub fn eval_program(program: &Program, mut env: Environment) -> Object {
    let mut ret = NIL;
    for stmt in &program.statements {
        let stmt = eval_statement(stmt, &mut env);
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

fn eval_statement(stmt: &Statement, env: &mut Environment) -> anyhow::Result<Object> {
    match stmt {
        Statement::Let(val) => eval_let(val, env),
        Statement::Return(val) => eval_return(val, env),
        Statement::Block(val) => eval_block(val, env),
        Statement::ExpressionStatement(val) => eval_expression(val, env),
    }
}

fn eval_let(stmt: &Let, env: &mut Environment) -> anyhow::Result<Object> {
    let value: Object = eval_expression(stmt.value(), env)?;
    env.set(stmt.name().clone(), value.clone());
    Ok(value)
}

fn eval_block(stmt: &Block, env: &mut Environment) -> anyhow::Result<Object> {
    let mut ret = NIL;
    for stmt in stmt.statements() {
        ret = eval_statement(stmt, env)?;
        if let Object::Return(_) = ret {
            return Ok(ret);
        }
    }
    Ok(ret)
}

fn eval_return(stmt: &Return, env: &mut Environment) -> anyhow::Result<Object> {
    let expr = eval_expression(stmt.value(), env)?;
    Ok(Object::Return(Box::new(expr)))
}

fn eval_expression(expr: &Expression, env: &mut Environment) -> anyhow::Result<Object> {
    match expr {
        Expression::Identifier(val) => eval_identifier(val, env),
        Expression::Primative(val) => Ok(eval_primative(val)),
        Expression::StringLiteral(val) => Ok(Object::String(val.to_string())),
        Expression::Prefix(val) => eval_prefix(val, env),
        Expression::Infix(val) => eval_infix(val, env),
        Expression::If(val) => eval_if(val, env),
        Expression::Function(_val) => todo!(),
        Expression::Call(_val) => todo!(),
    }
}

fn eval_identifier(expr: &Identifier, env: &Environment) -> anyhow::Result<Object> {
    match env.get(expr) {
        Some(obj) => Ok(obj.clone()),
        None => Err(anyhow::anyhow!("identifier not found: {}", expr)),
    }
}

fn eval_primative(expr: &Primative) -> Object {
    match expr {
        Primative::Int(val) => Object::Int(*val),
        Primative::Bool(val) => Object::new_bool(*val),
    }
}

fn eval_prefix(expr: &Prefix, env: &mut Environment) -> anyhow::Result<Object> {
    match expr.operator() {
        Token::Bang => eval_bang_prefix(expr.right(), env),
        Token::Minus => eval_minus_prefix(expr.right(), env),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_bang_prefix(right: &Expression, env: &mut Environment) -> anyhow::Result<Object> {
    let right = eval_expression(right, env)?;
    Ok(Object::new_bool(!right.bool_value()?))
}

fn eval_minus_prefix(right: &Expression, env: &mut Environment) -> anyhow::Result<Object> {
    let right = eval_expression(right, env)?;
    if let Object::Int(value) = right {
        return Ok(Object::Int(-value));
    }
    Err(anyhow::anyhow!("unknown operator: -{}", right.type_str()))
}

fn eval_infix(expr: &Infix, env: &mut Environment) -> anyhow::Result<Object> {
    let left = eval_expression(expr.left(), env)?;
    let right = eval_expression(expr.right(), env)?;
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

fn eval_if(expr: &If, env: &mut Environment) -> anyhow::Result<Object> {
    let condition = eval_expression(expr.condition(), env)?;

    if condition.bool_value()? {
        return eval_block(expr.consequence(), env);
    }

    if let Some(alternative) = expr.alternative() {
        return eval_block(alternative, env);
    }

    Ok(NIL)
}
