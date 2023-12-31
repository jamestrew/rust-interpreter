mod builtin;
mod environment;
mod object;
#[cfg(test)]
mod test;

use std::rc::Rc;

use object::*;

use self::environment::Env;
use crate::ast::{self, *};
pub use crate::eval::environment::new_env;
use crate::eval::environment::Environment;
use crate::lexer::{Lexer, TokenKind};
use crate::parser::*;

type ObjResult = anyhow::Result<Rc<Object>>;

pub fn eval(input: &str, env: &Env) -> Option<String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_programe() {
        Ok(prog) => prog,
        Err(err) => return Some(format!("Syntax error: {}", err)),
    };

    match eval_program(&program, env).as_ref() {
        Object::Empty => None,
        obj => Some(obj.to_string()),
    }
}

fn eval_program(program: &Program, env: &Env) -> Rc<Object> {
    let mut ret = Rc::new(EMPTY);
    for stmt in &program.statements {
        let stmt = eval_statement(stmt, env);
        match stmt {
            Ok(stmt) => {
                let obj = stmt.as_ref();
                match obj {
                    Object::Return(val) => return val.clone(),
                    Object::Error(_) => return stmt,
                    _ => ret = stmt,
                }
            }
            Err(err) => return Object::new_eror(err).into(),
        }
    }
    ret
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
    env.borrow_mut().set(stmt.name().value(), Rc::clone(&value));
    Ok(EMPTY.into())
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
    Ok(Object::Return(Rc::clone(&expr)).into())
}

fn eval_expression(expr: &Expression, env: &Env) -> ObjResult {
    match expr {
        Expression::Identifier(val) => eval_identifier(val, env),
        Expression::Primative(val) => Ok(eval_primative(val)),
        Expression::StringLiteral(val) => Ok(Object::String(val.to_string()).into()),
        Expression::Prefix(val) => eval_prefix(val, env),
        Expression::Infix(val) => eval_infix(val, env),
        Expression::If(val) => eval_if(val, env),
        Expression::Function(val) => eval_function(val, env),
        Expression::Array(val) => eval_array(val, env),
        Expression::Hash(val) => eval_hash(val, env),
        Expression::Call(val) => eval_fn_call(val, env),
        Expression::Index(val) => eval_index(val, env),
    }
}

fn eval_identifier(expr: &Identifier, env: &Env) -> ObjResult {
    match env.borrow().get(expr.value()) {
        Some(obj) => Ok(Rc::clone(&obj)),
        None => Err(anyhow::anyhow!("identifier not found: {}", expr)),
    }
}

fn eval_primative(expr: &Primative) -> Rc<Object> {
    match expr {
        Primative::Int { value, .. } => Object::Int(*value),
        Primative::Bool { value, .. } => Object::new_bool(*value),
        Primative::Nil { .. } => NIL,
    }
    .into()
}

fn eval_prefix(expr: &Prefix, env: &Env) -> ObjResult {
    match expr.operator() {
        TokenKind::Bang => eval_bang_prefix(expr.right(), env),
        TokenKind::Minus => eval_minus_prefix(expr.right(), env),
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

fn eval_integer_infix(left: i64, right: i64, operator: &TokenKind) -> ObjResult {
    let ret = match operator {
        TokenKind::Minus => Object::Int(left - right),
        TokenKind::Plus => Object::Int(left + right),
        TokenKind::Asterisk => Object::Int(left * right),
        TokenKind::ForwardSlash => {
            if right == 0 {
                return Err(anyhow::anyhow!("Error: divide by zero"));
            }
            Object::Int(left / right)
        }
        TokenKind::Equal => Object::new_bool(left == right),
        TokenKind::NotEqual => Object::new_bool(left != right),
        TokenKind::LT => Object::new_bool(left < right),
        TokenKind::GT => Object::new_bool(left > right),
        token => Object::Error(format!("unknown operator: INTEGER {} INTEGER", token)),
    };
    Ok(ret.into())
}

fn eval_string_infix(left: &str, right: &str, operator: &TokenKind) -> Rc<Object> {
    match operator {
        TokenKind::Plus => Object::String(format!("{}{}", left, right)),
        TokenKind::Equal => Object::new_bool(left == right),
        TokenKind::NotEqual => Object::new_bool(left != right),
        token => Object::Error(format!("unknown operator: STRING {} STRING", token)),
    }
    .into()
}

fn eval_bool_infix(left: bool, right: bool, operator: &TokenKind) -> ObjResult {
    let ret = match operator {
        TokenKind::Equal => Object::new_bool(left == right),
        TokenKind::NotEqual => Object::new_bool(left != right),
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

fn eval_function(expr: &ast::Function, env: &Env) -> ObjResult {
    Ok(Object::Function(object::Function::new(expr, env)).into())
}

fn eval_array(array: &[Expression], env: &Env) -> ObjResult {
    let mut elems = Vec::new();
    for elem in array {
        elems.push(eval_expression(elem, env)?);
    }
    Ok(Object::Array(elems).into())
}

fn eval_hash(hash: &ast::Hash, env: &Env) -> ObjResult {
    let mut keys = Vec::new();
    let mut values = Vec::new();

    for (key, value) in hash.items() {
        keys.push(eval_expression(key, env)?);
        values.push(eval_expression(value, env)?);
    }

    Ok(Object::Hash(object::Hash::try_new(keys, values)?).into())
}

fn eval_fn_call(expr: &Call, env: &Env) -> ObjResult {
    let func = eval_expression(expr.function(), env)?;

    let mut arg_objs = Vec::new();
    for arg in expr.args() {
        arg_objs.push(eval_expression(arg, env)?)
    }

    if let Object::Function(func) = func.as_ref() {
        let mut env = Environment::new_enclosed(&Rc::clone(func.env()));

        let params = func.params();
        if params.len() != arg_objs.len() {
            return Err(anyhow::anyhow!("mismatched arg and param count"));
        }

        for (param, arg) in params.iter().zip(arg_objs) {
            env.set(param.value(), Rc::clone(&arg))
        }
        return eval_block(func.body(), &new_env(Some(env)));
    }

    println!("eval_fn_call");
    match func.as_ref() {
        Object::Function(func) => {
            let mut env = Environment::new_enclosed(&Rc::clone(func.env()));

            let params = func.params();
            if params.len() != arg_objs.len() {
                return Err(anyhow::anyhow!("mismatched arg and param count"));
            }

            for (param, arg) in params.iter().zip(arg_objs) {
                env.set(param.value(), Rc::clone(&arg))
            }
            eval_block(func.body(), &new_env(Some(env)))
        }
        Object::Builtin(builtin) => builtin.execute(&arg_objs),
        _ => Err(anyhow::anyhow!(
            "'{}' is not a callable object",
            func.type_str()
        )),
    }
}

fn eval_index(expr: &Index, env: &Env) -> ObjResult {
    let left_obj = eval_expression(expr.left(), env)?;
    let index = eval_expression(expr.index(), env)?;

    match left_obj.as_ref() {
        Object::Array(arr) => {
            let index = integer_index(&index, "array", arr.len())?;
            Ok(Rc::clone(&arr[index]))
        }
        Object::String(s) => {
            let index = integer_index(&index, "string", s.len())?;
            Ok(Object::String(s[index..index + 1].to_string()).into())
        }
        Object::Hash(hash) => {
            let index = HashKey::try_from(index)?;
            Ok(hash.get(&index).unwrap_or(Rc::new(NIL)))
        }
        obj => Err(anyhow::anyhow!(
            "'{}' object is not subscriptable",
            obj.type_str()
        )),
    }
}

fn integer_index(index: &Rc<Object>, index_item: &str, index_len: usize) -> anyhow::Result<usize> {
    if let Object::Int(index) = index.as_ref() {
        let index = *index as usize;
        if index_len <= index {
            Err(anyhow::anyhow!("{} index out of range", index_item))
        } else {
            Ok(index)
        }
    } else {
        Err(anyhow::anyhow!(
            "{} indices must be integers, not '{}'",
            index_item,
            index.type_str()
        ))
    }
}
