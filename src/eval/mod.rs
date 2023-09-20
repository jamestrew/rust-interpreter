use std::fmt::Display;

use crate::ast::*;
use crate::lexer::Token;

pub enum Object {
    Int(i64),
    Bool(bool),
    StringLiteral(String),
    Return,
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
            Object::StringLiteral(val) => Ok(!val.is_empty()),
            Object::Nil => Ok(false),
            _ => Err(anyhow::anyhow!("not a bool value")),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::StringLiteral(value) => write!(f, "{}", value),
            Object::Return => write!(f, "nil"),
            Object::Nil => write!(f, "nil"),
        }
    }
}

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NIL: Object = Object::Nil;

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::Let(_) => todo!(),
        Statement::Return(_) => todo!(),
        Statement::Block(_) => todo!(),
        Statement::ExpressionStatement(val) => eval_expression(val),
    }
}

pub fn eval_statements(stmts: &[Statement]) -> Object {
    let mut ret = NIL;
    for stmt in stmts {
        ret = eval_statement(stmt);
        if let Object::Return = ret {
            return ret;
        }
    }

    ret
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        Expression::Identifier(_val) => todo!(),
        Expression::Primative(val) => eval_primative(val),
        Expression::StringLiteral(val) => Object::StringLiteral(val.to_string()),
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

fn eval_prefix(expr: &Prefix) -> Object {
    match expr.operator() {
        Token::Bang => eval_bang_prefix(expr.right()),
        Token::Minus => eval_minus_prefix(expr.right()),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_bang_prefix(right: &Expression) -> Object {
    let right = eval_expression(right);
    Object::new_bool(!right.bool_value().unwrap())
}

fn eval_minus_prefix(right: &Expression) -> Object {
    let right = eval_expression(right);
    if let Object::Int(value) = right {
        return Object::Int(-value);
    }
    unreachable!("ERROR: found non-integer value in unary operator `-`");
}

fn eval_infix(expr: &Infix) -> Object {
    let left = eval_expression(expr.left());
    let right = eval_expression(expr.right());
    let op = expr.operator();

    match (&left, &right) {
        (Object::Int(left), Object::Int(right)) => eval_integer_infix(*left, *right, op),
        (Object::StringLiteral(left), Object::StringLiteral(right)) => {
            eval_string_infix(left, right, op)
        }
        (Object::Bool(left), Object::Bool(right)) => eval_bool_infix(*left, *right, op),
        (Object::Int(left), Object::Bool(right)) => eval_bool_infix(*left != 0, *right, op),
        (Object::Bool(left), Object::Int(right)) => eval_bool_infix(*left, *right != 0, op),
        _ => todo!("eval_infix: handle rest as errors?"),
    }
}

fn eval_integer_infix(left: i64, right: i64, operator: &Token) -> Object {
    match operator {
        Token::Minus => Object::Int(left - right),
        Token::Plus => Object::Int(left + right),
        Token::Asterisk => Object::Int(left * right),
        Token::ForwardSlash => Object::Int(left / right), // HACK: this could panic implicity via
        // divide by zero
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        Token::LT => Object::new_bool(left < right),
        Token::GT => Object::new_bool(left > right),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_string_infix(left: &str, right: &str, operator: &Token) -> Object {
    match operator {
        Token::Plus => Object::StringLiteral(format!("{}{}", left, right)),
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_bool_infix(left: bool, right: bool, operator: &Token) -> Object {
    match operator {
        Token::Equal => Object::new_bool(left == right),
        Token::NotEqual => Object::new_bool(left != right),
        _ => Prefix::unreachable_operator(),
    }
}

fn eval_if(expr: &If) -> Object {
    let condition = eval_expression(expr.condition());

    if condition.bool_value().unwrap() {
        return eval_statements(expr.consequence().statements());
    }

    if let Some(alternative) = expr.alternative() {
        return eval_statements(alternative.statements());
    }

    NIL
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::Program;

    fn parse(input: &str) -> Program {
        use crate::lexer::Lexer;
        use crate::parser::*;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_programe().expect("valid program")
    }

    macro_rules! assert_stmt {
        ($name:tt, $input:expr, $expect:expr) => {
            #[test]
            fn $name() {
                let program = parse($input);
                assert_eq!(eval_statements(&program.statements).to_string(), $expect);
            }
        };
    }

    assert_stmt!(integer_1, "5", "5");
    assert_stmt!(integer_2, "5;", "5");

    assert_stmt!(boolean_1, "true", "true");
    assert_stmt!(boolean_2, "true;", "true");

    assert_stmt!(string_literal, "\"hello\"", "hello");

    assert_stmt!(prefix_bang_integer_1, "!1", "false");
    assert_stmt!(prefix_bang_integer_2, "!69420", "false");
    assert_stmt!(prefix_bang_integer_3, "!0", "true");
    assert_stmt!(prefix_bang_integer_4, "!!0", "false");
    assert_stmt!(prefix_bang_integer_5, "!!1", "true");

    assert_stmt!(prefix_bang_bool_1, "!true", "false");
    assert_stmt!(prefix_bang_bool_2, "!!true", "true");
    assert_stmt!(prefix_bang_bool_3, "!false", "true");
    assert_stmt!(prefix_bang_bool_4, "!!false", "false");

    assert_stmt!(prefix_bang_string_literal_1, "!\"hello\"", "false");
    assert_stmt!(prefix_bang_string_literal_2, "!\"\"", "true");

    assert_stmt!(prefix_minus_integer_1, "-1", "-1");
    assert_stmt!(prefix_minus_integer_2, "-(-1)", "1");

    assert_stmt!(infix_integer_1, "5 + 5 + 5 + 5 - 10", "10");
    assert_stmt!(infix_integer_2, "2 * 2 * 2 * 2 * 2", "32");
    assert_stmt!(infix_integer_3, "-50 + 100 + -50", "0");
    assert_stmt!(infix_integer_4, "5 * 2 + 10", "20");
    assert_stmt!(infix_integer_5, "5 + 2 * 10", "25");
    assert_stmt!(infix_integer_6, "20 + 2 * -10", "0");
    assert_stmt!(infix_integer_7, "50 / 2 * 2 + 10", "60");
    assert_stmt!(infix_integer_8, "2 * (5 + 10)", "30");
    assert_stmt!(infix_integer_9, "3 * 3 * 3 + 10", "37");
    assert_stmt!(infix_integer_10, "3 * (3 * 3) + 10", "37");
    assert_stmt!(infix_integer_11, "(5 + 10 * 2 + 15 / 3) * 2 + -10", "50");
    assert_stmt!(infix_integer_12, "1 < 2", "true");
    assert_stmt!(infix_integer_13, "1 > 2", "false");
    assert_stmt!(infix_integer_14, "1 < 1", "false");
    assert_stmt!(infix_integer_15, "1 > 1", "false");
    assert_stmt!(infix_integer_16, "1 == 1", "true");
    assert_stmt!(infix_integer_17, "1 != 1", "false");
    assert_stmt!(infix_integer_18, "1 == 2", "false");
    assert_stmt!(infix_integer_19, "1 != 2", "true");

    assert_stmt!(infix_bool_1, "true == true", "true");
    assert_stmt!(infix_bool_2, "false == false", "true");
    assert_stmt!(infix_bool_3, "true == false", "false");
    assert_stmt!(infix_bool_4, "true != false", "true");
    assert_stmt!(infix_bool_5, "false != true", "true");
    assert_stmt!(infix_bool_6, "(1 < 2) == true", "true");
    assert_stmt!(infix_bool_7, "(1 < 2) == false", "false");
    assert_stmt!(infix_bool_8, "(1 > 2) == true", "false");
    assert_stmt!(infix_bool_9, "(1 > 2) == false", "true");

    assert_stmt!(infix_string, "\"foo\" + \"bar\"", "foobar");

    assert_stmt!(if_expression_1, "if (true) { 10 }", "10");
    assert_stmt!(if_expression_2, "if (false) { 10 }", "nil");
    assert_stmt!(if_expression_3, "if (1) { 10 }", "10");
    assert_stmt!(if_expression_4, "if (1 < 2) { 10 }", "10");
    assert_stmt!(if_expression_5, "if (1 > 2) { 10 }", "nil");
    assert_stmt!(if_expression_6, "if (1 > 2) { 10 } else { 20 }", "20");
    assert_stmt!(if_expression_7, "if (1 < 2) { 10 } else { 20 }", "10");
}
