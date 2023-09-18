use std::fmt::Display;

use crate::ast::*;
use crate::lexer::Token;

pub enum Object {
    Int { value: i64 },
    Bool { value: bool },
    StringLiteral { value: String },
    Null,
}

const TRUE: Object = Object::Bool { value: true };
const FALSE: Object = Object::Bool { value: false };

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int { value } => write!(f, "{}", value),
            Object::Bool { value } => write!(f, "{}", value),
            Object::StringLiteral { value } => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}

pub fn eval_program(program: &Program) -> Object {
    if let Some(stmt) = program.statements.first() {
        let obj = match stmt {
            Statement::ExpressionStatement(val) => eval_expression(val),
            _ => todo!("Program::Eval -> should only return `Return`"),
        };
        return obj;
    }
    unreachable!("Program must have at least one statement")
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        Expression::Identifier(_val) => todo!(),
        Expression::Primative(val) => eval_primative(val),
        Expression::StringLiteral(val) => Object::StringLiteral {
            value: val.to_string(),
        },
        Expression::Prefix(val) => eval_prefix(val),
        Expression::Infix(val) => eval_infix(val),
        Expression::If(_val) => todo!(),
    }
}

fn eval_primative(expr: &Primative) -> Object {
    match expr {
        Primative::Int(val) => Object::Int { value: *val },
        Primative::Bool(val) => Object::Bool { value: *val },
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
    let truthy = match right {
        Object::Int { value } => value != 0,
        Object::Bool { value } => value,
        Object::StringLiteral { value } => !value.is_empty(),
        Object::Null => false,
    };

    if truthy {
        FALSE
    } else {
        TRUE
    }
}

fn eval_minus_prefix(right: &Expression) -> Object {
    let right = eval_expression(right);
    if let Object::Int { value } = right {
        return Object::Int { value: -value };
    }
    unreachable!("ERROR: found non-integer value in unary operator `-`");
}

fn eval_infix(expr: &Infix) -> Object {
    let left = eval_expression(expr.left());
    let right = eval_expression(expr.right());

    match expr.operator() {
        Token::Minus => eval_infix_minus(&left, &right),
        Token::Plus => eval_infix_plus(&left, &right),
        Token::Asterisk => eval_infix_multiply(&left, &right),
        Token::ForwardSlash => eval_infix_divide(&left, &right),
        Token::Equal => todo!(),
        Token::NotEqual => todo!(),
        Token::LT => todo!(),
        Token::GT => todo!(),
        _ => expr.unreachable_operator(),
    }
}

fn eval_infix_minus(left: &Object, right: &Object) -> Object {
    let (left, right) = integer_infix_values(left, right).unwrap();
    Object::Int {
        value: left - right,
    }
}

fn eval_infix_plus(left: &Object, right: &Object) -> Object {
    if let (Object::Int { value: left }, Object::Int { value: right }) = (left, right) {
        return Object::Int {
            value: *left + *right,
        };
    } else if let (Object::StringLiteral { value: left }, Object::StringLiteral { value: right }) =
        (left, right)
    {
        return Object::StringLiteral {
            value: format!("{}{}", left, right),
        };
    }
    panic!("Operands of `+` must both either be Object::StringLiteral or Object::Int")
}

fn eval_infix_multiply(left: &Object, right: &Object) -> Object {
    let (left, right) = integer_infix_values(left, right).unwrap();
    Object::Int {
        value: left * right,
    }
}

fn eval_infix_divide(left: &Object, right: &Object) -> Object {
    let (left, right) = integer_infix_values(left, right).unwrap();
    // HACK: this could panic implicitly
    Object::Int {
        value: left / right,
    }
}

fn integer_infix_values(left: &Object, right: &Object) -> anyhow::Result<(i64, i64)> {
    if let (Object::Int { value: left }, Object::Int { value: right }) = (left, right) {
        return Ok((*left, *right));
    }
    Err(anyhow::anyhow!(
        "Both operands of must be of type Object::Int"
    ))
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
                assert_eq!(eval_program(&program).to_string(), $expect);
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
    // assert_stmt!(infix_integer_2, "2 * 2 * 2 * 2 * 2", "32");
    // assert_stmt!(infix_integer_3, "-50 + 100 + -50", "0");
    // assert_stmt!(infix_integer_4, "5 * 2 + 10", "20");
    // assert_stmt!(infix_integer_5, "5 + 2 * 10", "25");
    // assert_stmt!(infix_integer_6, "20 + 2 * -10", "0");
    // assert_stmt!(infix_integer_7, "50 / 2 * 2 + 10", "60");
    // assert_stmt!(infix_integer_8, "2 * (5 + 10)", "30");
    // assert_stmt!(infix_integer_9, "3 * 3 * 3 + 10", "37");
    // assert_stmt!(infix_integer_10, "3 * (3 * 3) + 10", "37");
    // assert_stmt!(infix_integer_11, "(5 + 10 * 2 + 15 / 3) * 2 + -10", "50");
}
