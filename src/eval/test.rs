use super::environment::new_env;
use super::*;
use crate::ast::Program;

fn parse(input: &str) -> Program {
    use crate::parser::*;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    parser.parse_programe().expect("valid program")
}

macro_rules! assert_program {
    ($name:tt, $input:expr, $expect:expr) => {
        #[test]
        fn $name() {
            let program = parse($input);
            let env = new_env(None);
            let output = eval_program(&program, &env);
            assert_eq!(output.to_string(), $expect);
        }
    };
}

assert_program!(integer_1, "5", "5");
assert_program!(integer_2, "5;", "5");

assert_program!(boolean_1, "true", "true");
assert_program!(boolean_2, "true;", "true");

assert_program!(string_literal, "\"hello\"", "hello");

assert_program!(prefix_bang_integer_1, "!1", "false");
assert_program!(prefix_bang_integer_2, "!69420", "false");
assert_program!(prefix_bang_integer_3, "!0", "true");
assert_program!(prefix_bang_integer_4, "!!0", "false");
assert_program!(prefix_bang_integer_5, "!!1", "true");

assert_program!(prefix_bang_bool_1, "!true", "false");
assert_program!(prefix_bang_bool_2, "!!true", "true");
assert_program!(prefix_bang_bool_3, "!false", "true");
assert_program!(prefix_bang_bool_4, "!!false", "false");

assert_program!(prefix_bang_string_literal_1, "!\"hello\"", "false");
assert_program!(prefix_bang_string_literal_2, "!\"\"", "true");

assert_program!(prefix_minus_integer_1, "-1", "-1");
assert_program!(prefix_minus_integer_2, "-(-1)", "1");

assert_program!(infix_integer_1, "5 + 5 + 5 + 5 - 10", "10");
assert_program!(infix_integer_2, "2 * 2 * 2 * 2 * 2", "32");
assert_program!(infix_integer_3, "-50 + 100 + -50", "0");
assert_program!(infix_integer_4, "5 * 2 + 10", "20");
assert_program!(infix_integer_5, "5 + 2 * 10", "25");
assert_program!(infix_integer_6, "20 + 2 * -10", "0");
assert_program!(infix_integer_7, "50 / 2 * 2 + 10", "60");
assert_program!(infix_integer_8, "2 * (5 + 10)", "30");
assert_program!(infix_integer_9, "3 * 3 * 3 + 10", "37");
assert_program!(infix_integer_10, "3 * (3 * 3) + 10", "37");
assert_program!(infix_integer_11, "(5 + 10 * 2 + 15 / 3) * 2 + -10", "50");
assert_program!(infix_integer_12, "1 < 2", "true");
assert_program!(infix_integer_13, "1 > 2", "false");
assert_program!(infix_integer_14, "1 < 1", "false");
assert_program!(infix_integer_15, "1 > 1", "false");
assert_program!(infix_integer_16, "1 == 1", "true");
assert_program!(infix_integer_17, "1 != 1", "false");
assert_program!(infix_integer_18, "1 == 2", "false");
assert_program!(infix_integer_19, "1 != 2", "true");

assert_program!(infix_bool_1, "true == true", "true");
assert_program!(infix_bool_2, "false == false", "true");
assert_program!(infix_bool_3, "true == false", "false");
assert_program!(infix_bool_4, "true != false", "true");
assert_program!(infix_bool_5, "false != true", "true");
assert_program!(infix_bool_6, "(1 < 2) == true", "true");
assert_program!(infix_bool_7, "(1 < 2) == false", "false");
assert_program!(infix_bool_8, "(1 > 2) == true", "false");
assert_program!(infix_bool_9, "(1 > 2) == false", "true");

assert_program!(infix_string, "\"foo\" + \"bar\"", "foobar");

assert_program!(if_expression_1, "if (true) { 10 }", "10");
assert_program!(if_expression_2, "if (false) { 10 }", "nil");
assert_program!(if_expression_3, "if (1) { 10 }", "10");
assert_program!(if_expression_4, "if (1 < 2) { 10 }", "10");
assert_program!(if_expression_5, "if (1 > 2) { 10 }", "nil");
assert_program!(if_expression_6, "if (1 > 2) { 10 } else { 20 }", "20");
assert_program!(if_expression_7, "if (1 < 2) { 10 } else { 20 }", "10");

assert_program!(return_statement_1, "return 10;", "10");
assert_program!(return_statement_2, "return 10; 9;", "10");
assert_program!(return_statement_3, "return 2 * 5; 9;", "10");
assert_program!(return_statement_4, "9; return 2 * 5; 9;", "10");
assert_program!(return_statement_5, "if (true) { return 10; }", "10");
assert_program!(
    return_statement_6,
    r#"
    if (true) {
        if (false) {
            return 10;
        }
        return 1;
    }
    "#,
    "1"
);
assert_program!(
    return_statement_7,
    r#"
    if (true) {
        if (true) {
            return 10;
        }
        return 1;
    }
    "#,
    "10"
);

assert_program!(block_statement_1, "{ 1; }", "1");

assert_program!(
    error_handling_1,
    "5 + true;",
    "type mismatch: INTEGER + BOOLEAN"
);
assert_program!(
    error_handling_2,
    "5 + true; 5",
    "type mismatch: INTEGER + BOOLEAN"
);
assert_program!(
    error_handling_3,
    "\"foo\" - \"bar\"",
    "unknown operator: STRING - STRING"
);
assert_program!(error_handling_4, "-true", "unknown operator: -BOOLEAN");

assert_program!(let_stmt_1, "let a = 5; a;", "5");
assert_program!(let_stmt_2, "let a = 5 * 5; a;", "25");
assert_program!(let_stmt_3, "let a = 5; let b = a; b;", "5");
assert_program!(
    let_stmt_4,
    "let a = 5; let b = a; let c = a + b + 5; c;",
    "15"
);
assert_program!(let_stmt_5, "foobar;", "identifier not found: foobar");

assert_program!(fn_call_1, "let identity = fn(x) { x; }; identity(5)", "5");
assert_program!(
    fn_call_2,
    "let identity = fn(x) { return x; }; identity(5)",
    "5"
);
assert_program!(fn_call_3, "let double = fn(x) { x * 2; }; double(5)", "10");
assert_program!(fn_call_4, "let add = fn(x, y) { x + y; }; add(5, 6)", "11");
assert_program!(
    fn_call_5,
    "let add = fn(x, y) { x + y; }; add(5 + 6, add(7, 8))",
    "26"
);
assert_program!(fn_call_6, "fn(x) { x; }(5)", "5");

assert_program!(
    closure,
    "
let newAdder = fn(x) {
    fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(3);
",
    "5"
);

assert_program!(builtin_1, "len(\"foo\")", "3");
assert_program!(builtin_2, "puts(\"foo\")", "");
assert_program!(
    builtin_3,
    "let add = fn(x, y) { len(x + y) }; add(\"foo\", \"bar\")",
    "6"
);
