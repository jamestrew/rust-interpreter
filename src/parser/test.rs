use super::*;
use crate::ast::*;
use crate::lexer::Lexer;

fn parse(input: &str) -> Vec<Statement> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_programe().expect("valid program");
    assert!(!program.statements.is_empty());
    program.statements
}

macro_rules! assert_stmt {
    ($name:tt, $input:expr) => {
        #[test]
        fn $name() {
            let stmts = parse($input);
            let stmt = stmts[0].to_string();
            assert_eq!($input, stmt);
        }
    };
}

macro_rules! ss_display_stmt {
    ($name:tt, $input:expr) => {
        #[test]
        fn $name() {
            let statements = parse($input);
            let first = &statements[0];
            insta::with_settings!({
                description => $input,
            }, {
                insta::assert_display_snapshot!(first);
            })
        }
    };
}

#[allow(unused_macros)]
macro_rules! ss_debug_stmts {
    ($name:tt, $input:expr) => {
        #[test]
        fn $name() {
            let stmts = parse($input);
            insta::with_settings!({
                description => $input,
            }, {
                insta::assert_debug_snapshot!(stmts);
            })
        }
    };
}

#[test]
fn swallow_extra_semicolons() {
    let stmts = parse("return true;;;;");
    let stmt = stmts[0].to_string();
    assert_eq!(stmt, "return true;");

    let statements = parse("foobar;;;;");
    let first = statements[0].to_string();
    assert_eq!(first, "foobar;");
}

assert_stmt!(literal_let_statement_1, "let x = 5;");
assert_stmt!(literal_let_statement_2, "let y = 10;");
assert_stmt!(literal_let_statement_3, "let foobar = 838383;");
assert_stmt!(literal_let_statement_4, "let foo = \"bar\";");
assert_stmt!(literal_let_statement_5, "let foo = true;");
assert_stmt!(identifier_let_statement, "let foo = foobar;");

assert_stmt!(literal_return_statement_1, "return 5;");
assert_stmt!(literal_return_statement_2, "return true;");
assert_stmt!(literal_return_statement_3, "return \"foo\";");
assert_stmt!(identifier_return_statement, "return foobar;");

assert_stmt!(identifier, "foobar;");
assert_stmt!(integer_literal, "1;");
assert_stmt!(boolean_literal, "true;");
assert_stmt!(string_literal, "\"hello world\";");

ss_display_stmt!(prefix_expression_1, "-5;");
ss_display_stmt!(prefix_expression_2, "!foobar;");

ss_display_stmt!(infix_expr_1, "5 + 6;");
ss_display_stmt!(infix_expr_2, "5 - 6;");
ss_display_stmt!(infix_expr_3, "5 * 6;");
ss_display_stmt!(infix_expr_4, "5 / 6;");
ss_display_stmt!(infix_expr_5, "true == true;");
ss_display_stmt!(infix_expr_6, "true != false;");
ss_display_stmt!(infix_expr_7, "5 < 6");
ss_display_stmt!(infix_expr_8, "7 > 6");
ss_display_stmt!(infix_expr_9, "\"foo\" != \"bar\"");

ss_display_stmt!(operator_precedence_1, "-a * b");
ss_display_stmt!(operator_precedence_2, "!-a");
ss_display_stmt!(operator_precedence_3, "a + b + c");
ss_display_stmt!(operator_precedence_4, "a + b - c");
ss_display_stmt!(operator_precedence_5, "a * b * c");
ss_display_stmt!(operator_precedence_6, "a * b / c");
ss_display_stmt!(operator_precedence_7, "a + b / c");
ss_display_stmt!(operator_precedence_8, "a + b * c + d / e - f");
ss_display_stmt!(operator_precedence_9, "5 > 4 == 3 < 4");
ss_display_stmt!(operator_precedence_10, "5 < 4 != 3 > 4");
ss_display_stmt!(operator_precedence_11, "3 + 4 * 5 == 3 * 1 + 4 * 5");
ss_display_stmt!(operator_precedence_12, "true");
ss_display_stmt!(operator_precedence_13, "false");
ss_display_stmt!(operator_precedence_14, "3 > 5 == false");
ss_display_stmt!(operator_precedence_15, "3 < 5 == true");
ss_display_stmt!(operator_precedence_16, "1 + (2 + 3) + 4");
ss_display_stmt!(operator_precedence_17, "(5 + 5) * 2");
ss_display_stmt!(operator_precedence_18, "2 / (5 + 5)");
ss_display_stmt!(operator_precedence_19, "-(5 + 5)");
ss_display_stmt!(operator_precedence_20, "!(true == true)");

ss_display_stmt!(if_expr_1, "if (x < y) { x }");
ss_display_stmt!(if_expr_2, "if (x < y) { x } else { y }");
ss_display_stmt!(if_expr_3, "if (x < y) { x } else { let z = x + y; z }");

// assert_stmts!(block_1, "{ return 5; }");
// assert_stmts!(block_2, "{ return 5; return true; }");
