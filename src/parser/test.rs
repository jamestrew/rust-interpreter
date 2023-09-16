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

macro_rules! ss_debug_stmt {
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

ss_debug_stmt!(prefix_expression_1, "-5;");
ss_debug_stmt!(prefix_expression_2, "!foobar;");

// assert_stmts!(block_1, "{ return 5; }");
// assert_stmts!(block_2, "{ return 5; return true; }");
