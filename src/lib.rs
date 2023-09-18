mod ast;
pub mod lexer;
pub mod parser;
pub mod eval;

use lexer::Lexer;
use parser::*;
use ast::Statement;

pub fn parse(input: &str) -> Vec<Statement> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_programe().expect("valid program");
    assert!(!program.statements.is_empty());
    program.statements
}
