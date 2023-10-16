mod ast;
pub mod eval;
pub mod lexer;
pub mod parser;
mod errors;
mod types;

use ast::Statement;
pub use eval::*;
use lexer::Lexer;
use parser::*;

pub fn parse(input: &str) -> Vec<Statement> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_programe().expect("valid program");
    assert!(!program.statements.is_empty());
    program.statements
}
