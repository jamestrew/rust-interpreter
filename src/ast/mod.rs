mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

pub trait Node: std::fmt::Display + std::fmt::Debug {}

#[derive(Debug, Default, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
