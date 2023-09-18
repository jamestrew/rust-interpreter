mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

pub trait Node: std::fmt::Display + std::fmt::Debug {}

#[derive(Debug, Default, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
