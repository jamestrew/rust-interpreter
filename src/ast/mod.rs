mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

use crate::parser::Parser;

pub trait Node {
    // fn get_token(&self) -> Option<&Token<'a>>;

    fn parse(parser: &mut Parser) -> anyhow::Result<Self> where Self: std::marker::Sized;
}

#[derive(Debug, Default, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// impl<'a> Node<'a> for Program<'a> {
//     fn get_token(&self) -> Option<&Token<'a>> {
//         if self.statements.is_empty() {
//             self.statements[0].get_token()
//         } else {
//             None
//         }
//     }
// }
