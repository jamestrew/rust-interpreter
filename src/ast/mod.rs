mod expression;
mod statement;

pub use statement::{Let, Statement};

// pub trait Node<'a> {
//     fn get_token(&self) -> Option<&Token<'a>>;
//
//     fn parse() -> Self;
// }

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
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
