use super::expression::{Expression, Identifier};
use crate::lexer::Token;
use crate::parser::Parser;

pub enum Statement<'a> {
    Let(Let<'a>),
    // ...
}

pub struct Let<'a> {
    token: Token<'a>,
    name: &'a Identifier<'a>,
    value: &'a Expression<'a>,
}

impl<'a> TryFrom<&mut Parser<'a>> for Let<'a> {
    type Error = anyhow::Error;

    fn try_from(parser: &mut Parser) -> anyhow::Result<Self> {
        todo!()
    }
}
