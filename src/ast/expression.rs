use crate::lexer::Token;

pub enum Expression<'a> {
    Identifier(Identifier<'a>),
}

pub struct Identifier<'a> {
    token: Token<'a>,
    value: &'a str,
}

