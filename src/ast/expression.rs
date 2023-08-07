use std::rc::Rc;

use crate::{lexer::Token, parser::Parser};

use super::Node;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Primative(Primative),
    StringLiteral(Rc<str>),
}

impl Node for Expression {
    fn parse(parser: &mut Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        let token = parser.current_token_unwrap()?;
        match token {
            Token::Int(_) | Token::True | Token::False => {
                Ok(Expression::Primative(Primative::parse(parser)?))
            }
            Token::Str(s) => Ok(Expression::StringLiteral(s.clone())),
            _ => todo!("Expression::parse for {:?}", token),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    token: Token,
    value: Rc<str>,
}

impl<'a> TryFrom<&'a Token> for Identifier {
    type Error = anyhow::Error;

    fn try_from(token: &'a Token) -> anyhow::Result<Self> {
        if let Token::Identifier(ident) = token {
            Ok(Self {
                token: token.clone(),
                value: ident.clone(),
            })
        } else {
            Err(anyhow::anyhow!(
                "Identifier can only be created from an Identifier token"
            ))
        }
    }
}

impl Node for Identifier {
    fn parse(parser: &mut Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        Self::try_from(parser.current_token_unwrap()?)
    }
}

#[derive(Debug, PartialEq)]
pub enum Primative {
    Int(i64),
    Bool(bool),
}

impl Node for Primative {
    fn parse(parser: &mut Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        let token = parser.current_token_unwrap()?;
        match token {
            Token::Int(val) => Ok(Self::Int(val.parse::<i64>()?)),
            Token::True => Ok(Self::Bool(true)),
            Token::False => Ok(Self::Bool(false)),
            _ => unreachable!("Primative parse unexpected {:?}", token),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral {}
