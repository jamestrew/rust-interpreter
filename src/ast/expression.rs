use std::fmt::{Debug, Display};
use std::rc::Rc;

use super::*;
use crate::lexer::Token;

#[derive(PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Primative(Primative),
    StringLiteral(Rc<str>),
    Prefix(Prefix),
    Infix(Infix),
    If(If),
}

impl Node for Expression {}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(val) => write!(f, "{:?}", val),
            Expression::Primative(val) => write!(f, "{:?}", val),
            Expression::StringLiteral(val) => write!(f, "StringLiteral({:?})", val),
            Expression::Prefix(val) => write!(f, "{:?}", val),
            Expression::Infix(val) => write!(f, "{:?}", val),
            Expression::If(val) => write!(f, "{:?}", val),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expression::Identifier(val) => val.to_string(),
            Expression::Primative(val) => val.to_string(),
            Expression::StringLiteral(val) => format!("\"{}\"", val),
            Expression::Prefix(val) => val.to_string(),
            Expression::Infix(val) => val.to_string(),
            Expression::If(val) => val.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Clone)]
pub struct Identifier {
    token: Token,
    value: Rc<str>,
}

impl Node for Identifier {}

impl From<&Rc<str>> for Identifier {
    fn from(value: &Rc<str>) -> Self {
        Self {
            token: Token::Identifier(value.clone()),
            value: value.clone(),
        }
    }
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

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Identifier({:?})", self.value)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub enum Primative {
    Int(i64),
    Bool(bool),
}

impl Node for Primative {}

impl TryFrom<&Token> for Primative {
    type Error = anyhow::Error;

    fn try_from(token: &Token) -> anyhow::Result<Self> {
        match token {
            Token::Int(val) => Ok(Self::Int(val.parse::<i64>()?)),
            Token::True => Ok(Self::Bool(true)),
            Token::False => Ok(Self::Bool(false)),
            _ => unreachable!("Primative parse unexpected {:?}", token),
        }
    }
}

impl Debug for Primative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(val) => write!(f, "Int({val})"),
            Self::Bool(val) => write!(f, "Bool({val})"),
        }
    }
}

impl Display for Primative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primative::Int(val) => write!(f, "{}", val),
            Primative::Bool(val) => write!(f, "{}", val),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Prefix {
    token: Token,
    right: Box<Expression>,
}

impl Node for Prefix {}

impl Prefix {
    pub fn new(op: Token, right: Expression) -> Self {
        Self {
            token: op,
            right: Box::new(right),
        }
    }

    fn operator_str(&self) -> &'static str {
        match self.token {
            Token::Minus => "-",
            Token::Bang => "!",
            _ => Self::unreachable_operator(),
        }
    }

    pub fn operator(&self) -> &Token {
        &self.token
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }

    pub fn unreachable_operator() -> ! {
        unreachable!("only Bang and Minus are allowed prefixes")
    }
}

impl Debug for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Prefix")
            .field("token", &self.operator_str())
            .field("right", &self.right)
            .finish()
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator_str(), self.right)
    }
}

#[derive(PartialEq, Clone)]
pub struct Infix {
    token: Token,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl Node for Infix {}

impl Infix {
    pub fn new(op_token: Token, left: Expression, right: Expression) -> Self {
        Self {
            token: op_token,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    fn operator_str(&self) -> &'static str {
        match self.token {
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Asterisk => "*",
            Token::ForwardSlash => "/",
            Token::Equal => "==",
            Token::NotEqual => "!=",
            Token::LT => "<",
            Token::GT => ">",
            _ => self.unreachable_operator(),
        }
    }

    pub fn operator(&self) -> &Token {
        &self.token
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }

    pub fn unreachable_operator(&self) -> ! {
        unreachable!("unallowed infix operator {:?}", self.token)
    }
}

impl Debug for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Infix")
            .field("token", &self.operator_str())
            .field("left", &self.left)
            .field("right", &self.right)
            .finish()
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator_str(), self.right)
    }
}

#[derive(PartialEq, Clone)]
pub struct If {
    token: Token,
    condition: Box<Expression>,
    consequence: Block,
    alternative: Option<Block>,
}

impl Node for If {}

impl If {
    pub fn new(condition: Expression, consequence: Block, alternative: Option<Block>) -> Self {
        Self {
            token: Token::If,
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
}

impl Debug for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("If")
            .field("condition", &self.condition)
            .field("consequence", &self.consequence)
            .field("alternative", &self.alternative)
            .finish()
    }
}
impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ")?;
        write!(f, "{}", self.condition)?;
        write!(f, "\n{}", self.consequence)?;

        if let Some(alt) = &self.alternative {
            write!(f, " else {}", alt)?;
        }
        Ok(())
    }
}
