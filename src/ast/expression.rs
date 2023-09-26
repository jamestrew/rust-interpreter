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
    Function(Function),
    Call(Call),
    Array(Vec<Expression>),
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
            Expression::Function(val) => write!(f, "{:?}", val),
            Expression::Call(val) => write!(f, "{:?}", val),
            Expression::Array(val) => write!(f, "Array{:?}", val),
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
            Expression::Function(val) => val.to_string(),
            Expression::Call(val) => val.to_string(),
            Expression::Array(val) => format!(
                "[{}]",
                val.iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            ),
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Identifier {
    value: Rc<str>,
}

impl Identifier {
    pub fn new(val: &str) -> Self {
        Self { value: val.into() }
    }
}

impl Node for Identifier {}

impl From<&Rc<str>> for Identifier {
    fn from(value: &Rc<str>) -> Self {
        Self {
            value: value.clone(),
        }
    }
}

impl<'a> TryFrom<&'a Token> for Identifier {
    type Error = anyhow::Error;

    fn try_from(token: &'a Token) -> anyhow::Result<Self> {
        if let Token::Identifier(ident) = token {
            Ok(Self {
                value: ident.clone(),
            })
        } else {
            Err(anyhow::anyhow!("Invalid identifier: {}", token))
        }
    }
}

impl std::ops::Deref for Identifier {
    type Target = Rc<str>;

    fn deref(&self) -> &Self::Target {
        &self.value
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

    pub fn operator_str(&self) -> &'static str {
        match &self.token {
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Asterisk => "*",
            Token::ForwardSlash => "/",
            Token::Equal => "==",
            Token::NotEqual => "!=",
            Token::LT => "<",
            Token::GT => ">",
            token => Infix::unreachable_operator(token),
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

    pub fn unreachable_operator(token: &Token) -> ! {
        unreachable!("unallowed infix operator {:?}", token)
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

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn consequence(&self) -> &Block {
        &self.consequence
    }

    pub fn alternative(&self) -> Option<&Block> {
        self.alternative.as_ref()
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

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    params: Vec<Identifier>,
    body: Block,
}

impl Node for Function {}

impl Function {
    pub fn new(params: Vec<Identifier>, body: Block) -> Self {
        Self { params, body }
    }

    pub fn params(&self) -> &[Identifier] {
        &self.params
    }

    pub fn body(&self) -> &Block {
        &self.body
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (idx, param) in self.params.iter().enumerate() {
            if idx != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ") {}", self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    function: Box<Expression>,
    args: Vec<Expression>,
}

impl Node for Call {}

impl Call {
    pub fn new(function: Expression, args: Vec<Expression>) -> Self {
        Self {
            function: Box::new(function),
            args,
        }
    }

    pub fn function(&self) -> &Expression {
        &self.function
    }

    pub fn args(&self) -> &[Expression] {
        &self.args
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.function)?;
        for (idx, arg) in self.args.iter().enumerate() {
            if idx != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}
