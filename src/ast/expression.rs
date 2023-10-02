use std::fmt::{Debug, Display};
use std::rc::Rc;

use super::*;
use crate::lexer::{Token, TokenKind};

#[derive(PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Primative(Primative),
    StringLiteral(StringLiteral),
    Prefix(Prefix),
    Infix(Infix),
    If(If),
    Function(Function),
    Call(Call),
    Array(Vec<Expression>),
    Hash(Hash),
    Index(Index),
}

impl Node for Expression {}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(val) => write!(f, "{:?}", val),
            Expression::Primative(val) => write!(f, "{:?}", val),
            Expression::StringLiteral(val) => write!(f, "{:?}", val),
            Expression::Prefix(val) => write!(f, "{:?}", val),
            Expression::Infix(val) => write!(f, "{:?}", val),
            Expression::If(val) => write!(f, "{:?}", val),
            Expression::Function(val) => write!(f, "{:?}", val),
            Expression::Call(val) => write!(f, "{:?}", val),
            Expression::Array(val) => write!(f, "Array{:?}", val),
            Expression::Hash(val) => write!(f, "{:?}", val),
            Expression::Index(val) => write!(f, "{:?}", val),
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
            Expression::Hash(val) => val.to_string(),
            Expression::Index(val) => val.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Identifier {
    token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        Self { token }
    }

    pub fn value(&self) -> Rc<str> {
        if let TokenKind::Identifier(ident) = self.token.kind() {
            return Rc::clone(ident);
        } else {
            unreachable!()
        }
    }
}

impl Node for Identifier {}

impl TryFrom<Token> for Identifier {
    type Error = anyhow::Error;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        if let TokenKind::Identifier(_) = token.kind() {
            return Ok(Self { token });
        } else {
            Err(anyhow::anyhow!(
                "Unexpected token kind '{:?}'. Expected 'Identifier'.",
                token.kind()
            ))
        }
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Identifier({:?})", self.value())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

#[derive(PartialEq, Clone)]
pub enum Primative {
    Int { token: Token, value: i64 },
    Bool { token: Token, value: bool },
    Nil { token: Token },
}

impl Node for Primative {}

impl TryFrom<Token> for Primative {
    type Error = anyhow::Error;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        let kind = token.kind().clone();
        match kind {
            TokenKind::Int(val) => Ok(Self::Int {
                token,
                value: val.parse::<i64>()?,
            }),
            TokenKind::True => Ok(Self::Bool { token, value: true }),
            TokenKind::False => Ok(Self::Bool {
                token,
                value: false,
            }),
            TokenKind::Nil => Ok(Self::Nil { token }),
            _ => unreachable!("Primative parse unexpected {:?}", token),
        }
    }
}

impl Debug for Primative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "Int({value})"),
            Self::Bool { value, .. } => write!(f, "Bool({value})"),
            Self::Nil { .. } => write!(f, "Nil"),
        }
    }
}

impl Display for Primative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "{value}"),
            Self::Bool { value, .. } => write!(f, "{value}"),
            Self::Nil { .. } => write!(f, "Nil"),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct StringLiteral {
    token: Token,
}

impl Node for StringLiteral {}

impl From<Token> for StringLiteral {
    fn from(token: Token) -> Self {
        if let TokenKind::Str(_) = token.kind() {
            Self { token }
        } else {
            unreachable!(
                "Unable to generate StringLiteral from {} token kind",
                token.kind()
            )
        }
    }
}

impl Debug for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let TokenKind::Str(val) = self.token.kind() {
            write!(f, "StringLiteral(\"{}\")", val)
        } else {
            unreachable!()
        }
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let TokenKind::Str(val) = self.token.kind() {
            write!(f, "{val}")
        } else {
            unreachable!()
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
        match self.token.kind() {
            TokenKind::Minus => "-",
            TokenKind::Bang => "!",
            _ => Self::unreachable_operator(),
        }
    }

    pub fn operator(&self) -> &TokenKind {
        &self.token.kind()
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
        match &self.token.kind() {
            TokenKind::Minus => "-",
            TokenKind::Plus => "+",
            TokenKind::Asterisk => "*",
            TokenKind::ForwardSlash => "/",
            TokenKind::Equal => "==",
            TokenKind::NotEqual => "!=",
            TokenKind::LT => "<",
            TokenKind::GT => ">",
            token => Infix::unreachable_operator(token),
        }
    }

    pub fn operator(&self) -> &TokenKind {
        &self.token.kind()
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }

    pub fn unreachable_operator(token: &TokenKind) -> ! {
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
    pub fn new(
        token: Token,
        condition: Expression,
        consequence: Block,
        alternative: Option<Block>,
    ) -> Self {
        Self {
            token,
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
    token: Token,
    params: Vec<Identifier>,
    body: Block,
}

impl Node for Function {}

impl Function {
    pub fn new(token: Token, params: Vec<Identifier>, body: Block) -> Self {
        Self {
            token,
            params,
            body,
        }
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
    token: Token,
    function: Box<Expression>,
    args: Vec<Expression>,
}

impl Node for Call {}

impl Call {
    pub fn new(token: Token, function: Expression, args: Vec<Expression>) -> Self {
        Self {
            token,
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

#[derive(Debug, PartialEq, Clone)]
pub struct Hash {
    token: Token,
    keys: Vec<Expression>,
    values: Vec<Expression>,
}

impl Hash {
    pub fn new(token: Token, keys: Vec<Expression>, values: Vec<Expression>) -> Self {
        Self {
            token,
            keys,
            values,
        }
    }

    pub fn items(&self) -> impl Iterator<Item = (&Expression, &Expression)> {
        self.keys.iter().zip(self.values.iter())
    }
}

impl Node for Hash {}

impl Display for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (idx, (key, value)) in self.keys.iter().zip(self.values.iter()).enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Index {
    token: Token,
    left: Box<Expression>,
    index: Box<Expression>,
}

impl Index {
    pub fn new(token: Token, left: Expression, index: Expression) -> Self {
        Self {
            token,
            left: Box::new(left),
            index: Box::new(index),
        }
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn index(&self) -> &Expression {
        &self.index
    }
}

impl Node for Index {}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}
