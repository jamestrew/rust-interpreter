use std::fmt::{Debug, Display};

use super::expression::{Expression, Identifier};
use super::Node;
use crate::lexer::Token;

#[derive(PartialEq, Clone)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Block(Block),
    ExpressionStatement(Expression),
    // ...
}

impl Node for Statement {}

impl Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(val) => write!(f, "{:?}", val),
            Self::Return(val) => write!(f, "{:?}", val),
            Self::Block(val) => write!(f, "{:?}", val),
            Self::ExpressionStatement(val) => {
                f.debug_tuple("ExpressionStatement").field(val).finish()
            }
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Statement::Let(val) => val.to_string(),
            Statement::Return(val) => val.to_string(),
            Statement::Block(val) => val.to_string(),
            Statement::ExpressionStatement(val) => format!("{};", val),
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Clone)]
pub struct Let {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl Node for Let {}

impl Let {
    pub fn new(name: Identifier, value: Expression) -> Self {
        Self {
            token: Token::Let,
            name,
            value,
        }
    }

    pub fn name(&self) -> &Identifier {
        &self.name
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Debug for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Let")
            .field("name", &self.name)
            .field("value", &self.value)
            .finish()
    }
}

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(PartialEq, Clone)]
pub struct Return {
    token: Token,
    value: Expression,
}

impl Node for Return {}

impl Return {
    pub fn new(value: Expression) -> Self {
        Self {
            token: Token::Return,
            value,
        }
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Debug for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Return")
            .field("value", &self.value)
            .finish()
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub struct Block {
    token: Token,
    statements: Vec<Statement>,
}

impl Node for Block {}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            token: Token::LBrace,
            statements,
        }
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }
}

impl Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("token", &self.token)
            .field("statements", &self.statements)
            .finish()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.statements {
            f.write_fmt(format_args!("\t{}\n", stmt))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}
