use std::fmt::{Debug, Display};
use std::rc::Rc;

use super::Node;
use crate::lexer::Token;
use crate::parser::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Primative(Primative),
    StringLiteral(Rc<str>),
    Prefix(Prefix),
    Infix(Infix),
}

impl Node for Expression {}

impl Expression {
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> anyhow::Result<Self> {
        let token = parser.current_token()?;
        let expr = match token {
            Token::Int(_) | Token::True | Token::False => {
                Ok(Expression::Primative(Primative::parse(parser)?))
            }
            Token::Str(s) => Ok(Expression::StringLiteral(s.clone())),
            Token::Identifier(_) => Ok(Expression::Identifier(Identifier::parse(parser)?)),
            Token::Minus | Token::Bang => {
                Ok(Expression::Prefix(Prefix::parse(parser, precedence)?))
            }
            _ => todo!("Expression::parse for {:?}", token),
        };

        // while precedence < parser.peek_precedence()? {}
        parser.swallow_semicolons();
        expr
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expression::Identifier(val) => val.to_string(),
            Expression::Primative(val) => val.to_string(),
            Expression::StringLiteral(val) => format!("\"{}\"", val),
            Expression::Prefix(val) => val.to_string(),
            Expression::Infix(val) => val.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    token: Token,
    value: Rc<str>,
}

impl Node for Identifier {}

impl Identifier {
    pub fn parse(parser: &Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        Self::try_from(parser.current_token()?)
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

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Primative {
    Int(i64),
    Bool(bool),
}

impl Node for Primative {}

impl Primative {
    pub fn parse(parser: &Parser) -> anyhow::Result<Self> {
        let token = parser.current_token()?;
        match token {
            Token::Int(val) => Ok(Self::Int(val.parse::<i64>()?)),
            Token::True => Ok(Self::Bool(true)),
            Token::False => Ok(Self::Bool(false)),
            _ => unreachable!("Primative parse unexpected {:?}", token),
        }
    }
}

impl std::fmt::Display for Primative {
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
    pub fn parse(parser: &mut Parser, _precedence: Precedence) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        let operator = parser.current_token()?.clone();
        parser.next_token();
        let right = Expression::parse(parser, Precedence::Prefix)?;
        Ok(Self {
            token: operator,
            right: Box::new(right),
        })
    }
    fn operator_str(&self) -> &'static str {
        match self.token {
            Token::Minus => "-",
            Token::Bang => "!",
            _ => unreachable!("only Bang and Minus are allowed prefixes"),
        }
    }
}

impl Debug for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator_str(), self.right)
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator_str(), self.right)
    }
}

#[derive(PartialEq, Clone)]
pub struct Infix {
    token: Token,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl Infix {
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
            _ => unreachable!("only Bang and Minus are allowed prefixes"),
        }
    }
}

impl Debug for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator_str(), self.right)
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator_str(), self.right)
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse(input: &str) -> Vec<Statement> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_programe().expect("valid program");
        assert!(!program.statements.is_empty());
        program.statements
    }

    #[test]
    fn swallow_extra_semicolons() {
        let statements = parse("foobar;;;;");
        let first = statements[0].to_string();
        assert_eq!(first, "foobar;");
    }

    macro_rules! test {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let statements = parse($input);
                let first = statements[0].to_string();
                assert_eq!($input, first);
            }
        };
    }

    macro_rules! snapshot_debug {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let statements = parse($input);
                let first = &statements[0];
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_debug_snapshot!(first);
                })
            }
        };
    }

    test!(identifier, "foobar;");
    test!(integer_literal, "1;");
    test!(boolean_literal, "true;");
    test!(string_literal, "\"hello world\";");

    snapshot_debug!(prefix_expression_1, "-5;");
    snapshot_debug!(prefix_expression_2, "!foobar;");

    snapshot_debug!(infix_expr_1, "5 + 5;");
}
