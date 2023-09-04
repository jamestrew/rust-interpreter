use std::rc::Rc;

use super::Node;
use crate::lexer::Token;
use crate::parser::Parser;

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
        let token = parser.current_token()?;
        let expr = match token {
            Token::Int(_) | Token::True | Token::False => {
                Ok(Expression::Primative(Primative::parse(parser)?))
            }
            Token::Str(s) => Ok(Expression::StringLiteral(s.clone())),
            Token::Identifier(_) => Ok(Expression::Identifier(Identifier::parse(parser)?)),
            _ => todo!("Expression::parse for {:?}", token),
        };
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
        };
        write!(f, "{}", s)
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
        let node = Self::try_from(parser.current_token()?);
        node
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
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

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse(input: &str) -> String {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_programe().expect("valid program");
        assert!(!program.statements.is_empty());
        program.statements[0].to_string()
    }

    #[test]
    fn swallow_extra_semicolons() {
        let output = parse("foobar;;;;");
        assert_eq!(output, "foobar;");
    }

    macro_rules! test {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let output = parse($input);
                assert_eq!($input, output);
            }
        };
    }

    test!(identifier, "foobar;");
    test!(integer_literal, "1;");
    test!(boolean_literal, "true;");
    test!(string_literal, "\"hello world\";");
}
