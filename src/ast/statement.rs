use std::fmt::Display;

use anyhow::anyhow;

use super::expression::{Expression, Identifier};
use super::Node;
use crate::lexer::Token;
use crate::parser::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Let),
    Return(Return),
    ExpressionStatement(Box<Expression>),
    // ...
}

impl Node for Statement {}

impl Statement {
    pub fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        let token = parser.current_token()?;
        match token {
            Token::Let => Ok(Statement::Let(Let::parse(parser)?)),
            Token::Return => Ok(Statement::Return(Return::parse(parser)?)),
            _ => Ok(Statement::ExpressionStatement(Box::new(Expression::parse(
                parser, LOWEST,
            )?))),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Statement::Let(val) => val.to_string(),
            Statement::Return(val) => val.to_string(),
            Statement::ExpressionStatement(val) => format!("{};", val),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Let {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl Node for Let {}

impl Let {
    fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        parser.next_token();
        let name = Identifier::parse(parser)?;
        parser.next_token();
        if parser.current_token_is(Token::Equal) {
            return Err(anyhow!("Expected `=` symbol in let statement"));
        }
        parser.next_token();
        let value = Expression::parse(parser, LOWEST)?;
        parser.swallow_semicolons();
        Ok(Self {
            token: Token::Let,
            name,
            value,
        })
    }
}

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
    token: Token,
    value: Expression,
}

impl Node for Return {}

impl Return {
    pub fn parse(parser: &mut Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        parser.next_token();
        let value = Expression::parse(parser, LOWEST)?;
        parser.swallow_semicolons();
        Ok(Self {
            token: Token::Return,
            value,
        })
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.value)
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
        let output = parse("return true;;;;");
        assert_eq!(output, "return true;");
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

    test!(literal_let_statement_1, "let x = 5;");
    test!(literal_let_statement_2, "let y = 10;");
    test!(literal_let_statement_3, "let foobar = 838383;");
    test!(literal_let_statement_4, "let foo = \"bar\";");
    test!(literal_let_statement_5, "let foo = true;");
    test!(identifier_let_statement, "let foo = foobar;");

    test!(literal_return_statement_1, "return 5;");
    test!(literal_return_statement_2, "return true;");
    test!(literal_return_statement_3, "return \"foo\";");
    test!(identifier_return_statement, "return foobar;");
}
