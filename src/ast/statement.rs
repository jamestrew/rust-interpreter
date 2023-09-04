use anyhow::anyhow;

use super::expression::{Expression, Identifier};
use super::Node;
use crate::lexer::Token;
use crate::parser::Parser;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Let),
    // ...
}

impl Node for Statement {
    fn parse(parser: &mut Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        let token = parser.current_token()?;
        match token {
            Token::Let => Ok(Statement::Let(Let::parse(parser)?)),
            _ => todo!("Statement::parse for {:?}", token),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Statement::Let(val) => val.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq)]
pub struct Let {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl Let {
    pub fn new(name: Identifier, value: Expression) -> Self {
        Self {
            token: Token::Let,
            name,
            value,
        }
    }
}

impl Node for Let {
    fn parse(parser: &mut Parser) -> anyhow::Result<Self>
    where
        Self: std::marker::Sized,
    {
        parser.next_token();
        let name = Identifier::parse(parser)?;
        parser.next_token();
        if parser.current_token_is(Token::Equal) {
            return Err(anyhow!("Expected `=` symbol in let statement"));
        }
        parser.next_token();
        let value = Expression::parse(parser)?;
        parser.next_token();
        Ok(Self {
            token: Token::Let,
            name,
            value,
        })
    }
}

impl std::fmt::Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
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
}
