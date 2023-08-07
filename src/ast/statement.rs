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

#[cfg(test)]
mod test {
    use crate::ast::*;
    use crate::lexer::{Lexer, Token};
    use crate::parser::Parser;

    macro_rules! let_statement {
        ($(($ident:expr, $expr_variant:path, $expr_type:path, $value:expr)),*) => {{
            let mut temp_vec = Vec::new();
        $(
            let ident = Identifier::try_from(&Token::Identifier($ident.into())).unwrap();
            let value = $expr_variant($expr_type($value));
            let stmt = Statement::Let(Let::new(ident, value));
            temp_vec.push(stmt);
        )*
            temp_vec
        }};
    }

    #[test]
    fn let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        let foo = "bar";
        let fool = true;
        "#;

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_programe().expect("valid program");

        let expected = let_statement![
            ("x", Expression::Primative, Primative::Int, 5),
            ("y", Expression::Primative, Primative::Int, 10),
            ("foobar", Expression::Primative, Primative::Int, 838383),
            ("foo", Expression::StringLiteral, std::rc::Rc::from, "bar"),
            ("fool", Expression::Primative, Primative::Bool, true)
        ];

        assert_eq!(program.statements.len(), expected.len());
        assert_eq!(program.statements, expected);
    }
}
