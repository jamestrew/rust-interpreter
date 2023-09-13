use std::fmt::{Debug, Display};

use anyhow::anyhow;

use super::expression::{Expression, Identifier};
use super::Node;
use crate::lexer::Token;
use crate::parser::*;

#[derive(PartialEq, Clone)]
pub enum Statement {
    Let(Let),
    Return(Return),
    ExpressionStatement(Box<Expression>),
    Block(Block),
    // ...
}

impl Node for Statement {}

impl Statement {
    pub fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        let token = parser.current_token()?;
        match token {
            Token::Let => Ok(Statement::Let(Let::parse(parser)?)),
            Token::Return => Ok(Statement::Return(Return::parse(parser)?)),
            Token::LBrace => Ok(Statement::Block(Block::parse(parser)?)),
            _ => Ok(Statement::ExpressionStatement(Box::new(Expression::parse(
                parser,
                Precedence::Lowest,
            )?))),
        }
    }
}

impl Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(val) => write!(f, "{:?}", val),
            Self::Return(val) => write!(f, "{:?}", val),
            Self::ExpressionStatement(val) => {
                f.debug_tuple("ExpressionStatement").field(val).finish()
            }
            Self::Block(val) => write!(f, "{:?}", val),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Statement::Let(val) => val.to_string(),
            Statement::Return(val) => val.to_string(),
            Statement::ExpressionStatement(val) => format!("{};", val),
            Statement::Block(val) => val.to_string(),
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
    fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        parser.next_token();
        let name = Identifier::parse(parser)?;
        parser.next_token();
        if parser.current_token_is(&Token::Equal) {
            return Err(anyhow!("Expected `=` symbol in let statement"));
        }
        parser.next_token();
        let value = Expression::parse(parser, Precedence::Lowest)?;
        parser.swallow_semicolons();
        Ok(Self {
            token: Token::Let,
            name,
            value,
        })
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
    pub fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        parser.next_token();
        let value = Expression::parse(parser, Precedence::Lowest)?;
        parser.swallow_semicolons();
        Ok(Self {
            token: Token::Return,
            value,
        })
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
    pub fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        parser.next_token();
        let mut statements = Vec::new();

        while !parser.current_token_is(&Token::RBrace) {
            println!("{:?}", parser.current_token());
            statements.push(Statement::parse(parser)?);
            parser.next_token();
        }

        println!("outside - {:?}", parser.current_token());

        Ok(Self {
            token: Token::LBrace,
            statements,
        })
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
        for stmt in &self.statements {
            f.write_fmt(format_args!("{}", stmt))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::Statement;
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
        let stmts = parse("return true;;;;");
        let stmt = stmts[0].to_string();
        assert_eq!(stmt, "return true;");
    }

    macro_rules! assert_stmt {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let stmts = parse($input);
                let stmt = stmts[0].to_string();
                assert_eq!($input, stmt);
            }
        };
    }

    assert_stmt!(literal_let_statement_1, "let x = 5;");
    assert_stmt!(literal_let_statement_2, "let y = 10;");
    assert_stmt!(literal_let_statement_3, "let foobar = 838383;");
    assert_stmt!(literal_let_statement_4, "let foo = \"bar\";");
    assert_stmt!(literal_let_statement_5, "let foo = true;");
    assert_stmt!(identifier_let_statement, "let foo = foobar;");

    assert_stmt!(literal_return_statement_1, "return 5;");
    assert_stmt!(literal_return_statement_2, "return true;");
    assert_stmt!(literal_return_statement_3, "return \"foo\";");
    assert_stmt!(identifier_return_statement, "return foobar;");

    macro_rules! assert_stmts {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let stmts = parse($input);
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_debug_snapshot!(stmts);
                })
            }
        };
    }

    assert_stmts!(block_1, "{ return 5; }");
    assert_stmts!(block_2, "{ return 5; return true; }");
}
