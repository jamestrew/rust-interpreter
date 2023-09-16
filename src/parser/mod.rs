mod precedence;

#[cfg(test)]
mod test;

pub use precedence::Precedence;

use crate::ast::*;
use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next();
        let peek_token = lexer.next();
        Self {
            lexer,
            current_token,
            peek_token,
        }
    }

    pub fn parse_programe(&mut self) -> anyhow::Result<Program> {
        let mut program = Program::default();
        while !self.current_token_is(Token::Eof) {
            program.statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(program)
    }

    pub fn current_token_is<T: AsRef<Token>>(&self, match_token: T) -> bool {
        if let Some(token) = &self.current_token {
            *token == *match_token.as_ref()
        } else {
            false
        }
    }

    pub fn peek_token_is<T: AsRef<Token>>(&self, match_token: T) -> bool {
        if let Some(token) = &self.peek_token {
            *token == *match_token.as_ref()
        } else {
            false
        }
    }

    pub fn current_token(&self) -> anyhow::Result<&Token> {
        if self.current_token.is_none() {
            return Ok(&Token::Eof);
        }
        Ok(self.current_token.as_ref().unwrap())
    }

    pub fn peek_token(&self) -> anyhow::Result<&Token> {
        if self.peek_token.is_none() {
            return Ok(&Token::Eof);
        }
        Ok(self.peek_token.as_ref().unwrap())
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    fn expect_current<T: AsRef<Token>>(&mut self, token: T) -> anyhow::Result<()> {
        if self.current_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "Expected current token {:?} not found",
                token.as_ref()
            ))
        }
    }

    pub fn expect_peek<T: AsRef<Token>>(&mut self, token: T) -> anyhow::Result<()> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "Expected next token {:?} not found",
                token.as_ref()
            ))
        }
    }

    pub fn swallow_semicolons(&mut self) {
        while self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }
    }

    pub fn current_precedence(&self) -> anyhow::Result<Precedence> {
        Ok(self.current_token()?.into())
    }

    pub fn peek_precedence(&self) -> anyhow::Result<Precedence> {
        Ok(self.peek_token()?.into())
    }

    fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        match self.current_token()? {
            Token::Let => self.parse_let(),
            Token::Return => self.parse_return(),
            Token::LBrace => self.parse_block(),
            _ => Ok(Statement::ExpressionStatement(
                self.parse_expression(Precedence::Lowest)?,
            )),
        }
    }

    fn parse_let(&mut self) -> anyhow::Result<Statement> {
        self.expect_current(Token::Let)?;
        let name = self.parse_identifer()?;
        self.expect_peek(Token::Assign)?;
        self.expect_current(Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.swallow_semicolons();
        Ok(Statement::Let(Let::new(name, value)))
    }

    fn parse_return(&mut self) -> anyhow::Result<Statement> {
        self.expect_current(Token::Return)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.swallow_semicolons();
        Ok(Statement::Return(Return::new(value)))
    }

    fn parse_block(&mut self) -> anyhow::Result<Statement> {
        todo!("parse_block")
    }

    fn parse_expression(&mut self, precedence: Precedence) -> anyhow::Result<Expression> {
        use {Expression as Expr, Token as T};

        let token = self.current_token()?;
        let expr = match token {
            T::Int(_) | T::True | T::False => Expr::Primative(Primative::try_from(token)?),
            T::Str(s) => Expr::StringLiteral(s.clone()),
            T::Identifier(val) => Expr::Identifier(Identifier::from(val)),
            T::Minus | T::Bang => Expr::Prefix(self.parse_prefix()?),
            T::LParen => todo!(),
            T::If => todo!(),
            _ => unreachable!("parse_expression for {:?}", token),
        };

        while precedence < self.peek_precedence()? {
            todo!()
        }

        self.swallow_semicolons();
        Ok(expr)
    }

    fn parse_identifer(&self) -> anyhow::Result<Identifier> {
        Ok(Identifier::try_from(self.current_token()?)?)
    }

    fn parse_prefix(&mut self) -> anyhow::Result<Prefix> {
        let operator = self.current_token()?.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Prefix::new(operator, right))
    }
}
