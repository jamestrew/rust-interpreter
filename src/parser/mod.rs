mod precedence;

pub use precedence::Precedence;

use crate::ast::{Program, Statement};
use crate::lexer::{Lexer, Token};

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
            program.statements.push(Statement::parse(self)?);
            self.next_token();
        }
        Ok(program)
    }

    pub fn current_token_is(&self, match_token: Token) -> bool {
        if let Some(token) = &self.current_token {
            *token == match_token
        } else {
            false
        }
    }

    pub fn peek_token_is(&self, match_token: &Token) -> bool {
        if let Some(token) = &self.peek_token {
            *token == *match_token
        } else {
            false
        }
    }

    pub fn current_token(&self) -> anyhow::Result<&Token> {
        if self.current_token.is_none() {
            return Err(anyhow::anyhow!("No current token"));
        }
        Ok(self.current_token.as_ref().unwrap())
    }

    pub fn peek_token(&self) -> anyhow::Result<&Token> {
        if self.peek_token.is_none() {
            return Err(anyhow::anyhow!("No peek token"));
        }
        Ok(self.peek_token.as_ref().unwrap())
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn expect_peek(&mut self, token: Token) -> anyhow::Result<()> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(anyhow::anyhow!("Expected next token {:?} not found", token))
        }
    }

    pub fn swallow_semicolons(&mut self) {
        while self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }
    }

    pub fn current_precedence(&self) -> anyhow::Result<Precedence> {
        Ok(self.current_token()?.into())
    }

    pub fn peek_precedence(&self) -> anyhow::Result<Precedence> {
        Ok(self.peek_token()?.into())
    }
}
