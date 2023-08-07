use crate::ast::{Let, Node, Program, Statement};
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
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
            program.statements.push(self.parse_node()?);
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

    fn parse_node(&mut self) -> anyhow::Result<Statement> {
        let token = self.current_token_unwrap()?;
        match token {
            Token::Let => Ok(Statement::Let(Let::parse(self)?)),
            _ => todo!("unexpected token {:?}", token),
        }
    }

    pub fn current_token_unwrap(&self) -> anyhow::Result<&Token> {
        if self.current_token.is_none() {
            return Err(anyhow::anyhow!("No current token"));
        }
        Ok(self.current_token.as_ref().unwrap())
    }

    pub fn peek_token_wrap(&self) -> anyhow::Result<&Token> {
        if self.peek_token.is_none() {
            return Err(anyhow::anyhow!("No peek token"));
        }
        Ok(self.peek_token.as_ref().unwrap())
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }
}
