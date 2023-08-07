use crate::ast::{Let, Program, Statement};
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        Self {
            lexer,
            current_token: None,
            peek_token: None,
        }
    }

    pub fn parse_programe(&mut self) -> anyhow::Result<Program<'a>> {
        let mut program = Program::new();
        while !self.current_token_is(Token::Eof) {
            program.statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(program)
    }

    fn current_token_is(&self, match_token: Token<'a>) -> bool {
        if let Some(token) = &self.current_token {
            *token == match_token
        } else {
            false
        }
    }

    fn parse_statement(&mut self) -> anyhow::Result<Statement<'a>> {
        if self.current_token.is_none() {
            return Err(anyhow::anyhow!("No current token"));
        }

        let token = &self.current_token.as_ref().unwrap();
        match token {
            Token::Let => Ok(Statement::Let(Let::try_from(self)?)),
            _ => todo!(),
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_programe().expect("valid program");
    }
}
