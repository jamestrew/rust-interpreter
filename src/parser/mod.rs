mod precedence;

#[cfg(test)]
mod test;

pub use precedence::Precedence;

use crate::ast::*;
use crate::errors::{ParserError, Result};
use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    // prev_token: Option<Token>,
    current_token: Option<Result<Token>>,
    peek_token: Option<Result<Token>>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next();
        let peek_token = lexer.next();
        Self {
            lexer,
            // prev_token: None,
            current_token,
            peek_token,
        }
    }

    pub fn parse_programe(&mut self) -> Result<Program> {
        let mut program = Program::default();
        while !self.current_token_is(TokenKind::Eof)? {
            program.statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(program)
    }

    pub fn current_token_is<T: AsRef<TokenKind>>(&self, match_token: T) -> Result<bool> {
        Ok(*match_token.as_ref() == **self.current_token()?)
    }

    pub fn peek_token_is<T: AsRef<TokenKind>>(&self, match_token: T) -> Result<bool> {
        Ok(*match_token.as_ref() == **self.peek_token()?)
    }

    pub fn current_token(&self) -> Result<&Token> {
        match &self.current_token {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err.clone()),
            None => todo!("return UnexpectedEOF with prev_token span"),
        }
    }

    pub fn peek_token(&self) -> Result<&Token> {
        match &self.peek_token {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err.clone()),
            None => Err(self.current_token()?.map(ParserError::UnexpectedEOF.into())),
        }
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    fn expect_current<T: AsRef<TokenKind>>(&mut self, token: T) -> Result<Token> {
        if self.current_token_is(&token)? {
            self.next_token();
            Ok(self.current_token()?.clone())
        } else {
            Err(self
                .current_token()?
                .map(ParserError::ExpectedTokenNotFound(token.as_ref().to_string()).into()))
        }
    }

    pub fn expect_peek<T: AsRef<TokenKind>>(&mut self, token: T) -> Result<Token> {
        if self.peek_token_is(&token)? {
            self.next_token();
            Ok(self.current_token()?.clone())
        } else {
            Err(self
                .peek_token()?
                .map(ParserError::ExpectedTokenNotFound(token.as_ref().to_string()).into()))
        }
    }

    pub fn eat_semicolons(&mut self) {
        while self.peek_token_is(TokenKind::Semicolon).unwrap_or(false) {
            self.next_token();
        }
    }

    pub fn eat_comma(&mut self) {
        if self.current_token_is(TokenKind::Comma).unwrap_or(false) {
            self.next_token();
        }
    }

    pub fn current_precedence(&self) -> Result<Precedence> {
        let token = &**self.current_token()?;
        Ok(token.into())
    }

    pub fn peek_precedence(&self) -> Result<Precedence> {
        let token = &**self.peek_token()?;
        Ok(token.into())
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match **self.current_token()? {
            TokenKind::Let => self.parse_let(),
            TokenKind::Return => self.parse_return(),
            _ => Ok(Statement::ExpressionStatement(
                self.parse_expression(Precedence::Lowest)?,
            )),
        }
    }

    fn parse_let(&mut self) -> Result<Statement> {
        let let_token = self.expect_current(TokenKind::Let)?;
        let name = self.parse_identifer()?;
        self.expect_peek(TokenKind::Assign)?;
        self.expect_current(TokenKind::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.eat_semicolons();
        Ok(Statement::Let(Let::new(let_token, name, value)))
    }

    fn parse_return(&mut self) -> Result<Statement> {
        let ret_token = self.expect_current(TokenKind::Return)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.eat_semicolons();
        Ok(Statement::Return(Return::new(ret_token, value)))
    }

    fn parse_block(&mut self) -> Result<Block> {
        let block_token = self.expect_current(TokenKind::LBrace)?;

        let mut statements = Vec::new();
        while !self.current_token_is(TokenKind::RBrace)?
            && !self.current_token_is(TokenKind::Eof)?
        {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Block::new(block_token, statements))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        use {Expression as Expr, TokenKind as T};

        let token = self.current_token()?;
        let mut expr = match **token {
            T::Int(_) | T::True | T::False | T::Nil => Expr::Primative(self.parse_primative()?),
            T::Str(_) => Expr::StringLiteral(self.parse_string_literal()?),
            T::Identifier(_) => Expr::Identifier(self.parse_identifer()?),
            T::Minus | T::Bang => Expr::Prefix(self.parse_prefix()?),
            T::If => Expr::If(self.parse_if()?),
            T::LParen => self.parse_grouped()?,
            T::Function => Expr::Function(self.parse_function()?),
            T::LBracket => Expr::Array(self.parse_array()?),
            T::LBrace => Expr::Hash(self.parse_hash()?),
            _ => {
                return Err(token.map(ParserError::UnexpectedToken(token.to_string()).into()));
            }
        };

        while precedence < self.peek_precedence()? {
            let peek_token = self.peek_token()?;
            expr = match **peek_token {
                T::Assign
                | T::Plus
                | T::Minus
                | T::Asterisk
                | T::ForwardSlash
                | T::Equal
                | T::NotEqual
                | T::LT
                | T::GT
                | T::Eof => Expr::Infix(self.parse_infix(expr)?),
                T::LParen => Expr::Call(self.parse_call(expr)?),
                T::LBracket => Expr::Index(self.parse_index(expr)?),
                _ => {
                    return Err(
                        peek_token.map(ParserError::UnexpectedToken(peek_token.to_string()).into())
                    );
                }
            };
        }

        self.eat_semicolons();
        Ok(expr)
    }

    fn parse_primative(&mut self) -> Result<Primative> {
        let prim_token = self.current_token()?.clone();
        Primative::try_from(prim_token)
    }

    fn parse_string_literal(&mut self) -> Result<StringLiteral> {
        let str_token = self.current_token()?.clone();
        Ok(StringLiteral::from(str_token))
    }

    fn parse_identifer(&mut self) -> Result<Identifier> {
        let ident_token = self.current_token.take().unwrap()?;
        Ok(Identifier::new(ident_token))
    }

    fn parse_prefix(&mut self) -> Result<Prefix> {
        let op_token = self.current_token()?.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Prefix::new(op_token, right))
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Infix> {
        self.next_token();
        let op_token = self.current_token()?.clone();
        let op_precedence = self.current_precedence()?;

        self.next_token();
        let right = self.parse_expression(op_precedence)?;
        Ok(Infix::new(op_token, left, right))
    }

    fn parse_if(&mut self) -> Result<If> {
        let if_token = self.expect_current(TokenKind::If)?;
        self.expect_current(TokenKind::LParen)?;

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;

        self.expect_peek(TokenKind::LBrace)?;
        let consequence = self.parse_block()?;

        let mut alternative = None;
        if self.peek_token_is(TokenKind::Else)? {
            self.next_token();
            self.expect_peek(TokenKind::LBrace)?;
            alternative = Some(self.parse_block()?);
        }

        Ok(If::new(if_token, condition, consequence, alternative))
    }

    fn parse_grouped(&mut self) -> Result<Expression> {
        self.expect_current(TokenKind::LParen)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;
        Ok(expr)
    }

    fn parse_function(&mut self) -> Result<Function> {
        let fn_token = self.expect_current(TokenKind::Function)?;
        self.expect_current(TokenKind::LParen)?;

        let mut params = Vec::new();
        while !self.current_token_is(TokenKind::RParen)? {
            params.push(self.parse_identifer()?);
            self.next_token();
            self.eat_comma();
        }
        self.expect_current(TokenKind::RParen)?;

        let body = self.parse_block()?;
        Ok(Function::new(fn_token, params, body))
    }

    fn parse_array(&mut self) -> Result<Vec<Expression>> {
        self.expect_current(TokenKind::LBracket)?;

        let mut elems = Vec::new();
        while !self.current_token_is(TokenKind::RBracket)? {
            elems.push(self.parse_expression(Precedence::Lowest)?);
            self.next_token();
            self.eat_comma();
        }

        Ok(elems)
    }

    fn parse_hash(&mut self) -> Result<Hash> {
        let hash_token = self.expect_current(TokenKind::LBrace)?;

        let mut keys = Vec::new();
        let mut values = Vec::new();
        while !self.current_token_is(TokenKind::RBrace)? {
            keys.push(self.parse_expression(Precedence::Lowest)?);
            self.expect_peek(TokenKind::Colon)?;
            self.next_token();
            values.push(self.parse_expression(Precedence::Lowest)?);
            self.next_token();
            self.eat_comma();
        }

        Ok(Hash::new(hash_token, keys, values))
    }

    fn parse_call(&mut self, func: Expression) -> Result<Call> {
        let call_token = self.expect_peek(TokenKind::LParen)?;
        self.next_token();

        let mut args = Vec::new();
        while !self.current_token_is(TokenKind::RParen)? {
            args.push(self.parse_expression(Precedence::Lowest)?);
            self.next_token();
            self.eat_comma();
        }

        Ok(Call::new(call_token, func, args))
    }

    fn parse_index(&mut self, left: Expression) -> Result<Index> {
        let idx_token = self.expect_peek(TokenKind::LBracket)?;
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RBracket)?;
        Ok(Index::new(idx_token, left, index))
    }
}
