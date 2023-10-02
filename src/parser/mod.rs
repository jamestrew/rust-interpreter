mod precedence;

#[cfg(test)]
mod test;

pub use precedence::Precedence;

use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenKind};

fn expected_token_not_found_err<T: AsRef<TokenKind>>(token: T) -> anyhow::Error {
    anyhow::anyhow!("Expected current token {:?} not found", token.as_ref())
}

fn unexpected_missing_token(expected: Option<TokenKind>) -> anyhow::Error {
    if let Some(token_kind) = expected {
        anyhow::anyhow!(
            "Unexpectedly missing token. Expected token kind {:?}",
            token_kind
        )
    } else {
        anyhow::anyhow!("Unexpectedly missing token.")
    }
}

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
        while !self.current_token_is(TokenKind::Eof) {
            program.statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(program)
    }

    pub fn current_token_is<T: AsRef<TokenKind>>(&self, match_token: T) -> bool {
        if let Some(token) = &self.current_token {
            *token.kind() == *match_token.as_ref()
        } else {
            false
        }
    }

    pub fn peek_token_is<T: AsRef<TokenKind>>(&self, match_token: T) -> bool {
        if let Some(token) = &self.peek_token {
            *token.kind() == *match_token.as_ref()
        } else {
            false
        }
    }

    pub fn current_token(&self) -> anyhow::Result<&Token> {
        if self.current_token.is_none() {
            return Err(unexpected_missing_token(None));
        }
        Ok(self.current_token.as_ref().unwrap())
    }

    pub fn peek_token(&self) -> anyhow::Result<&Token> {
        if self.peek_token.is_none() {
            return Err(unexpected_missing_token(None));
        }
        Ok(self.peek_token.as_ref().unwrap())
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    fn expect_current<T: AsRef<TokenKind>>(&mut self, token: T) -> anyhow::Result<Token> {
        if self.current_token_is(&token) {
            self.next_token();
            Ok(self.current_token()?.clone())
        } else {
            Err(expected_token_not_found_err(token))
        }
    }

    pub fn expect_peek<T: AsRef<TokenKind>>(&mut self, token: T) -> anyhow::Result<Token> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(self.current_token()?.clone())
        } else {
            Err(expected_token_not_found_err(token))
        }
    }

    pub fn eat_semicolons(&mut self) {
        while self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }
    }

    pub fn eat_comma(&mut self) {
        if self.current_token_is(TokenKind::Comma) {
            self.next_token();
        }
    }

    pub fn current_precedence(&self) -> anyhow::Result<Precedence> {
        Ok(self.current_token()?.kind().into())
    }

    pub fn peek_precedence(&self) -> anyhow::Result<Precedence> {
        Ok(self.peek_token()?.kind().into())
    }

    fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        match self.current_token()?.kind() {
            TokenKind::Let => self.parse_let(),
            TokenKind::Return => self.parse_return(),
            _ => Ok(Statement::ExpressionStatement(
                self.parse_expression(Precedence::Lowest)?,
            )),
        }
    }

    fn parse_let(&mut self) -> anyhow::Result<Statement> {
        let let_token = self.expect_current(TokenKind::Let)?;
        let name = self.parse_identifer()?;
        self.expect_peek(TokenKind::Assign)?;
        self.expect_current(TokenKind::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.eat_semicolons();
        Ok(Statement::Let(Let::new(let_token, name, value)))
    }

    fn parse_return(&mut self) -> anyhow::Result<Statement> {
        let ret_token = self.expect_current(TokenKind::Return)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.eat_semicolons();
        Ok(Statement::Return(Return::new(ret_token, value)))
    }

    fn parse_block(&mut self) -> anyhow::Result<Block> {
        let block_token = self.expect_current(TokenKind::LBrace)?;

        let mut statements = Vec::new();
        while !self.current_token_is(TokenKind::RBrace) && !self.current_token_is(TokenKind::Eof) {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Block::new(block_token, statements))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> anyhow::Result<Expression> {
        use {Expression as Expr, TokenKind as T};

        let token = self.current_token()?;
        let mut expr = match token.kind() {
            T::Int(_) | T::True | T::False | T::Nil => Expr::Primative(self.parse_primative()?),
            T::Str(_) => Expr::StringLiteral(self.parse_string_literal()?),
            T::Identifier(_) => Expr::Identifier(self.parse_identifer()?),
            T::Minus | T::Bang => Expr::Prefix(self.parse_prefix()?),
            T::If => Expr::If(self.parse_if()?),
            T::LParen => self.parse_grouped()?,
            T::Function => Expr::Function(self.parse_function()?),
            T::LBracket => Expr::Array(self.parse_array()?),
            T::LBrace => Expr::Hash(self.parse_hash()?),
            token => return Err(anyhow::anyhow!("Unexpected token: {}", token)),
        };

        while precedence < self.peek_precedence()? {
            expr = match self.peek_token()?.kind() {
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
                token => return Err(anyhow::anyhow!("Unexpected token: {}", token)),
            };
        }

        self.eat_semicolons();
        Ok(expr)
    }

    fn parse_primative(&mut self) -> anyhow::Result<Primative> {
        let prim_token = self.current_token()?.clone();
        Primative::try_from(prim_token)
    }

    fn parse_string_literal(&mut self) -> anyhow::Result<StringLiteral> {
        let str_token = self.current_token()?.clone();
        Ok(StringLiteral::from(str_token))
    }

    fn parse_identifer(&mut self) -> anyhow::Result<Identifier> {
        let ident_token = self.current_token.take().unwrap();
        Ok(Identifier::new(ident_token))
    }

    fn parse_prefix(&mut self) -> anyhow::Result<Prefix> {
        let op_token = self.current_token()?.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Prefix::new(op_token, right))
    }

    fn parse_infix(&mut self, left: Expression) -> anyhow::Result<Infix> {
        self.next_token();
        let op_token = self.current_token()?.clone();
        let op_precedence = self.current_precedence()?;

        self.next_token();
        let right = self.parse_expression(op_precedence)?;
        Ok(Infix::new(op_token, left, right))
    }

    fn parse_if(&mut self) -> anyhow::Result<If> {
        let if_token = self.expect_current(TokenKind::If)?;
        self.expect_current(TokenKind::LParen)?;

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;

        self.expect_peek(TokenKind::LBrace)?;
        let consequence = self.parse_block()?;

        let mut alternative = None;
        if self.peek_token_is(TokenKind::Else) {
            self.next_token();
            self.expect_peek(TokenKind::LBrace)?;
            alternative = Some(self.parse_block()?);
        }

        Ok(If::new(if_token, condition, consequence, alternative))
    }

    fn parse_grouped(&mut self) -> anyhow::Result<Expression> {
        self.expect_current(TokenKind::LParen)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;
        Ok(expr)
    }

    fn parse_function(&mut self) -> anyhow::Result<Function> {
        let fn_token = self.expect_current(TokenKind::Function)?;
        self.expect_current(TokenKind::LParen)?;

        let mut params = Vec::new();
        while !self.current_token_is(TokenKind::RParen) {
            params.push(self.parse_identifer()?);
            self.next_token();
            self.eat_comma();
        }
        self.expect_current(TokenKind::RParen)?;

        let body = self.parse_block()?;
        Ok(Function::new(fn_token, params, body))
    }

    fn parse_array(&mut self) -> anyhow::Result<Vec<Expression>> {
        self.expect_current(TokenKind::LBracket)?;

        let mut elems = Vec::new();
        while !self.current_token_is(TokenKind::RBracket) {
            elems.push(self.parse_expression(Precedence::Lowest)?);
            self.next_token();
            self.eat_comma();
        }

        Ok(elems)
    }

    fn parse_hash(&mut self) -> anyhow::Result<Hash> {
        let hash_token = self.expect_current(TokenKind::LBrace)?;

        let mut keys = Vec::new();
        let mut values = Vec::new();
        while !self.current_token_is(TokenKind::RBrace) {
            keys.push(self.parse_expression(Precedence::Lowest)?);
            self.expect_peek(TokenKind::Colon)?;
            self.next_token();
            values.push(self.parse_expression(Precedence::Lowest)?);
            self.next_token();
            self.eat_comma();
        }

        Ok(Hash::new(hash_token, keys, values))
    }

    fn parse_call(&mut self, func: Expression) -> anyhow::Result<Call> {
        let call_token = self.expect_peek(TokenKind::LParen)?;
        self.next_token();

        let mut args = Vec::new();
        while !self.current_token_is(TokenKind::RParen) {
            args.push(self.parse_expression(Precedence::Lowest)?);
            self.next_token();
            self.eat_comma();
        }

        Ok(Call::new(call_token, func, args))
    }

    fn parse_index(&mut self, left: Expression) -> anyhow::Result<Index> {
        let idx_token = self.expect_peek(TokenKind::LBracket)?;
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RBracket)?;
        Ok(Index::new(idx_token, left, index))
    }
}
