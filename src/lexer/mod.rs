#[cfg(test)]
mod test;
mod tokens;

use std::str::from_utf8;

pub use tokens::{Token, TokenKind};

use crate::errors::{Result, TokenizerError};
use crate::types::Spanned;

#[derive(Debug, Default)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
    row: usize,
    col: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.as_bytes().to_vec(),
            ..Default::default()
        };
        lexer.read_char();
        lexer.col = 0;
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        if self.is_letter() {
            return self.get_ident_or_kw();
        }

        let span_start = self.col;
        let token_type = match self.ch {
            b'=' => self.if_peek(b'=', TokenKind::Equal, TokenKind::Assign),
            b'+' => TokenKind::Plus,
            b'-' => TokenKind::Minus,
            b'*' => TokenKind::Asterisk,
            b'/' => TokenKind::ForwardSlash,
            b'!' => self.if_peek(b'=', TokenKind::NotEqual, TokenKind::Bang),

            b'<' => TokenKind::LT,
            b'>' => TokenKind::GT,

            b',' => TokenKind::Comma,
            b';' => TokenKind::Semicolon,
            b':' => TokenKind::Colon,

            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,

            b'0'..=b'9' => return self.get_int(),
            b'"' => return self.get_str(),

            0 => {
                self.read_char();
                return Ok(Token::new(self.row, span_start..span_start, TokenKind::Eof));
            }
            _ => {
                return Err(Spanned::new(
                    self.row,
                    span_start..span_start,
                    TokenizerError::UnexpectedInput(self.ch as char).into(),
                ));
            }
        };

        self.read_char();
        let span_end = self.col;
        Ok(Token::new(self.row, span_start..span_end, token_type))
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            let last_char = self.ch;
            self.read_char();
            if last_char == b'\n' {
                self.row += 1;
                self.col = 0;
            }
        }
    }

    fn is_letter(&self) -> bool {
        matches!(self.ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' )
    }

    fn get_ident_or_kw(&mut self) -> Result<Token> {
        let span_start = self.col;
        let ident = self.read_ident()?;
        let token_type = match ident {
            "let" => TokenKind::Let,
            "fn" => TokenKind::Function,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Return,
            "nil" => TokenKind::Nil,
            _ => TokenKind::Identifier(ident.into()),
        };

        let span_end = self.col;

        Ok(Token::new(self.row, span_start..span_end, token_type))
    }

    fn read_ident(&mut self) -> Result<&str> {
        let start = self.position;
        while self.is_letter() {
            self.read_char()
        }

        from_utf8(&self.input[start..self.position]).map_err(|_| {
            Spanned::new(
                self.row,
                start..self.position,
                TokenizerError::NonUTF8Input.into(),
            )
        })
    }

    fn get_int(&mut self) -> Result<Token> {
        let start = self.position;
        let span_start = self.col;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let span_end = self.col;
        let val = from_utf8(&self.input[start..self.position]).map_err(|_| {
            Spanned::new(
                self.row,
                start..self.position,
                TokenizerError::NonUTF8Input.into(),
            )
        })?;
        Ok(Token::new(
            self.row,
            span_start..span_end,
            TokenKind::Int(val.into()),
        ))
    }

    fn get_str(&mut self) -> Result<Token> {
        let span_start = self.col;
        self.read_char();
        let start = self.position;
        while self.ch != b'"' {
            self.read_char();
        }
        let val = from_utf8(&self.input[start..self.position]).map_err(|_| {
            Spanned::new(
                self.row,
                start..self.position,
                TokenizerError::NonUTF8Input.into(),
            )
        })?;

        let token_type = TokenKind::Str(val.into());
        self.read_char();
        let span_end = self.col;
        Ok(Token::new(self.row, span_start..span_end, token_type))
    }

    fn if_peek(&mut self, peek: u8, true_token: TokenKind, false_token: TokenKind) -> TokenKind {
        if let Some(token) = self.peek() {
            if token == peek {
                self.read_char();
                return true_token;
            }
        }
        false_token
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.read_position).copied()
    }
}

impl Iterator for Lexer {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();

        if let Ok(ref token) = token {
            if **token == TokenKind::Eof && self.position - 1 > self.input.len() {
                return None;
            }
        }
        Some(token)
    }
}
