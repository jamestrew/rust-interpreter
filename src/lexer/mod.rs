#![allow(dead_code)]
mod tokens;

use std::str::{from_utf8, Utf8Error};

use thiserror::Error;
use tokens::Token;

#[derive(Error, Debug)]
enum TokenizerError {
    #[error("Unexpected input {0}. Failed to tokenize.")]
    UnexpectedInput(char),

    #[error("Invalid non UTF-8 string found: {0}")]
    NonUTF8Input(Utf8Error),
}

type Result<T> = std::result::Result<T, TokenizerError>;

pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input: input.as_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
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
    }

    fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        if self.is_letter() {
            return self.get_ident_or_kw();
        }

        let token = match self.ch {
            b'=' => self.if_peek(b'=', Token::Equal, Token::Assign),
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::ForwardSlash,
            b'!' => self.if_peek(b'=', Token::NotEqual, Token::Bang),

            b'<' => Token::LT,
            b'>' => Token::GT,

            b',' => Token::Comma,
            b';' => Token::Semicolon,

            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,

            b'0'..=b'9' => return self.get_int(),
            b'"' => return self.get_str(),

            0 => Token::Eof,
            _ => return Err(TokenizerError::UnexpectedInput(self.ch as char)),
        };
        self.read_char();
        Ok(token)
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn is_letter(&self) -> bool {
        matches!(self.ch, b'a'..=b'z' | b'A'..=b'Z' | b'_' )
    }

    fn get_ident_or_kw(&mut self) -> Result<Token> {
        let ident = self.read_ident()?;
        Ok(match ident {
            "let" => Token::Let,
            "fn" => Token::Function,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "return" => Token::Return,
            _ => Token::Identifier(ident),
        })
    }

    fn read_ident(&mut self) -> Result<&'a str> {
        let start = self.position;
        while self.is_letter() {
            self.read_char()
        }
        from_utf8(&self.input[start..self.position]).map_err(TokenizerError::NonUTF8Input)
    }

    fn get_int(&mut self) -> Result<Token> {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let val =
            from_utf8(&self.input[start..self.position]).map_err(TokenizerError::NonUTF8Input)?;
        Ok(Token::Int(val))
    }

    fn get_str(&mut self) -> Result<Token> {
        self.read_char();
        let start = self.position;
        while self.ch != b'"' {
            self.read_char();
        }
        let val =
            from_utf8(&self.input[start..self.position]).map_err(TokenizerError::NonUTF8Input)?;
        Ok(Token::Str(val))
    }

    fn if_peek<'t>(
        &mut self,
        peek: u8,
        true_token: Token<'t>,
        false_token: Token<'t>,
    ) -> Token<'t> {
        if self.peek() == peek {
            self.read_char();
            true_token
        } else {
            false_token
        }
    }

    fn peek(&self) -> u8 {
        self.input[self.read_position]
    }
}

#[cfg(test)]
mod test {
    use super::{tokens::Token, *};

    #[test]
    fn next_token() -> Result<()> {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        for token in tokens {
            assert_eq!(token, lexer.next_token()?);
        }
        Ok(())
    }

    #[test]
    fn get_next_complete() -> Result<()> {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "#;

        let mut lex = Lexer::new(input);

        let tokens = vec![
            Token::Let,
            Token::Identifier("five"),
            Token::Assign,
            Token::Int("5"),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten"),
            Token::Assign,
            Token::Int("10"),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add"),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier("x"),
            Token::Comma,
            Token::Identifier("y"),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("x"),
            Token::Plus,
            Token::Identifier("y"),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result"),
            Token::Assign,
            Token::Identifier("add"),
            Token::LParen,
            Token::Identifier("five"),
            Token::Comma,
            Token::Identifier("ten"),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::ForwardSlash,
            Token::Asterisk,
            Token::Int("5"),
            Token::Semicolon,
            Token::Int("5"),
            Token::LT,
            Token::Int("10"),
            Token::GT,
            Token::Int("5"),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5"),
            Token::LT,
            Token::Int("10"),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int("10"),
            Token::Equal,
            Token::Int("10"),
            Token::Semicolon,
            Token::Int("10"),
            Token::NotEqual,
            Token::Int("9"),
            Token::Semicolon,
            Token::Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token()?;
            assert_eq!(token, next_token);
        }

        Ok(())
    }
}
