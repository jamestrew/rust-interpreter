#![allow(dead_code)]
mod tokens;

use thiserror::Error;
use tokens::Token;

#[derive(Error, Debug)]
enum TokenizerError {
    #[error("Unexpected input {0}. Failed to tokenize.")]
    UnexpectedInput(char),
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
            return Ok(self.get_ident_or_kw());
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

            b'0'..=b'9' => return Ok(self.get_int()),

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

    fn get_ident_or_kw(&mut self) -> Token {
        let ident = self.read_ident();
        match ident.as_str() {
            "let" => Token::Let,
            "fn" => Token::Function,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "return" => Token::Return,
            _ => Token::Identifier(ident),
        }
    }

    fn read_ident(&mut self) -> String {
        let start = self.position;
        while self.is_letter() {
            self.read_char()
        }
        String::from_utf8_lossy(&self.input[start..self.position]).to_string()
    }

    fn get_int(&mut self) -> Token {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let val = String::from_utf8_lossy(&self.input[start..self.position]).to_string();
        Token::Int(val)
    }

    fn if_peek(&mut self, peek: u8, true_token: Token, false_token: Token) -> Token {
        if self.peek() == peek {
            self.read_char();
            true_token
        } else {
            false_token
        }
    }

    fn get_assign_or_eq(&mut self) -> Token {
        if self.peek() == b'=' {
            self.read_char();
            Token::Equal
        } else {
            Token::Assign
        }
    }

    fn get_bang_or_not_eq(&mut self) -> Token {
        if self.peek() == b'=' {
            self.read_char();
            Token::NotEqual
        } else {
            Token::Bang
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
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::ForwardSlash,
            Token::Asterisk,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::LT,
            Token::Int(String::from("10")),
            Token::GT,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(String::from("5")),
            Token::LT,
            Token::Int(String::from("10")),
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
            Token::Int(String::from("10")),
            Token::Equal,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::NotEqual,
            Token::Int(String::from("9")),
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
