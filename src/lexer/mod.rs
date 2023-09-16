mod tokens;

use std::str::{from_utf8, Utf8Error};

use thiserror::Error;
pub use tokens::Token;

#[derive(Error, Debug)]
enum TokenizerError {
    #[error("Unexpected input {0}. Failed to tokenize.")]
    UnexpectedInput(char),

    #[error("Invalid non UTF-8 string found: {0}")]
    NonUTF8Input(Utf8Error),
}

type Result<T> = std::result::Result<T, TokenizerError>;

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.as_bytes().to_vec(),
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
            b':' => Token::Colon,

            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,

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
            _ => Token::Identifier(ident.into()),
        })
    }

    fn read_ident(&mut self) -> Result<&str> {
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
        Ok(Token::Int(val.into()))
    }

    fn get_str(&mut self) -> Result<Token> {
        self.read_char();
        let start = self.position;
        while self.ch != b'"' {
            self.read_char();
        }
        let val =
            from_utf8(&self.input[start..self.position]).map_err(TokenizerError::NonUTF8Input)?;
        let ret = Token::Str(val.into());
        self.read_char();
        Ok(ret)
    }

    fn if_peek(&mut self, peek: u8, true_token: Token, false_token: Token) -> Token {
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

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token().ok()?;
        if token == Token::Eof && self.position - 1 > self.input.len() {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod test {
    use super::tokens::Token;
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        let lexer = Lexer::new(input);

        lexer.collect()
    }

    macro_rules! snapshot {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let tokens = lex($input);
                insta::assert_debug_snapshot!(($input, tokens));
            }
        };
    }

    snapshot!(symbols, "=+(){},;");
    snapshot!(assignments1, "let five = 5;");
    snapshot!(assignments2, "let ten = 10;");
    snapshot!(
        assignments3,
        r#"let add = fn(x, y) {
    x + y;
};"#
    );
    snapshot!(assignments4, "let result = add(five, ten);");

    snapshot!(operators, "!-/*5;");
    snapshot!(equality1, "5 < 10 > 5;");
    snapshot!(equality2, "10 == 10;");
    snapshot!(equality3, "10 != 9;");
    snapshot!(
        conditional,
        r#"if (5 < 10) {
    return true;
} else {
    return false;
}"#
    );

    snapshot!(string_literal, r#""hello world";"#);
    snapshot!(arrays, "[1, 2]");
    snapshot!(dictionary, r#"{ "foo": "bar" };"#);
}
