mod tokens;

use std::str::{from_utf8, Utf8Error};

use thiserror::Error;
pub use tokens::{Token, TokenKind};

#[derive(Error, Debug)]
enum TokenizerError {
    #[error("Unexpected input {0}. Failed to tokenize.")]
    UnexpectedInput(char),

    #[error("Invalid non UTF-8 string found: {0}")]
    NonUTF8Input(Utf8Error),
}

type Result<T> = std::result::Result<T, TokenizerError>;

#[derive(Debug, Default)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
    line: usize,
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

            0 => TokenKind::Eof,
            _ => return Err(TokenizerError::UnexpectedInput(self.ch as char)),
        };

        self.read_char();
        let span_end = self.col;
        Ok(Token::new(token_type, self.line, span_start..span_end))
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            let last_char = self.ch;
            self.read_char();
            if last_char == b'\n' {
                self.line += 1;
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

        Ok(Token::new(token_type, self.line, span_start..span_end))
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
        let span_start = self.col;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let span_end = self.col;
        let val =
            from_utf8(&self.input[start..self.position]).map_err(TokenizerError::NonUTF8Input)?;
        Ok(Token::new(
            TokenKind::Int(val.into()),
            self.line,
            span_start..span_end,
        ))
    }

    fn get_str(&mut self) -> Result<Token> {
        let span_start = self.col;
        self.read_char();
        let start = self.position;
        while self.ch != b'"' {
            self.read_char();
        }
        let val =
            from_utf8(&self.input[start..self.position]).map_err(TokenizerError::NonUTF8Input)?;
        let token_type = TokenKind::Str(val.into());
        self.read_char();
        let span_end = self.col;
        Ok(Token::new(token_type, self.line, span_start..span_end))
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
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token().ok()?;

        if *token.kind() == TokenKind::Eof && self.position - 1 > self.input.len() {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[allow(unused)]
    #[derive(Debug)]
    struct T<'a> {
        token: Token,
        slice: &'a str,
    }

    fn lex(input: &str) -> Vec<Token> {
        let lexer = Lexer::new(input);

        lexer.collect()
    }

    fn debug_print(input: &str, tokens: Vec<Token>) -> Vec<T> {
        let lines = input.lines().collect::<Vec<_>>();

        let mut ret = Vec::new();
        for token in tokens {
            let line = lines[token.line];
            let slice = &line[token.span.clone()];
            ret.push(T { token, slice });
        }
        ret
    }

    macro_rules! snapshot {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let tokens = lex($input);
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_debug_snapshot!(debug_print($input, tokens));
                })
            }
        };
    }

    snapshot!(basic, "=");
    snapshot!(symbols, "=+(){},;");
    snapshot!(assignments1, "let five = 5;");
    snapshot!(assignments2, "let ten = 10;");
    snapshot!(
        assignments3,
        r"let add = fn(x, y) {
        x + y;
    };"
    );
    snapshot!(assignments4, "let result = add(five, ten);");

    snapshot!(operators, "!-/*5;");
    snapshot!(equality1, "5 < 10 > 5;");
    snapshot!(equality2, "10 == 10;");
    snapshot!(equality3, "10 != 9;");
    snapshot!(
        conditional,
        r"if (5 < 10) {
        return true;
    } else {
        return false;
    }"
    );

    snapshot!(string_literal, r#""hello world";"#);
    snapshot!(arrays, "[1, 2]");
    snapshot!(dictionary, r#"{ "foo": "bar" };"#);
}
