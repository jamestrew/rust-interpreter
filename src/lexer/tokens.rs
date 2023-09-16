use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Assign,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Bang,

    Equal,
    NotEqual,
    LT,
    GT,

    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Identifier(Rc<str>),
    Int(Rc<str>),
    Str(Rc<str>),

    // keywords
    Let,
    Function,
    If,
    Else,
    True,
    False,
    Return,

    Eof,
}

impl AsRef<Token> for Token {
    fn as_ref(&self) -> &Token {
        self
    }
}
