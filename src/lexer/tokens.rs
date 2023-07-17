#[derive(Debug, PartialEq)]
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

    LParen,
    RParen,
    LBrace,
    RBrace,

    Identifier(String),
    Int(String),

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
