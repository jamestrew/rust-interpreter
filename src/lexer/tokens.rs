#[derive(Debug, PartialEq)]
pub enum Token<'a> {
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

    Identifier(&'a str),
    Int(&'a str),
    Str(&'a str),

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
