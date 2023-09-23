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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::ForwardSlash => write!(f, "/"),
            Token::Bang => write!(f, "!"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::Int(val) => write!(f, "{}", val),
            Token::Str(val) => write!(f, "\"{}\"", val),
            Token::Let => write!(f, "let"),
            Token::Function => write!(f, "fn"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Return => write!(f, "return"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}
