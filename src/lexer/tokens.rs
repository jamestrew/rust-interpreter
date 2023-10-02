use std::ops::Range;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub token_kind: TokenKind,
    pub line: usize,
    pub span: Range<usize>,
}

impl Token {
    pub fn new(token_kind: TokenKind, line: usize, span: Range<usize>) -> Token {
        let span_start = span.start;
        let mut span_end = span.end;
        if token_kind == TokenKind::Eof {
            span_end = span_start;
        }
        Self {
            token_kind,
            line,
            span: span_start..span_end,
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.token_kind
    }

    pub fn start(&self) -> usize {
        self.span.start
    }

    pub fn end(&self) -> usize {
        self.span.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
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
    Nil,

    Eof,
}

impl AsRef<TokenKind> for TokenKind {
    fn as_ref(&self) -> &TokenKind {
        self
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Assign => write!(f, "="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::ForwardSlash => write!(f, "/"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::LT => write!(f, "<"),
            TokenKind::GT => write!(f, ">"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Identifier(ident) => write!(f, "{}", ident),
            TokenKind::Int(val) => write!(f, "{}", val),
            TokenKind::Str(val) => write!(f, "\"{}\"", val),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}
