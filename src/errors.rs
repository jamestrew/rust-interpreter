use thiserror::Error;

use crate::types::Spanned;

pub type Result<T> = std::result::Result<T, Spanned<MonkeyError>>;

#[derive(Error, Debug, Clone)]
pub enum MonkeyError {
    #[error("TokenizerError: {0}")]
    TokenizerError(TokenizerError),

    #[error("ParserError: {0}")]
    ParserError(ParserError),
}

#[derive(Error, Debug, Clone)]
pub enum TokenizerError {
    #[error("Unexpected input {0}. Failed to tokenize.")]
    UnexpectedInput(char),

    #[error("Invalid non UTF-8 string found.")]
    NonUTF8Input,
}

#[derive(Error, Debug, Clone)]
pub enum ParserError {
    #[error("Unexpected token '{0}'.")]
    UnexpectedToken(String),

    #[error("Expected token '{0}' not found.")]
    ExpectedTokenNotFound(String),

    #[error("Unexpected EOF.")]
    UnexpectedEOF,
}

macro_rules! impl_from_for_monkeyerror {
    ($($err:tt),+) => {$(
        impl From<$err> for MonkeyError {
            fn from(value: $err) -> Self {
                MonkeyError::$err(value)
            }
        }
    )+}
}

impl_from_for_monkeyerror!(TokenizerError, ParserError);
