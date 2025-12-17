use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum JsonError {
    #[error("Unexpected end of input")]
    UnexpectedEof,

    #[error("Expected {expected} but found '{found}' at position {position}")]
    UnexpectedToken {
        expected: String,
        found: char,
        position: usize,
    },

    #[error("Invalid number '{value}' at position {position}")]
    InvalidNumber { value: String, position: usize },
}
