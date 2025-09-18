use thiserror::Error;

#[derive(Debug, Error)]
pub enum MonkeyError {
    #[error("Syntax error: {message}")]
    SyntaxError { message: String },

    #[error("Type error: expected {expected}, got {actual}")]
    TypeError { expected: String, actual: String },

    #[error("Runtime error: {message}")]
    RuntimeError { message: String },

    #[error("Wrong number of arguments: expected {expected}, got {actual}")]
    WrongArgumentCount { expected: usize, actual: usize },

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}
