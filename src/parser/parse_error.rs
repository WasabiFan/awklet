use crate::lexer::TokenizeError;

#[derive(Debug)]
pub enum ParseError {
    TokenizeError(TokenizeError),
    SyntaxError,
}
