use crate::lexer::{Token, tokenize, TokenizeError};

#[test]
fn test_syntax_error_invalid_token() {
    let result = tokenize("!10");

    assert!(
        matches!(result, Err(TokenizeError::SyntaxError)),
        "result = {:?}",
        result
    );
}