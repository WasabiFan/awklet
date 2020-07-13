use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_syntax_error_invalid_token() {
    let result = tokenize("!10");

    assert!(
        matches!(result, Err(TokenizeError::SyntaxError)),
        "result = {:?}",
        result
    );
}
