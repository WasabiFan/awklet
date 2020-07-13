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

#[test]
fn test_multiple_tokens() -> Result<(), TokenizeError> {
    let tokens = tokenize("5 10")?;

    assert_eq!(
        tokens,
        vec![Token::NumericLiteral(5.), Token::NumericLiteral(10.)]
    );

    Ok(())
}

#[test]
fn test_multiple_tokens_leading_space() -> Result<(), TokenizeError> {
    let tokens = tokenize(" 5 10")?;

    assert_eq!(
        tokens,
        vec![Token::NumericLiteral(5.), Token::NumericLiteral(10.)]
    );

    Ok(())
}

#[test]
fn test_multiple_tokens_trailing_space() -> Result<(), TokenizeError> {
    let tokens = tokenize("5 10 ")?;

    assert_eq!(
        tokens,
        vec![Token::NumericLiteral(5.), Token::NumericLiteral(10.)]
    );

    Ok(())
}
