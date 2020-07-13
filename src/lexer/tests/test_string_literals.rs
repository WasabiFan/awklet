use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_string_single_literal() -> Result<(), TokenizeError> {
    let tokens = tokenize("\"this is a string\"")?;

    assert_eq!(
        tokens,
        vec![Token::StringLiteral(String::from("this is a string"))]
    );

    Ok(())
}

#[test]
fn test_string_nested_quotes_1() -> Result<(), TokenizeError> {
    let tokens = tokenize("\"this \\\" \\ is \\\"a\\\" string\"")?;

    assert_eq!(
        tokens,
        vec![Token::StringLiteral(String::from(
            "this \" \\ is \"a\" string"
        ))]
    );

    Ok(())
}

#[test]
fn test_string_nested_quotes_2() -> Result<(), TokenizeError> {
    let tokens = tokenize("\"\\\"\\\"\"")?;

    assert_eq!(tokens, vec![Token::StringLiteral(String::from("\"\""))]);

    Ok(())
}

#[test]
fn test_string_multiple_with_spacing() -> Result<(), TokenizeError> {
    let tokens = tokenize(" \"foo \\\" bar\" \"abc \" ")?;

    assert_eq!(
        tokens,
        vec![
            Token::StringLiteral(String::from("foo \" bar")),
            Token::StringLiteral(String::from("abc ")),
        ]
    );

    Ok(())
}

#[test]
fn test_string_syntax_error_unnmatched_quote() {
    let result = tokenize(" \"foo \\\"");

    assert!(
        matches!(result, Err(TokenizeError::SyntaxError)),
        "result = {:?}",
        result
    );
}
