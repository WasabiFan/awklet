use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_basic_identifier() -> Result<(), TokenizeError> {
    let tokens = tokenize("somewords")?;

    assert_eq!(tokens, vec![Token::Identifier(String::from("somewords"))]);

    Ok(())
}

#[test]
fn test_identifier_underscores() -> Result<(), TokenizeError> {
    let tokens = tokenize("_some_words__")?;

    assert_eq!(
        tokens,
        vec![Token::Identifier(String::from("_some_words__"))]
    );

    Ok(())
}

#[test]
fn test_identifier_numbers() -> Result<(), TokenizeError> {
    let tokens = tokenize("foo9bar")?;

    assert_eq!(tokens, vec![Token::Identifier(String::from("foo9bar"))]);

    Ok(())
}

#[test]
fn test_identifier_no_leading_digit() {
    let result = tokenize("9foobar");

    assert_matches!(result, Err(TokenizeError::SyntaxError));
}

#[test]
fn test_identifier_multiple() -> Result<(), TokenizeError> {
    let tokens = tokenize("this3num_ some_symbol foo__9")?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("this3num_")),
            Token::Identifier(String::from("some_symbol")),
            Token::Identifier(String::from("foo__9")),
        ]
    );

    Ok(())
}
