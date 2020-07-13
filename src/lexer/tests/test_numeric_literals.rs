use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_numeric_single_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("5")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.)]);

    Ok(())
}

#[test]
fn test_numeric_multi_digit_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("84")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(84.)]);

    Ok(())
}

#[test]
fn test_numeric_negative_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("-19")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(-19.)]);

    Ok(())
}

#[test]
fn test_numeric_decimal() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.29)]);

    Ok(())
}

#[test]
fn test_numeric_positive_exponential() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29e2")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.29 * 10f64.powi(2))]);

    Ok(())
}

#[test]
fn test_numeric_negative_exponential() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29e-2")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.29 * 10f64.powi(-2))]);

    Ok(())
}

#[test]
fn test_numeric_decimal_ending_with_dot() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.)]);

    Ok(())
}

#[test]
fn test_numeric_multiple() -> Result<(), TokenizeError> {
    let tokens = tokenize("5 10")?;

    assert_eq!(
        tokens,
        vec![Token::NumericLiteral(5.), Token::NumericLiteral(10.)]
    );

    Ok(())
}

#[test]
fn test_numeric_multiple_leading_space() -> Result<(), TokenizeError> {
    let tokens = tokenize(" 5 10")?;

    assert_eq!(
        tokens,
        vec![Token::NumericLiteral(5.), Token::NumericLiteral(10.)]
    );

    Ok(())
}

#[test]
fn test_numeric_multiple_trailing_space() -> Result<(), TokenizeError> {
    let tokens = tokenize("5 10 ")?;

    assert_eq!(
        tokens,
        vec![Token::NumericLiteral(5.), Token::NumericLiteral(10.)]
    );

    Ok(())
}
