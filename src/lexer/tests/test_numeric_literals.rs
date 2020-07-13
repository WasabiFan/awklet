use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_single_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("5")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.)]);

    Ok(())
}

#[test]
fn test_multi_digit_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("84")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(84.)]);

    Ok(())
}

#[test]
fn test_negative_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("-19")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(-19.)]);

    Ok(())
}

#[test]
fn test_decimal() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.29)]);

    Ok(())
}

#[test]
fn test_positive_exponential() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29e2")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.29 * 10f64.powi(2))]);

    Ok(())
}

#[test]
fn test_negative_exponential() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29e-2")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.29 * 10f64.powi(-2))]);

    Ok(())
}

#[test]
fn test_decimal_ending_with_dot() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.")?;

    assert_eq!(tokens, vec![Token::NumericLiteral(5.)]);

    Ok(())
}
