use crate::lexer::{tokenize, SpannedToken, Token, TokenizeError};

use super::helpers::TokenSpanCursor;

#[test]
fn numeric_single_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("5")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::NumericLiteral(5.),
            spans.advance_spanned(1)
        )]
    );

    Ok(())
}

#[test]
fn numeric_multi_digit_integer() -> Result<(), TokenizeError> {
    let tokens = tokenize("84")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::NumericLiteral(84.),
            spans.advance_spanned(2)
        )]
    );

    Ok(())
}

#[test]
fn numeric_decimal() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::NumericLiteral(5.29),
            spans.advance_spanned(4)
        )]
    );

    Ok(())
}

#[test]
fn numeric_positive_exponential() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29e2")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::NumericLiteral(5.29 * 10f64.powi(2)),
            spans.advance_spanned(6)
        )]
    );

    Ok(())
}

#[test]
fn numeric_negative_exponential() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.29e-2")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::NumericLiteral(5.29 * 10f64.powi(-2)),
            spans.advance_spanned(7)
        )]
    );

    Ok(())
}

#[test]
fn numeric_decimal_ending_with_dot() -> Result<(), TokenizeError> {
    let tokens = tokenize("5.")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::NumericLiteral(5.),
            spans.advance_spanned(2)
        )]
    );

    Ok(())
}

#[test]
fn numeric_multiple() -> Result<(), TokenizeError> {
    let tokens = tokenize("5 10")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::NumericLiteral(5.),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::NumericLiteral(10.), spans.advance_spanned(2))
        ]
    );

    Ok(())
}

#[test]
fn numeric_multiple_leading_space() -> Result<(), TokenizeError> {
    let tokens = tokenize(" 5 10")?;
    let mut spans = TokenSpanCursor::new();
    spans.advance(1);

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::NumericLiteral(5.),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::NumericLiteral(10.), spans.advance_spanned(2))
        ]
    );

    Ok(())
}

#[test]
fn numeric_multiple_trailing_space() -> Result<(), TokenizeError> {
    let tokens = tokenize("5 10 ")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::NumericLiteral(5.),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(
                Token::NumericLiteral(10.),
                spans.advance_spanned_and_skip(2, 1)
            )
        ]
    );

    Ok(())
}
