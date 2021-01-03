use crate::lexer::{tokenize, SpannedToken, Token, TokenizeError};
use matches::assert_matches;

use super::helpers::TokenSpanCursor;

#[test]
fn basic_identifier() -> Result<(), TokenizeError> {
    let tokens = tokenize("somewords")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::Identifier(String::from("somewords")),
            spans.advance_spanned(9)
        )]
    );

    Ok(())
}

#[test]
fn identifier_underscores() -> Result<(), TokenizeError> {
    let tokens = tokenize("_some_words__")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::Identifier(String::from("_some_words__")),
            spans.advance_spanned(13)
        )]
    );

    Ok(())
}

#[test]
fn identifier_numbers() -> Result<(), TokenizeError> {
    let tokens = tokenize("foo9bar")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::Identifier(String::from("foo9bar")),
            spans.advance_spanned(7)
        )]
    );
    Ok(())
}

#[test]
fn identifier_no_leading_digit() {
    let result = tokenize("9foobar");

    assert_matches!(result, Err(TokenizeError::SyntaxError));
}

#[test]
fn identifier_multiple() -> Result<(), TokenizeError> {
    let tokens = tokenize("this3num_ some_symbol foo__9")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::Identifier(String::from("this3num_")),
                spans.advance_spanned_and_skip(9, 1)
            ),
            SpannedToken(
                Token::Identifier(String::from("some_symbol")),
                spans.advance_spanned_and_skip(11, 1)
            ),
            SpannedToken(
                Token::Identifier(String::from("foo__9")),
                spans.advance_spanned(6)
            ),
        ]
    );

    Ok(())
}
