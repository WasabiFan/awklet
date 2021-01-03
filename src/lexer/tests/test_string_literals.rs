use crate::lexer::{token::SpannedToken, tokenize, Token, TokenizeError};
use matches::assert_matches;

use super::helpers::TokenSpanCursor;

#[test]
fn string_single_literal() -> Result<(), TokenizeError> {
    let tokens = tokenize("\"this is a string\"")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::StringLiteral(String::from("this is a string")),
            spans.advance_spanned(18)
        )]
    );

    Ok(())
}

#[test]
fn string_nested_quotes_1() -> Result<(), TokenizeError> {
    let tokens = tokenize("\"this \\\" \\ is \\\"a\\\" string\"")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::StringLiteral(String::from("this \" \\ is \"a\" string")),
            spans.advance_spanned(27)
        )]
    );

    Ok(())
}

#[test]
fn string_nested_quotes_2() -> Result<(), TokenizeError> {
    let tokens = tokenize("\"\\\"\\\"\"")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::StringLiteral(String::from("\"\"")),
            spans.advance_spanned(6)
        )]
    );

    Ok(())
}

#[test]
fn string_multiple_with_spacing() -> Result<(), TokenizeError> {
    let tokens = tokenize(" \"foo \\\" bar\" \"abc \" ")?;
    let mut spans = TokenSpanCursor::new();
    spans.advance(1);

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::StringLiteral(String::from("foo \" bar")),
                spans.advance_spanned_and_skip(12, 1)
            ),
            SpannedToken(
                Token::StringLiteral(String::from("abc ")),
                spans.advance_spanned(6)
            ),
        ]
    );

    Ok(())
}

#[test]
fn string_syntax_error_unnmatched_quote() {
    let result = tokenize(" \"foo \\\"");

    assert_matches!(result, Err(TokenizeError::SyntaxError));
}
