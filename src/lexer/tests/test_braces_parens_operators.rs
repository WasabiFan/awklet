use crate::lexer::{tokenize, SpannedToken, Token, TokenizeError};

use super::helpers::TokenSpanCursor;

#[test]
fn braces_parens() -> Result<(), TokenizeError> {
    let tokens = tokenize("{{ ){( (}")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(Token::OpenBrace, spans.advance_spanned(1)),
            SpannedToken(Token::OpenBrace, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::CloseParen, spans.advance_spanned(1)),
            SpannedToken(Token::OpenBrace, spans.advance_spanned(1)),
            SpannedToken(Token::OpenParen, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::OpenParen, spans.advance_spanned(1)),
            SpannedToken(Token::CloseBrace, spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn math_operators() -> Result<(), TokenizeError> {
    let tokens = tokenize("1 +44.2 -4. / 18*99%2")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::NumericLiteral(1.),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::Plus, spans.advance_spanned(1)),
            SpannedToken(
                Token::NumericLiteral(44.2),
                spans.advance_spanned_and_skip(4, 1)
            ),
            SpannedToken(Token::Minus, spans.advance_spanned(1)),
            SpannedToken(
                Token::NumericLiteral(4.),
                spans.advance_spanned_and_skip(2, 1)
            ),
            SpannedToken(Token::Slash, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::NumericLiteral(18.), spans.advance_spanned(2)),
            SpannedToken(Token::Star, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(99.), spans.advance_spanned(2)),
            SpannedToken(Token::Mod, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(2.), spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn increment_decrement() -> Result<(), TokenizeError> {
    let tokens = tokenize("a++ + +b-- -2")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::Identifier(String::from("a")),
                spans.advance_spanned(1)
            ),
            SpannedToken(Token::Increment, spans.advance_spanned_and_skip(2, 1)),
            SpannedToken(Token::Plus, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::Plus, spans.advance_spanned(1)),
            SpannedToken(
                Token::Identifier(String::from("b")),
                spans.advance_spanned(1)
            ),
            SpannedToken(Token::Decrement, spans.advance_spanned_and_skip(2, 1)),
            SpannedToken(Token::Minus, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(2.), spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn plus_equals() -> Result<(), TokenizeError> {
    let tokens = tokenize("a+=-9")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::Identifier(String::from("a")),
                spans.advance_spanned(1)
            ),
            SpannedToken(Token::PlusEquals, spans.advance_spanned(2)),
            SpannedToken(Token::Minus, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(9.), spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn minus_equals() -> Result<(), TokenizeError> {
    let tokens = tokenize("a -=-12")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::Identifier(String::from("a")),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::MinusEquals, spans.advance_spanned(2)),
            SpannedToken(Token::Minus, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(12.), spans.advance_spanned(2)),
        ]
    );

    Ok(())
}

#[test]
fn assignment() -> Result<(), TokenizeError> {
    let tokens = tokenize("a = -88")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::Identifier(String::from("a")),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::AssignEquals, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::Minus, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(88.), spans.advance_spanned(2)),
        ]
    );

    Ok(())
}

#[test]
fn field_reference_comma() -> Result<(), TokenizeError> {
    let tokens = tokenize("print $2, $ 5")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::Identifier(String::from("print")),
                spans.advance_spanned_and_skip(5, 1)
            ),
            SpannedToken(Token::FieldReference, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(2.), spans.advance_spanned(1)),
            SpannedToken(Token::Comma, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::FieldReference, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::NumericLiteral(5.), spans.advance_spanned(1)),
        ]
    );

    Ok(())
}
