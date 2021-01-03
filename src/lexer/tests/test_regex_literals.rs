use crate::lexer::{tokenize, SpannedToken, Token, TokenizeError};

use super::helpers::TokenSpanCursor;

#[test]
fn regex_single_literal() -> Result<(), TokenizeError> {
    let tokens = tokenize("/[a-z]+/")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::RegexLiteral(String::from("[a-z]+")),
            spans.advance_spanned(8)
        )]
    );

    Ok(())
}

#[test]
fn regex_escaping() -> Result<(), TokenizeError> {
    let tokens = tokenize("/\\d+\\/\\d+/")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [SpannedToken(
            Token::RegexLiteral(String::from("\\d+/\\d+")),
            spans.advance_spanned(10)
        )]
    );

    Ok(())
}

#[test]
fn regex_newline_break() -> Result<(), TokenizeError> {
    let tokens = tokenize("2 / 3\n 5/6")?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(
                Token::NumericLiteral(2.),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::Slash, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::NumericLiteral(3.), spans.advance_spanned(1)),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(2)),
            SpannedToken(Token::NumericLiteral(5.), spans.advance_spanned(1)),
            SpannedToken(Token::Slash, spans.advance_spanned(1)),
            SpannedToken(Token::NumericLiteral(6.), spans.advance_spanned(1)),
        ]
    );

    Ok(())
}
