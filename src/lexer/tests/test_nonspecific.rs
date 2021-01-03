use crate::lexer::{tokenize, SpannedToken, Token, TokenizeError};
use matches::assert_matches;

use super::helpers::TokenSpanCursor;

#[test]
fn syntax_error_invalid_token() {
    let result = tokenize("@10");

    assert_matches!(result, Err(TokenizeError::SyntaxError));
}

#[test]
fn basic_awk_program() -> Result<(), TokenizeError> {
    let source = "BEGIN {
        print \"Hello, world!\"
    }";
    let tokens = tokenize(source)?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(Token::BeginKeyword, spans.advance_spanned_and_skip(5, 1)),
            SpannedToken(Token::OpenBrace, spans.advance_spanned(1)),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(9)),
            SpannedToken(
                Token::Identifier(String::from("print")),
                spans.advance_spanned_and_skip(5, 1)
            ),
            SpannedToken(
                Token::StringLiteral(String::from("Hello, world!")),
                spans.advance_spanned(15)
            ),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(5)),
            SpannedToken(Token::CloseBrace, spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn statement_separator() -> Result<(), TokenizeError> {
    let source = "BEGIN {
        print \"Hello, world!\"
        a = 5;
        ; ;
        getline foo ; print
    }";
    let tokens = tokenize(source)?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(Token::BeginKeyword, spans.advance_spanned_and_skip(5, 1)),
            SpannedToken(Token::OpenBrace, spans.advance_spanned(1)),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(9)),
            SpannedToken(
                Token::Identifier(String::from("print")),
                spans.advance_spanned_and_skip(5, 1)
            ),
            SpannedToken(
                Token::StringLiteral(String::from("Hello, world!")),
                spans.advance_spanned(15)
            ),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(9)),
            SpannedToken(
                Token::Identifier(String::from("a")),
                spans.advance_spanned_and_skip(1, 1)
            ),
            SpannedToken(Token::AssignEquals, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::NumericLiteral(5.), spans.advance_spanned(1)),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(22)), // adjacent StatementSeparators are coalesced together
            SpannedToken(
                Token::Identifier(String::from("getline")),
                spans.advance_spanned_and_skip(7, 1)
            ),
            SpannedToken(
                Token::Identifier(String::from("foo")),
                spans.advance_spanned_and_skip(3, 1)
            ),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(2)),
            SpannedToken(
                Token::Identifier(String::from("print")),
                spans.advance_spanned(5)
            ),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(5)),
            SpannedToken(Token::CloseBrace, spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn comparisons() -> Result<(), TokenizeError> {
    let source = "< <= == != > >=";
    let tokens = tokenize(source)?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(Token::LeftCaret, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::LessEqual, spans.advance_spanned_and_skip(2, 1)),
            SpannedToken(Token::CompareEquals, spans.advance_spanned_and_skip(2, 1)),
            SpannedToken(Token::BangEqual, spans.advance_spanned_and_skip(2, 1)),
            SpannedToken(Token::RightCaret, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::GreaterEqual, spans.advance_spanned(2))
        ]
    );

    Ok(())
}

#[test]
fn short_final_token() -> Result<(), TokenizeError> {
    let source = "< >";
    let tokens = tokenize(source)?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(Token::LeftCaret, spans.advance_spanned_and_skip(1, 1)),
            SpannedToken(Token::RightCaret, spans.advance_spanned(1)),
        ]
    );

    Ok(())
}

#[test]
fn leading_newlines() -> Result<(), TokenizeError> {
    let source = "
    BEGIN {
    }";
    let tokens = tokenize(source)?;
    let mut spans = TokenSpanCursor::new();

    assert_eq!(
        tokens[..],
        [
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(5)),
            SpannedToken(Token::BeginKeyword, spans.advance_spanned_and_skip(5, 1)),
            SpannedToken(Token::OpenBrace, spans.advance_spanned(1)),
            SpannedToken(Token::StatementSeparator, spans.advance_spanned(5)),
            SpannedToken(Token::CloseBrace, spans.advance_spanned(1)),
        ]
    );

    Ok(())
}
