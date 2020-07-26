use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn regex_single_literal() -> Result<(), TokenizeError> {
    let tokens = tokenize("/[a-z]+/")?;

    assert_eq!(tokens, vec![Token::RegexLiteral(String::from("[a-z]+"))]);

    Ok(())
}

#[test]
fn regex_escaping() -> Result<(), TokenizeError> {
    let tokens = tokenize("/\\d+\\/\\d+/")?;

    assert_eq!(tokens, vec![Token::RegexLiteral(String::from("\\d+/\\d+"))]);

    Ok(())
}

#[test]
fn regex_newline_break() -> Result<(), TokenizeError> {
    let tokens = tokenize("2 / 3\n 5/6")?;

    assert_eq!(
        tokens,
        vec![
            Token::NumericLiteral(2.),
            Token::Slash,
            Token::NumericLiteral(3.),
            Token::StatementSeparator,
            Token::NumericLiteral(5.),
            Token::Slash,
            Token::NumericLiteral(6.),
        ]
    );

    Ok(())
}
