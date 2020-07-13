use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_braces_parens() -> Result<(), TokenizeError> {
    let tokens = tokenize("{{ ){( (}")?;

    assert_eq!(
        tokens,
        vec![
            Token::OpenBrace,
            Token::OpenBrace,
            Token::CloseParen,
            Token::OpenBrace,
            Token::OpenParen,
            Token::OpenParen,
            Token::CloseBrace
        ]
    );

    Ok(())
}
