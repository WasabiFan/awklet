use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_syntax_error_invalid_token() {
    let result = tokenize("@10");

    assert!(
        matches!(result, Err(TokenizeError::SyntaxError)),
        "result = {:?}",
        result
    );
}

#[test]
fn test_basic_awk_program() -> Result<(), TokenizeError> {
    let source = "BEGIN {
        print \"Hello, world!\"
    }";
    let tokens = tokenize(source)?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("BEGIN")),
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::StringLiteral(String::from("Hello, world!")),
            Token::CloseBrace
        ]
    );

    Ok(())
}
