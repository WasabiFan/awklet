use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn test_syntax_error_invalid_token() {
    let result = tokenize("@10");

    assert_matches!(result, Err(TokenizeError::SyntaxError));
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
            Token::BeginKeyword,
            Token::OpenBrace,
            Token::StatementSeparator,
            Token::Identifier(String::from("print")),
            Token::StringLiteral(String::from("Hello, world!")),
            Token::StatementSeparator,
            Token::CloseBrace
        ]
    );

    Ok(())
}

#[test]
fn test_statement_separator() -> Result<(), TokenizeError> {
    let source = "BEGIN {
        print \"Hello, world!\"
        a = 5;
        ; ;
        getline foo ; print
    }";
    let tokens = tokenize(source)?;

    assert_eq!(
        tokens,
        vec![
            Token::BeginKeyword,
            Token::OpenBrace,
            Token::StatementSeparator,
            Token::Identifier(String::from("print")),
            Token::StringLiteral(String::from("Hello, world!")),
            Token::StatementSeparator,
            Token::Identifier(String::from("a")),
            Token::AssignEquals,
            Token::NumericLiteral(5.),
            Token::StatementSeparator, // adjacent StatementSeparators are coalesced together
            Token::Identifier(String::from("getline")),
            Token::Identifier(String::from("foo")),
            Token::StatementSeparator,
            Token::Identifier(String::from("print")),
            Token::StatementSeparator,
            Token::CloseBrace
        ]
    );

    Ok(())
}

#[test]
fn test_comparisons() -> Result<(), TokenizeError> {
    let source = "< <= == != > >=";
    let tokens = tokenize(source)?;

    assert_eq!(
        tokens,
        vec![
            Token::LeftCaret,
            Token::LessEqual,
            Token::CompareEquals,
            Token::BangEqual,
            Token::RightCaret,
            Token::GreaterEqual
        ]
    );

    Ok(())
}
