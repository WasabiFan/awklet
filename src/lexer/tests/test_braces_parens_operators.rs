use crate::lexer::{tokenize, Token, TokenizeError};

#[test]
fn braces_parens() -> Result<(), TokenizeError> {
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

#[test]
fn math_operators() -> Result<(), TokenizeError> {
    let tokens = tokenize("1 +44.2 -4. / 18*99%2")?;

    assert_eq!(
        tokens,
        vec![
            Token::NumericLiteral(1.),
            Token::Plus,
            Token::NumericLiteral(44.2),
            Token::Minus,
            Token::NumericLiteral(4.),
            Token::Slash,
            Token::NumericLiteral(18.),
            Token::Star,
            Token::NumericLiteral(99.),
            Token::Mod,
            Token::NumericLiteral(2.),
        ]
    );

    Ok(())
}

#[test]
fn increment_decrement() -> Result<(), TokenizeError> {
    let tokens = tokenize("a++ + +b-- -2")?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("a")),
            Token::Increment,
            Token::Plus,
            Token::Plus,
            Token::Identifier(String::from("b")),
            Token::Decrement,
            Token::Minus,
            Token::NumericLiteral(2.),
        ]
    );

    Ok(())
}

#[test]
fn plus_equals() -> Result<(), TokenizeError> {
    let tokens = tokenize("a+=-9")?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("a")),
            Token::PlusEquals,
            Token::Minus,
            Token::NumericLiteral(9.),
        ]
    );

    Ok(())
}

#[test]
fn minus_equals() -> Result<(), TokenizeError> {
    let tokens = tokenize("a -=-12")?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("a")),
            Token::MinusEquals,
            Token::Minus,
            Token::NumericLiteral(12.),
        ]
    );

    Ok(())
}

#[test]
fn assignment() -> Result<(), TokenizeError> {
    let tokens = tokenize("a = -88")?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("a")),
            Token::AssignEquals,
            Token::Minus,
            Token::NumericLiteral(88.),
        ]
    );

    Ok(())
}

#[test]
fn field_reference_comma() -> Result<(), TokenizeError> {
    let tokens = tokenize("print $2, $ 5")?;

    assert_eq!(
        tokens,
        vec![
            Token::Identifier(String::from("print")),
            Token::FieldReference,
            Token::NumericLiteral(2.),
            Token::Comma,
            Token::FieldReference,
            Token::NumericLiteral(5.),
        ]
    );

    Ok(())
}
