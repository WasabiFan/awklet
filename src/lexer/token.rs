#[derive(Debug, PartialEq)]
pub enum Token {
    // Literals
    NumericLiteral(f64),
    StringLiteral(String),

    // Core language
    Identifier(String),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,

    FieldReference,

    // TODO: field reference

    // Unary operators
    Increment,
    Decrement,

    // TODO: exponentiation operators
    // TODO: bang/not

    // Pure math operators
    Plus,
    Minus,
    Star,
    Slash,
    Mod,

    // TODO: relational and redirection operators
    // TODO: matching, nonmatching operators
    // TODO: logical and/or

    // Assignment operators
    AssignEquals,
    PlusEquals,
    MinusEquals,
    // TODO: misc other assignments
}
