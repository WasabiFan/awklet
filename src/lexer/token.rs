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
}
