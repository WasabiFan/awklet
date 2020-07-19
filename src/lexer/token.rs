#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Literals
    NumericLiteral(f64),
    StringLiteral(String),
    RegexLiteral(String),

    // Core language
    Identifier(String),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    StatementSeparator,

    FieldReference,

    // Keywords
    BeginKeyword,
    EndKeyword,

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

    // Relational and redirection
    LeftCaret,
    LessEqual,
    CompareEquals,
    BangEqual,
    RightCaret,
    GreaterEqual,
    // TODO: redirection operators

    // TODO: matching, nonmatching operators
    // TODO: logical and/or

    // Assignment operators
    AssignEquals,
    PlusEquals,
    MinusEquals,
    // TODO: misc other assignments
}
