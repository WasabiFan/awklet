#[derive(Debug, PartialEq)]
pub enum Token {
    NumericLiteral(f64),
    Identifier(String),
}
