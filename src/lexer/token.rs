use std::ops::Range;

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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SourceSpan {
    pub start: usize,
    pub len: usize,
}

impl SourceSpan {
    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }
}

impl From<SourceSpan> for Range<usize> {
    fn from(span: SourceSpan) -> Self {
        span.start..span.start + span.len
    }
}

impl From<Range<usize>> for SourceSpan {
    fn from(range: Range<usize>) -> Self {
        SourceSpan {
            start: range.start,
            len: range.end - range.start,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedToken(pub Token, pub SourceSpan);
