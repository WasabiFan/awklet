use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Ast {
    pub rules: Vec<Rule>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Rule {
    pub pattern: Pattern,
    pub action: Action,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    SingleCondition(Expression),
    Range(Expression, Expression),
    Begin,
    End,
    // TODO: BEGINFILE, ENDFILE omitted
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Action {
    Empty,
    Present(Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinCommand {
    Print,
    // Printf
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression(Expression),
    Command(BuiltinCommand, Vec<Expression>),
    // TODO: control
    // TODO: compound
    // TODO: deletion statements
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Assign,
    AddAssign,
    SubtractAssign,
    Concatenate,
    CompareLess,
    CompareLessEquals,
    CompareEquals,
    CompareNotEquals,
    CompareGreater,
    CompareGreaterEquals,
}

impl BinOp {
    pub fn partial_from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(BinOp::Add),
            Token::Minus => Some(BinOp::Subtract),
            Token::Star => Some(BinOp::Multiply),
            Token::Slash => Some(BinOp::Divide),
            Token::Mod => Some(BinOp::Mod),
            Token::AssignEquals => Some(BinOp::Assign),
            Token::PlusEquals => Some(BinOp::AddAssign),
            Token::MinusEquals => Some(BinOp::SubtractAssign),
            Token::LeftCaret => Some(BinOp::CompareLess),
            Token::LessEqual => Some(BinOp::CompareLessEquals),
            Token::CompareEquals => Some(BinOp::CompareEquals),
            Token::RightCaret => Some(BinOp::CompareGreater),
            Token::GreaterEqual => Some(BinOp::CompareGreaterEquals),
            _ => None,
        }
    }

    pub fn is_valid_op(token: &Token) -> bool {
        BinOp::partial_from_token(token).is_some()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnOp {
    FieldReference,
    Negation,
    Increment,
    Decrement,
}

impl UnOp {
    pub fn partial_from_token(token: &Token) -> Option<Self> {
        match token {
            Token::FieldReference => Some(UnOp::FieldReference),
            Token::Minus => Some(UnOp::Negation),
            Token::Increment => Some(UnOp::Increment),
            Token::Decrement => Some(UnOp::Decrement),
            _ => None,
        }
    }

    pub fn is_valid_op(token: &Token) -> bool {
        UnOp::partial_from_token(token).is_some()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    NumericLiteral(f64),
    StringLiteral(String),
    // TODO: would be nice to store regex as Regex, but that would make testing difficult.
    RegexLiteral(String),
    FunctionCall(String, Vec<Expression>),
    BinaryOperation(BinOp, Box<Expression>, Box<Expression>),
    UnaryOperation(UnOp, Box<Expression>),
    VariableValue(String),
    // TODO: getline and friends
}
