#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub rules: Vec<Rule>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Rule {
    pub pattern: Pattern,
    pub action: Action,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SingleConditionPattern {
    // TODO: would be nice to store regex as Regex, but that would make testing difficult.
    Regex(String),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    SingleCondition(SingleConditionPattern),
    Range(SingleConditionPattern, SingleConditionPattern),
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
pub enum Statement {
    Expression(Expression),
    // TODO: control
    // TODO: compound
    // commands are treated as expressions
    // Command(String, Vec<Expression>),
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnOp {
    Increment,
    Decrement,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    NumericLiteral(f64),
    Negation(Box<Expression>),
    StringLiteral(String),
    FunctionCall(String, Vec<Expression>),
    BinaryOperation(BinOp, Box<Expression>, Box<Expression>),
    UnaryOperation(UnOp, Box<Expression>),
    FieldReference(Box<Expression>),
    VariableValue(String),
    Command(String, Vec<Expression>),
}
