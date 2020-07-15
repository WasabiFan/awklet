#[derive(Debug, PartialEq)]
pub struct Ast {
    pub rules: Vec<Rule>,
}

#[derive(Debug, PartialEq)]
pub struct Rule {
    pub pattern: Pattern,
    pub action: Action,
}

#[derive(Debug, PartialEq)]
pub enum SingleConditionPattern {
    // TODO: would be nice to store regex as Regex, but that would make testing difficult.
    Regex(String),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    SingleCondition(SingleConditionPattern),
    Range(SingleConditionPattern, SingleConditionPattern),
    Begin,
    End,
    // TODO: BEGINFILE, ENDFILE omitted
    Empty,
}

#[derive(Debug, PartialEq)]
pub enum Action {
    Empty,
    Present(Vec<Statement>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    // TODO: control
    // TODO: compound
    // commands are treated as expressions
    // Command(String, Vec<Expression>),
    // TODO: deletion statements
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Increment,
    Decrement,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    FunctionCall(String, Vec<Expression>),
    BinaryOperation(BinOp),
    UnaryOperation(UnOp),
    FieldReference(Box<Expression>),
    VariableValue(String),
    Command(String, Vec<Expression>),
}
