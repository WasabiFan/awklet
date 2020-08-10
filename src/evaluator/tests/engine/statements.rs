use crate::{
    evaluator::{
        engine::ExecutionEngine, tests::test_utils::TestEnvironment, EvaluationError, VariableValue,
    },
    parser::ast::{BinOp, BuiltinCommand, Expression, Statement, UnOp},
};
use std::rc::Rc;

#[test]
fn default_print_command() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar"]);

    engine.execute_statement(&Statement::Command(BuiltinCommand::Print, vec![]))?;

    assert_eq!(env.get_printed_lines(), vec![String::from("foo bar\n")]);
    Ok(())
}

#[test]
fn multi_print() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar"]);

    engine.execute_statement(&Statement::Command(
        BuiltinCommand::Print,
        vec![
            Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(0.)),
            ),
            Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(1.)),
            ),
            Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(2.)),
            ),
        ],
    ))?;

    assert_eq!(
        env.get_printed_lines(),
        vec![String::from("foo bar foo bar\n")]
    );
    Ok(())
}

#[test]
fn print_with_ors() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_variable("ORS", VariableValue::String(String::from(";")));
    engine.set_record(spaced_record!["foo", "bar"]);

    engine.execute_statement(&Statement::Command(BuiltinCommand::Print, vec![]))?;
    engine.execute_statement(&Statement::Command(BuiltinCommand::Print, vec![]))?;

    assert_eq!(
        env.get_printed_lines(),
        vec![String::from("foo bar;"), String::from("foo bar;")]
    );
    Ok(())
}

#[test]
fn expression_as_statement() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar"]);

    engine.execute_statement(&Statement::Expression(Expression::BinaryOperation(
        BinOp::Assign,
        Box::new(Expression::VariableValue(String::from("myvar"))),
        Box::new(Expression::NumericLiteral(3.)),
    )))?;

    assert_eq!(
        engine.get_variable("myvar"),
        Some(&VariableValue::Numeric(3.))
    );
    Ok(())
}
