use crate::{
    evaluator::{
        engine::ExecutionEngine, tests::test_utils::TestEnvironment, EvaluationError, VariableValue,
    },
    parser::ast::{Expression, UnOp},
};
use std::rc::Rc;

#[test]
fn field_reference_basic() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar", "baz"]);

    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::FieldReference,
        Box::new(Expression::NumericLiteral(2.)),
    ))?;

    assert_eq!(value, VariableValue::String(String::from("bar")));
    Ok(())
}

#[test]
fn field_reference_nonexistent() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar", "baz"]);

    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::FieldReference,
        Box::new(Expression::NumericLiteral(4.)),
    ))?;

    assert_eq!(value, VariableValue::String(String::from("")));
    Ok(())
}

#[test]
fn field_reference_negative() {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar", "baz"]);

    let result = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::FieldReference,
        Box::new(Expression::NumericLiteral(-1.)),
    ));

    assert_eq!(result, Err(EvaluationError::InvalidFieldReference(-1.)));
}

#[test]
fn field_reference_decimal() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["foo", "bar", "baz"]);

    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::FieldReference,
        Box::new(Expression::NumericLiteral(2.7)),
    ))?;

    assert_eq!(value, VariableValue::String(String::from("bar")));
    Ok(())
}
