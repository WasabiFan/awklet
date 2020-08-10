use crate::{
    evaluator::{
        engine::ExecutionEngine, tests::test_utils::TestEnvironment, EvaluationError, VariableValue,
    },
    parser::ast::{Expression, UnOp},
};
use std::rc::Rc;

#[test]
fn decrement() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    engine.set_variable("myvar", VariableValue::Numeric(5.));
    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::Decrement,
        Box::new(Expression::VariableValue(String::from("myvar"))),
    ))?;

    assert_eq!(value, VariableValue::Numeric(5.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(4.)
    );
    Ok(())
}

#[test]
fn decrement_field() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["some", "4", "record"]);

    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::Decrement,
        Box::new(Expression::UnaryOperation(
            UnOp::FieldReference,
            Box::new(Expression::NumericLiteral(2.)),
        )),
    ))?;

    assert_eq!(value, VariableValue::Numeric(4.));
    assert_eq!(
        engine.get_record().get_field(2),
        VariableValue::NumericString(3., String::from("3"))
    );
    assert_eq!(
        engine.get_record().get_field(0),
        VariableValue::String(String::from("some 3 record"))
    );
    Ok(())
}

#[test]
fn decrement_string() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());
    engine.set_record(spaced_record!["some", "4", "record"]);

    let decrement_value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::Decrement,
        Box::new(Expression::UnaryOperation(
            UnOp::FieldReference,
            Box::new(Expression::NumericLiteral(1.)),
        )),
    ))?;

    let new_value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::FieldReference,
        Box::new(Expression::NumericLiteral(1.)),
    ))?;

    assert_eq!(decrement_value, VariableValue::Numeric(0.));
    assert_eq!(
        new_value,
        VariableValue::NumericString(-1., String::from("-1"))
    );

    Ok(())
}

#[test]
fn increment() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    engine.set_variable("myvar", VariableValue::Numeric(5.));
    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::Increment,
        Box::new(Expression::VariableValue(String::from("myvar"))),
    ))?;

    assert_eq!(value, VariableValue::Numeric(5.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(6.)
    );
    Ok(())
}

#[test]
fn increment_nonexistent() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let value = engine.evaluate_expression(&Expression::UnaryOperation(
        UnOp::Increment,
        Box::new(Expression::VariableValue(String::from("myvar"))),
    ))?;

    assert_eq!(value, VariableValue::Numeric(0.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(1.)
    );
    Ok(())
}
