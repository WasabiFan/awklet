use crate::{
    evaluator::{
        engine::ExecutionEngine, input::Record, tests::test_utils::TestEnvironment,
        EvaluationError, VariableValue,
    },
    parser::ast::{BinOp, Expression},
};
use std::rc::Rc;

#[test]
fn assign() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Assign,
            Box::new(Expression::VariableValue(String::from("myvar"))),
            Box::new(Expression::NumericLiteral(5.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(5.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(5.)
    );
    Ok(())
}

#[test]
fn add_assign() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    engine.set_variable("myvar", VariableValue::Numeric(5.));
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::AddAssign,
            Box::new(Expression::VariableValue(String::from("myvar"))),
            Box::new(Expression::NumericLiteral(2.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(7.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(7.)
    );
    Ok(())
}

#[test]
fn add_assign_create() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::AddAssign,
            Box::new(Expression::VariableValue(String::from("myvar"))),
            Box::new(Expression::NumericLiteral(2.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(2.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(2.)
    );
    Ok(())
}

#[test]
fn subtract_assign() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    engine.set_variable("myvar", VariableValue::Numeric(5.));
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::SubtractAssign,
            Box::new(Expression::VariableValue(String::from("myvar"))),
            Box::new(Expression::NumericLiteral(2.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(3.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(3.)
    );
    Ok(())
}
