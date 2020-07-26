use crate::{
    evaluator::{
        engine::ExecutionEngine, input::Record, tests::test_utils::TestEnvironment,
        EvaluationError, VariableValue,
    },
    parser::ast::{BinOp, Expression},
};
use std::rc::Rc;

#[test]
fn compare_less_given_less_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareLess,
            Box::new(Expression::NumericLiteral(19.)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_less_given_equal_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareLess,
            Box::new(Expression::NumericLiteral(19.8)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_less_given_greater_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareLess,
            Box::new(Expression::NumericLiteral(19.9)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_less_equal_given_less_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareLessEquals,
            Box::new(Expression::NumericLiteral(19.)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_less_equal_given_equal_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareLessEquals,
            Box::new(Expression::NumericLiteral(19.8)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_less_equal_given_greater_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareLessEquals,
            Box::new(Expression::NumericLiteral(19.9)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}
