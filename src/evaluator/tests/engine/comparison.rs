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


#[test]
fn compare_equal_given_less_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareEquals,
            Box::new(Expression::NumericLiteral(19.)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_equal_given_equal_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareEquals,
            Box::new(Expression::NumericLiteral(19.8)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_equal_given_greater_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareEquals,
            Box::new(Expression::NumericLiteral(19.9)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}


#[test]
fn compare_not_equal_given_less_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareNotEquals,
            Box::new(Expression::NumericLiteral(19.)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_not_equal_given_equal_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareNotEquals,
            Box::new(Expression::NumericLiteral(19.8)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_not_equal_given_greater_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareNotEquals,
            Box::new(Expression::NumericLiteral(19.9)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}


#[test]
fn compare_greater_equal_given_less_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreaterEquals,
            Box::new(Expression::NumericLiteral(19.)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_greater_equal_given_equal_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreaterEquals,
            Box::new(Expression::NumericLiteral(19.8)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_greater_equal_given_greater_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreaterEquals,
            Box::new(Expression::NumericLiteral(19.9)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}


#[test]
fn compare_greater_given_less_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreater,
            Box::new(Expression::NumericLiteral(19.)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_greater_given_equal_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreater,
            Box::new(Expression::NumericLiteral(19.8)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}

#[test]
fn compare_greater_given_greater_numeric() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreater,
            Box::new(Expression::NumericLiteral(19.9)),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn compare_strings() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::CompareGreaterEquals,
            Box::new(Expression::StringLiteral(String::from("abc"))),
            Box::new(Expression::StringLiteral(String::from("xyz"))),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(0.));
    Ok(())
}