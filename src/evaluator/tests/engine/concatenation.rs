use crate::{
    evaluator::{
        engine::ExecutionEngine, input::Record, tests::test_utils::TestEnvironment,
        EvaluationError, VariableValue,
    },
    parser::ast::{BinOp, Expression},
};
use std::rc::Rc;

#[test]
fn concatenate_strings() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Concatenate,
            Box::new(Expression::StringLiteral(String::from("foo"))),
            Box::new(Expression::StringLiteral(String::from("bar"))),
        ),
    )?;

    assert_eq!(value, VariableValue::String(String::from("foobar")));
    Ok(())
}

#[test]
fn concatenate_with_integer() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Concatenate,
            Box::new(Expression::StringLiteral(String::from("foo"))),
            Box::new(Expression::NumericLiteral(19.)),
        ),
    )?;

    assert_eq!(value, VariableValue::String(String::from("foo19")));
    Ok(())
}

#[test]
fn concatenate_with_float() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Concatenate,
            Box::new(Expression::StringLiteral(String::from("foo"))),
            Box::new(Expression::NumericLiteral(19.8)),
        ),
    )?;

    assert_eq!(value, VariableValue::String(String::from("foo19.8")));
    Ok(())
}
