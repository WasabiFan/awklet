use crate::{
    evaluator::{
        engine::ExecutionEngine, input::Record, tests::test_utils::TestEnvironment,
        EvaluationError, VariableValue,
    },
    parser::ast::{BinOp, Expression, UnOp},
};
use std::rc::Rc;

#[test]
fn negation() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    engine.set_variable("myvar", VariableValue::Numeric(5.));
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::UnaryOperation(
            UnOp::Negation,
            Box::new(Expression::VariableValue(String::from("myvar"))),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(-5.));
    assert_eq!(
        engine.get_variable("myvar").unwrap(),
        &VariableValue::Numeric(5.)
    );
    Ok(())
}

#[test]
fn add() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Add,
            Box::new(Expression::NumericLiteral(2.)),
            Box::new(Expression::NumericLiteral(2.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(4.));
    Ok(())
}

#[test]
fn subtract() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Subtract,
            Box::new(Expression::NumericLiteral(3.)),
            Box::new(Expression::NumericLiteral(2.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}

#[test]
fn multiply() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Multiply,
            Box::new(Expression::NumericLiteral(3.)),
            Box::new(Expression::NumericLiteral(3.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(9.));
    Ok(())
}

#[test]
fn divide() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Divide,
            Box::new(Expression::NumericLiteral(12.)),
            Box::new(Expression::NumericLiteral(3.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(4.));
    Ok(())
}

#[test]
fn modulo() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::default();
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::BinaryOperation(
            BinOp::Mod,
            Box::new(Expression::NumericLiteral(11.)),
            Box::new(Expression::NumericLiteral(5.)),
        ),
    )?;

    assert_eq!(value, VariableValue::Numeric(1.));
    Ok(())
}
