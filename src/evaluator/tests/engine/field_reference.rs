use crate::{
    evaluator::{
        engine::ExecutionEngine, input::Record, tests::test_utils::TestEnvironment,
        EvaluationError, VariableValue,
    },
    parser::ast::{Expression, UnOp},
};
use std::rc::Rc;

#[test]
fn field_reference_basic() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = spaced_record!["foo", "bar", "baz"];
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::UnaryOperation(
            UnOp::FieldReference,
            Box::new(Expression::NumericLiteral(2.)),
        ),
    )?;

    assert_eq!(value, VariableValue::String(String::from("bar")));
    Ok(())
}

#[test]
fn field_reference_nonexistent() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = spaced_record!["foo", "bar", "baz"];
    let value = engine.evaluate_expression(
        &mut record,
        &Expression::UnaryOperation(
            UnOp::FieldReference,
            Box::new(Expression::NumericLiteral(4.)),
        ),
    )?;

    assert_eq!(value, VariableValue::String(String::from("")));
    Ok(())
}
