use crate::{
    evaluator::{
        engine::ExecutionEngine, tests::test_utils::TestEnvironment, EvaluationError, VariableValue,
    },
    parser::ast::Expression,
};
use std::rc::Rc;

#[test]
fn basic_length_with_arg() {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let value = engine
        .evaluate_expression(&Expression::FunctionCall(
            String::from("length"),
            vec![Expression::StringLiteral(String::from("some string"))],
        ))
        .unwrap();

    assert_eq!(value, VariableValue::Numeric(11.));
}

#[test]
fn basic_length_no_arg() {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    engine.set_record(spaced_record!["foo", "bar"]);
    let value = engine
        .evaluate_expression(&Expression::FunctionCall(String::from("length"), vec![]))
        .unwrap();

    assert_eq!(value, VariableValue::Numeric(7.));
}

#[test]
fn basic_length_extra_arg() {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    assert_eq!(
        engine.evaluate_expression(&Expression::FunctionCall(
            String::from("length"),
            vec![
                Expression::NumericLiteral(1.),
                Expression::NumericLiteral(2.)
            ],
        )),
        Err(EvaluationError::NoSuchFunction(String::from("length")))
    );
}
