use crate::{
    evaluator::{
        engine::ExecutionEngine, input::Record, tests::test_utils::TestEnvironment, EvaluationError,
    },
    parser::ast::{BuiltinCommand, Expression, Statement, UnOp},
};
use std::rc::Rc;

#[test]
fn default_print_command() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::new(String::from("foo bar"), vec![]);
    engine.execute_statement(
        &mut record,
        &Statement::Command(BuiltinCommand::Print, vec![]),
    )?;

    assert_eq!(env.get_printed_lines(), vec![String::from("foo bar\n")]);
    Ok(())
}

#[test]
fn multi_print() -> Result<(), EvaluationError> {
    let env = Rc::new(TestEnvironment::default());
    let mut engine = ExecutionEngine::new(env.clone());

    let mut record = Record::new(
        String::from("foo bar"),
        vec![String::from("foo"), String::from("bar")],
    );
    engine.execute_statement(
        &mut record,
        &Statement::Command(
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
        ),
    )?;

    assert_eq!(
        env.get_printed_lines(),
        vec![String::from("foo bar foo bar\n")]
    );
    Ok(())
}
