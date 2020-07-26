use crate::{
    evaluator::{tests::test_utils::TestEnvironment, EvaluationError, ProgramEvaluator},
    parser::ast::{Action, Ast, BuiltinCommand, Expression, Pattern, Rule, Statement, UnOp},
};
use std::rc::Rc;

#[test]
fn begin_empty_action() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::Begin,
            action: Action::Empty,
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());
    evaluator.begin()?;

    assert_eq!(env.get_printed_lines(), vec![String::from("\n")]);
    Ok(())
}

#[test]
fn begin_present_action() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::Begin,
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::NumericLiteral(5.)],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());
    evaluator.begin()?;

    assert_eq!(env.get_printed_lines(), vec![String::from("5\n")]);
    Ok(())
}

#[test]
fn begin_record_value() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::Begin,
            action: Action::Present(vec![Statement::Command(
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
                ],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());
    evaluator.begin()?;

    assert_eq!(env.get_printed_lines(), vec![String::from(" \n")]);
    Ok(())
}
