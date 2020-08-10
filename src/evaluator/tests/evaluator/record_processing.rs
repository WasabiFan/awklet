use crate::{
    evaluator::{tests::test_utils::TestEnvironment, EvaluationError, ProgramEvaluator},
    parser::ast::{Action, Ast, BuiltinCommand, Expression, Pattern, Rule, Statement, UnOp},
};
use std::rc::Rc;

#[test]
fn simple_expression_matches() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::SingleCondition(Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(2.)),
            )),
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::UnaryOperation(
                    UnOp::FieldReference,
                    Box::new(Expression::NumericLiteral(0.)),
                )],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.process_record(spaced_record!["foo", "1", "bar"])?;

    assert_eq!(env.get_printed_lines(), vec![String::from("foo 1 bar\n")]);
    Ok(())
}

#[test]
fn simple_expression_not_matches() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::SingleCondition(Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(2.)),
            )),
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::UnaryOperation(
                    UnOp::FieldReference,
                    Box::new(Expression::NumericLiteral(0.)),
                )],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.process_record(spaced_record!["foo", "0", "bar"])?;

    assert_eq!(env.get_printed_lines(), Vec::<String>::new());
    Ok(())
}

#[test]
fn record_count_basic_increment() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::Empty,
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::VariableValue(String::from("NR"))],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.process_record(spaced_record!["foo"])?;
    evaluator.process_record(spaced_record!["bar"])?;

    assert_eq!(
        env.get_printed_lines(),
        vec![String::from("1\n"), String::from("2\n"),]
    );
    Ok(())
}

#[test]
fn record_count_begin_end() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![
            Rule {
                pattern: Pattern::Begin,
                action: Action::Present(vec![Statement::Command(
                    BuiltinCommand::Print,
                    vec![Expression::VariableValue(String::from("NR"))],
                )]),
            },
            Rule {
                pattern: Pattern::End,
                action: Action::Present(vec![Statement::Command(
                    BuiltinCommand::Print,
                    vec![Expression::VariableValue(String::from("NR"))],
                )]),
            },
        ],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.begin()?;
    evaluator.process_record(spaced_record!["foo"])?;
    evaluator.process_record(spaced_record!["bar"])?;
    evaluator.end()?;

    assert_eq!(
        env.get_printed_lines(),
        vec![String::from("0\n"), String::from("2\n"),]
    );
    Ok(())
}

#[test]
fn record_count_assign() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::End,
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::VariableValue(String::from("NR"))],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.set_variable("NR", crate::evaluator::VariableValue::Numeric(5.));
    evaluator.process_record(spaced_record!["foo"])?;
    evaluator.end()?;

    assert_eq!(env.get_printed_lines(), vec![String::from("6\n"),]);
    Ok(())
}
