use crate::{
    evaluator::{
        input::Record, tests::test_utils::TestEnvironment, EvaluationError, ProgramEvaluator,
    },
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

    let record = Record::new(
        String::from("foo 1 bar"),
        vec![String::from("foo"), String::from("1"), String::from("bar")],
    );
    evaluator.process_record(record)?;

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

    let record = Record::new(
        String::from("foo 0 bar"),
        vec![String::from("foo"), String::from("0"), String::from("bar")],
    );
    evaluator.process_record(record)?;

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

    let record1 = Record::new(
        String::from("foo"),
        vec![String::from("foo")],
    );
    evaluator.process_record(record1)?;

    let record2 = Record::new(
        String::from("bar"),
        vec![String::from("bar")],
    );
    evaluator.process_record(record2)?;

    assert_eq!(env.get_printed_lines(), vec![
        String::from("1\n"),
        String::from("2\n"),
    ]);
    Ok(())
}

#[test]
fn record_count_begin_end() -> Result<(), EvaluationError> {
    let program = Ast {
        rules: vec![Rule {
            pattern: Pattern::Begin,
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::VariableValue(String::from("NR"))],
            )]),
        }, Rule {
            pattern: Pattern::End,
            action: Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![Expression::VariableValue(String::from("NR"))],
            )]),
        }],
    };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.begin()?;

    let record1 = Record::new(
        String::from("foo"),
        vec![String::from("foo")],
    );
    evaluator.process_record(record1)?;

    let record2 = Record::new(
        String::from("bar"),
        vec![String::from("bar")],
    );
    evaluator.process_record(record2)?;

    evaluator.end()?;

    assert_eq!(env.get_printed_lines(), vec![
        String::from("0\n"),
        String::from("2\n"),
    ]);
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

    let record1 = Record::new(
        String::from("foo"),
        vec![String::from("foo")],
    );
    evaluator.process_record(record1)?;

    evaluator.end()?;

    assert_eq!(env.get_printed_lines(), vec![
        String::from("6\n"),
    ]);
    Ok(())
}
