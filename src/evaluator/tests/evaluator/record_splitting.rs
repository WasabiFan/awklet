use crate::{
    evaluator::{record::Record, tests::test_utils::TestEnvironment, ProgramEvaluator},
    parser::ast::Ast,
};
use matches::assert_matches;
use std::rc::Rc;

#[test]
fn consumes_full_string_if_no_separator() {
    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(Ast::default(), env);

    let input = "this is a record";
    let (num_consumed_chars, record) = evaluator.consume_next_record(input).unwrap().unwrap();

    assert_eq!(num_consumed_chars, 16);
    assert_eq!(record, spaced_record!["this", "is", "a", "record"]);
}

#[test]
fn consumes_partial_string_includes_separator() {
    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(Ast::default(), env);

    let input = "this is a record\nthis is another";
    let (num_consumed_chars, record) = evaluator.consume_next_record(input).unwrap().unwrap();

    assert_eq!(num_consumed_chars, 17);
    assert_eq!(record, spaced_record!["this", "is", "a", "record"]);
}

#[test]
fn handles_separator_at_start() {
    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(Ast::default(), env);

    let input = "\nthis is a record";
    let (num_consumed_chars, record) = evaluator.consume_next_record(input).unwrap().unwrap();

    assert_eq!(num_consumed_chars, 1);
    assert_eq!(record, spaced_record![""]);
}

#[test]
fn handles_separator_at_end() {
    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(Ast::default(), env);

    let input = "this is a record\n";
    let (num_consumed_chars, record) = evaluator.consume_next_record(input).unwrap().unwrap();

    assert_eq!(num_consumed_chars, 17);
    assert_eq!(record, spaced_record!["this", "is", "a", "record"]);
}

#[test]
fn empty_string_returns_error() {
    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(Ast::default(), env);

    let input = "";
    assert_matches!(evaluator.consume_next_record(input), Ok(None));
}

#[test]
fn custom_single_char_separator() {
    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(Ast::default(), env);

    evaluator.set_variable(
        "RS",
        crate::evaluator::variable::VariableValue::String(String::from(";")),
    );

    let input = "one record\nstill;two record";
    let (num_consumed_chars, record) = evaluator.consume_next_record(input).unwrap().unwrap();

    assert_eq!(num_consumed_chars, 17);
    assert_eq!(
        record,
        Record::new(
            String::from("one record\nstill"),
            vec![
                String::from("one"),
                String::from("record"),
                String::from("still")
            ]
        )
    );
}
