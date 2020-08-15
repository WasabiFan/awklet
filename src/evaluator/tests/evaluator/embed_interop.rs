use crate::{
    evaluator::{
        tests::test_utils::TestEnvironment, EvaluationError, ProgramEvaluator, VariableValue,
    },
    parser::ast::Ast,
};
use std::rc::Rc;

#[test]
fn get_set_variable() -> Result<(), EvaluationError> {
    let program = Ast { rules: vec![] };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    evaluator.set_variable("myvariable", VariableValue::String(String::from("foo")));
    assert_eq!(
        evaluator.get_variable("myvariable")?,
        VariableValue::String(String::from("foo"))
    );

    Ok(())
}

#[test]
fn get_nonexistent_variable() {
    let program = Ast { rules: vec![] };

    let env = Rc::new(TestEnvironment::default());
    let evaluator = ProgramEvaluator::new(program, env.clone());

    assert_eq!(
        evaluator.get_variable("i_dont_exist"),
        Err(EvaluationError::NoSuchVariable(String::from(
            "i_dont_exist"
        )))
    );
}
