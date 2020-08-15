/// Sample programs taken from https://en.wikipedia.org/wiki/AWK
mod environment;

use awklet::{evaluator::ProgramEvaluator, lexer::tokenize, parser::parse};
use environment::IntegrationTestEnvironment;
use std::rc::Rc;

#[test]
fn hello_world() {
    let program = "BEGIN { print \"Hello, world!\" }";
    let tokens = tokenize(program).unwrap();
    let ast = parse(&tokens[..]).unwrap();

    let environment = Rc::new(IntegrationTestEnvironment::default());
    let evaluator = ProgramEvaluator::new(ast, environment.clone());
    evaluator.begin().unwrap();

    assert_eq!(environment.get_output(), "Hello, world!\n");
}

#[test]
fn count_words() {
    // Edits from original:
    //   - Removed comment (unsupported)
    //   - Added parens for length() call
    let program = r#"{
        words += NF
        chars += length() + 1
    }
    END { print NR, words, chars }"#;
    let tokens = tokenize(program).unwrap();
    let ast = parse(&tokens[..]).unwrap();

    let environment = Rc::new(IntegrationTestEnvironment::default());
    let evaluator = ProgramEvaluator::new(ast, environment.clone());

    evaluator.begin().unwrap();
    evaluator
        .consume_and_process_all("this is a record \nspaces  are\ntricky\n")
        .unwrap();
    evaluator.end().unwrap();

    assert_eq!(environment.get_output(), "3 7 37\n");
}

#[test]
fn sum_last_word() {
    let program = r#"
    { s += $NF }
    END { print s + 0 }
    "#;
    let tokens = tokenize(program).unwrap();
    let ast = parse(&tokens[..]).unwrap();

    let environment = Rc::new(IntegrationTestEnvironment::default());
    let evaluator = ProgramEvaluator::new(ast, environment.clone());

    evaluator.begin().unwrap();
    evaluator
        .consume_and_process_all("4 b 7\n2\n foo 50bar\n")
        .unwrap();
    evaluator.end().unwrap();

    assert_eq!(environment.get_output(), "59\n");
}
