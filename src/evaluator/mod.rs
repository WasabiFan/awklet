use crate::parser::ast::{Action, Ast, BuiltinCommand, Expression, Pattern, Statement};
use engine::ExecutionEngine;
use input::Record;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[cfg(test)]
mod test_utils;

mod engine;
mod input;

const INPUT_FIELD_SEPARATOR_NAME: &str = "FS";
const OUTPUT_FIELD_SEPARATOR_NAME: &str = "OFS";
const INPUT_RECORD_SEPARATOR_NAME: &str = "RS";
const OUTPUT_RECORD_SEPARATOR_NAME: &str = "ORS";

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    NoSuchVariable(String),
    InvalidNumericLiteral(String),
    NonVariableAsLvalue(Expression),
    InvalidFieldReference(f64),
}

pub trait Environment {
    fn print(&self, string: &str);
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableValue {
    String(String),
    Numeric(f64),
    NumericString(String),
}

impl VariableValue {
    pub fn to_numeric(&self) -> Result<f64, EvaluationError> {
        // TODO: officially, any numeric prefix should be allowable, not just the whole string
        match self {
            VariableValue::String(string) => string
                .parse()
                .map_err(|_| EvaluationError::InvalidNumericLiteral(string.clone())),
            VariableValue::NumericString(string) => string
                .parse()
                .map_err(|_| EvaluationError::InvalidNumericLiteral(string.clone())),
            VariableValue::Numeric(val) => Ok(*val),
        }
    }

    pub fn to_string(&self) -> Result<String, EvaluationError> {
        match self {
            VariableValue::String(string) => Ok(string.clone()),
            VariableValue::NumericString(string) => Ok(string.clone()),
            VariableValue::Numeric(val) => Ok(val.to_string()),
        }
    }
}

#[derive(Debug)]
pub struct Closure {
    // TODO: variable types
    variables: HashMap<String, VariableValue>,
}

impl Closure {
    pub fn get_variable(&self, name: &str) -> Result<&VariableValue, EvaluationError> {
        self.variables
            .get(name)
            .ok_or(EvaluationError::NoSuchVariable(String::from(name)))
    }

    pub fn set_variable(&mut self, name: &str, value: VariableValue) {
        self.variables.insert(String::from(name), value);
    }
}

impl Default for Closure {
    fn default() -> Self {
        Closure {
            variables: hashmap![
                String::from(INPUT_FIELD_SEPARATOR_NAME) => VariableValue::String(String::from(" ")),
                String::from(OUTPUT_FIELD_SEPARATOR_NAME) => VariableValue::String(String::from(" ")),
                String::from(INPUT_RECORD_SEPARATOR_NAME) => VariableValue::String(String::from("\n")),
                String::from(OUTPUT_RECORD_SEPARATOR_NAME) => VariableValue::String(String::from("\n"))
            ],
        }
    }
}

pub struct ProgramEvaluator {
    program: Ast,
    engine: RefCell<ExecutionEngine>,
}

impl ProgramEvaluator {
    pub fn new(program: Ast, env: Rc<dyn Environment>) -> ProgramEvaluator {
        ProgramEvaluator {
            program,
            engine: RefCell::new(ExecutionEngine::new(env)),
        }
    }

    pub fn begin(&self) -> Result<(), EvaluationError> {
        let record = Record::default();
        for rule in self
            .program
            .rules
            .iter()
            .filter(|r| r.pattern == Pattern::Begin)
        {
            self.execute_action(&rule.action, &record)?;
        }

        Ok(())
    }

    fn execute_action(&self, action: &Action, record: &Record) -> Result<(), EvaluationError> {
        match action {
            Action::Empty => self
                .engine
                .borrow_mut()
                .execute_statements(record, &[Statement::Command(BuiltinCommand::Print, vec![])]),
            Action::Present(statements) => self
                .engine
                .borrow_mut()
                .execute_statements(record, statements),
        }
    }

    pub fn set_variable(&mut self, name: &str, value: VariableValue) {
        self.engine.borrow_mut().set_variable(name, value);
    }

    pub fn get_variable(&mut self, name: &str) -> Result<VariableValue, EvaluationError> {
        Ok(self.engine.borrow_mut().get_variable(name)?.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::{test_utils::TestEnvironment, EvaluationError, ProgramEvaluator, VariableValue};
    use crate::parser::ast::{
        Action, Ast, BuiltinCommand, Expression, Pattern, Rule, Statement, UnOp,
    };
    use std::rc::Rc;

    #[test]
    fn test_begin_empty_action() -> Result<(), EvaluationError> {
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
    fn test_get_set_variable() -> Result<(), EvaluationError> {
        let program = Ast { rules: vec![] };

        let env = Rc::new(TestEnvironment::default());
        let mut evaluator = ProgramEvaluator::new(program, env.clone());

        evaluator.set_variable("myvariable", VariableValue::String(String::from("foo")));
        assert_eq!(
            evaluator.get_variable("myvariable")?,
            VariableValue::String(String::from("foo"))
        );

        Ok(())
    }

    #[test]
    fn test_get_nonexistent_variable() {
        let program = Ast { rules: vec![] };

        let env = Rc::new(TestEnvironment::default());
        let mut evaluator = ProgramEvaluator::new(program, env.clone());

        assert_eq!(
            evaluator.get_variable("i_dont_exist"),
            Err(EvaluationError::NoSuchVariable(String::from(
                "i_dont_exist"
            )))
        );
    }

    #[test]
    fn test_begin_present_action() -> Result<(), EvaluationError> {
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
    fn test_begin_record_value() -> Result<(), EvaluationError> {
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
}
