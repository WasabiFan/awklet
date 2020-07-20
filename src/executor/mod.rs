use crate::parser::ast::{Action, Ast, BuiltinCommand, Pattern, Statement};
use engine::ExecutionEngine;
use input::Record;
use std::{collections::HashMap, rc::Rc};

#[cfg(test)]
mod test_utils;

mod engine;
mod input;

const INPUT_FIELD_SEPARATOR_NAME: &str = "FS";
const OUTPUT_FIELD_SEPARATOR_NAME: &str = "OFS";
const INPUT_RECORD_SEPARATOR_NAME: &str = "RS";
const OUTPUT_RECORD_SEPARATOR_NAME: &str = "ORS";

#[derive(Debug)]
pub enum ExecutionError {
    NoSuchVariable(String),
    InvalidNumericLiteral(String),
}

pub trait Environment {
    fn print(&self, string: &str);
}

#[derive(Debug)]
pub enum VariableValue {
    String(String),
    Numeric(f64),
    NumericString(String),
}

impl VariableValue {
    pub fn to_numeric(&self) -> Result<f64, ExecutionError> {
        match self {
            VariableValue::String(string) => string
                .parse()
                .map_err(|_| ExecutionError::InvalidNumericLiteral(string.clone())),
            VariableValue::NumericString(string) => string
                .parse()
                .map_err(|_| ExecutionError::InvalidNumericLiteral(string.clone())),
            VariableValue::Numeric(val) => Ok(*val),
        }
    }

    pub fn to_string(&self) -> Result<String, ExecutionError> {
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
    pub fn get_variable(&self, name: &str) -> Result<&VariableValue, ExecutionError> {
        self.variables
            .get(name)
            .ok_or(ExecutionError::NoSuchVariable(String::from(name)))
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

pub struct ProgramExecutor {
    program: Ast,
    engine: ExecutionEngine,
}

impl ProgramExecutor {
    pub fn new(program: Ast, env: Rc<dyn Environment>) -> ProgramExecutor {
        ProgramExecutor {
            program,
            engine: ExecutionEngine::new(env),
        }
    }

    pub fn begin(&mut self) -> Result<(), ExecutionError> {
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

    fn execute_action(&self, action: &Action, record: &Record) -> Result<(), ExecutionError> {
        match action {
            Action::Empty => self
                .engine
                .execute_statements(record, &[Statement::Command(BuiltinCommand::Print, vec![])]),
            Action::Present(statements) => self.engine.execute_statements(record, statements),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{test_utils::TestEnvironment, ExecutionError, ProgramExecutor};
    use crate::parser::ast::{Action, Ast, Pattern, Rule};
    use std::rc::Rc;

    #[test]
    fn test_begin_empty_action() -> Result<(), ExecutionError> {
        let program = Ast {
            rules: vec![Rule {
                pattern: Pattern::Begin,
                action: Action::Empty,
            }],
        };

        let env = Rc::new(TestEnvironment::default());
        let mut executor = ProgramExecutor::new(program, env.clone());
        executor.begin()?;

        assert_eq!(env.printed_lines.borrow().clone(), vec![String::from("\n")]);
        Ok(())
    }
}
