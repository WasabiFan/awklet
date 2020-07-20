use crate::parser::ast::{Ast, Pattern, Action, BuiltinCommand, Statement};
use std::{rc::Rc, collections::HashMap};
use input::Record;
use engine::ExecutionEngine;

#[cfg(test)]
mod test_utils;

mod engine;
mod input;

#[derive(Debug)]
pub enum ExecutionError {
    NoSuchVariable(String),
}

pub trait Environment {
    fn print(&self, string: &str);
}

#[derive(Debug, Default)]
pub struct Closure {
    // TODO: variable types
    variables: HashMap<String, String>
}

impl Closure {
    pub fn get_variable(&self, name: &str) -> Result<&String, ExecutionError> {
        self.variables.get(name).ok_or(ExecutionError::NoSuchVariable(String::from(name)))
    }
}

pub struct ProgramExecutor {
    program: Ast,
    engine: ExecutionEngine,
}

impl ProgramExecutor {
    pub fn new(program: Ast, env: Rc<dyn Environment>) -> ProgramExecutor {
        ProgramExecutor { program, engine: ExecutionEngine::new(env) }
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
            Action::Empty => {
                self.engine.execute_statements(record, &[
                    Statement::Command(BuiltinCommand::Print, vec![])
                ])
            },
            Action::Present(statements) => self.engine.execute_statements(record, statements)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Rule, Ast, Pattern, Action};
    use super::{ProgramExecutor, ExecutionError, test_utils::TestEnvironment};
    use std::{rc::Rc};

    #[test]
    fn test_begin_empty_action() -> Result<(), ExecutionError> {
        let program = Ast {
            rules: vec![
                Rule {
                    pattern: Pattern::Begin,
                    action: Action::Empty
                }
            ]
        };

        let env = Rc::new(TestEnvironment::default());
        let mut executor = ProgramExecutor::new(program, env.clone());
        executor.begin()?;

        assert_eq!(env.printed_lines.borrow().clone(), vec![String::from("")]);
        Ok(())
    }
}