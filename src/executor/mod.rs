use crate::parser::ast::{Ast, Pattern};
use std::{rc::Rc, collections::HashMap};
use input::Record;

#[cfg(test)]
mod test_utils;

mod input;
mod action;

#[derive(Debug)]
pub enum ExecutionError {
    NoSuchVariable(String),
}

pub trait Environment {
    fn print(&self, string: &str);
}

pub struct Closure {
    // TODO: variable types
    variables: HashMap<String, String>
}

impl Closure {
    pub fn new() -> Closure {
        Closure { variables: HashMap::new() }
    }

    pub fn get_variable(&self, name: &str) -> Result<&String, ExecutionError> {
        self.variables.get(name).ok_or(ExecutionError::NoSuchVariable(String::from(name)))
    }
}

pub struct Executor {
    env: Rc<dyn Environment>,
    root_closure: Closure,
    program: Ast,
}

impl Executor {
    pub fn new(program: Ast, env: Rc<dyn Environment>) -> Executor {
        Executor { program, env, root_closure: Closure::new() }
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
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Rule, Ast, Pattern, Action};
    use super::{Executor, ExecutionError, test_utils::TestEnvironment};
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
        let mut executor = Executor::new(program, env.clone());
        executor.begin()?;

        assert_eq!(env.printed_lines.borrow().clone(), vec![String::from("")]);
        Ok(())
    }
}