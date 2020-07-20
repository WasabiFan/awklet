use crate::parser::ast::{Ast, Pattern, Action};
use std::{rc::Rc, collections::HashMap};

#[derive(Debug)]
pub enum ExecutionError {
    NoSuchVariable(String),
}

#[derive(Default)]
struct Record {
    full_text: String,
    fields: Vec<String>
}

impl Record {
    pub fn get_field(&self, field: usize) -> &str {
        match field {
            0 => &self.full_text[..],
            _ => self.fields.get(field - 1).map_or("", |val| &val[..])
        }
    }
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

struct Context {
    pub env: Rc<dyn Environment>,
    pub record: Record
}

impl Context {
    pub fn new(env: Rc<dyn Environment>, record: Record) -> Context {
        Context { env: env, record }
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
        let context = Context::new(self.env.clone(), Record::default());
        for rule in self
            .program
            .rules
            .iter()
            .filter(|r| r.pattern == Pattern::Begin)
        {
            execute_action(&rule.action, &context, &self.root_closure)?;
        }

        Ok(())
    }
}

fn execute_action(action: &Action, context: &Context, _closure: &Closure) -> Result<(), ExecutionError> {
    if let Action::Empty = action {
        context.env.print(context.record.get_field(0));
        return Ok(())
    }

    todo!();
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Rule, Ast, Pattern, Action};
    use super::{Environment, Executor, ExecutionError};
    use std::{rc::Rc, cell::RefCell};

    #[derive(Debug, Default)]
    struct TestEnvironment {
        printed_lines: RefCell<Vec<String>>
    }

    impl Environment for TestEnvironment {
        fn print(&self, string: &str) {
            self.printed_lines.borrow_mut().push(String::from(string));
        }
     }

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