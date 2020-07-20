use std::rc::Rc;
use super::{Closure, Environment, ExecutionError, input::Record};
use crate::parser::ast::{BuiltinCommand, Statement};

pub struct ExecutionEngine {
    env: Rc<dyn Environment>,
    // in awk, all variables are scoped globally (ignoring user functions, which aren't supported).
    _closure: Closure,
}

impl ExecutionEngine {
    pub fn new(env: Rc<dyn Environment>) -> ExecutionEngine {
        ExecutionEngine { env, _closure: Closure::default() }
    }

    pub fn execute_statements(&self, record: &Record, statements: &[Statement]) -> Result<(), ExecutionError> {
        for statement in statements {
            if statement == &Statement::Command(BuiltinCommand::Print, vec![]) {
                self.env.print(record.get_field(0));
            } else {
                todo!();
            }
        }

        Ok(())
    }
}