use super::{input::Record, Executor, ExecutionError};
use crate::parser::ast::Action;

impl Executor {
    pub(super) fn execute_action(&self, action: &Action, record: &Record) -> Result<(), ExecutionError> {
        if let Action::Empty = action {
            self.env.print(record.get_field(0));
            return Ok(())
        }
    
        todo!();
    }
}