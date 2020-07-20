use std::rc::Rc;
use super::{Closure, Environment, ExecutionError, input::Record, VariableValue, OUTPUT_FIELD_SEPARATOR_NAME, OUTPUT_RECORD_SEPARATOR_NAME};
use crate::parser::ast::{BuiltinCommand, Statement, Expression, UnOp};

use lazy_static::lazy_static;

lazy_static!{
    static ref NO_ARGS_PRINT_SUBSTITUION: Statement = Statement::Command(BuiltinCommand::Print, vec![Expression::UnaryOperation(UnOp::FieldReference, Box::new(Expression::NumericLiteral(0.)))]);
}

pub struct ExecutionEngine {
    env: Rc<dyn Environment>,
    // in awk, all variables are scoped globally (ignoring user functions, which aren't supported).
    _closure: Closure,
}

impl ExecutionEngine {
    pub fn new(env: Rc<dyn Environment>) -> ExecutionEngine {
        ExecutionEngine { env, _closure: Closure::default() }
    }

    pub fn evaluate_expression(&self, record: &Record, expression: &Expression) -> Result<VariableValue, ExecutionError> {
        match expression {
            Expression::UnaryOperation(UnOp::FieldReference, var) => {
                if let Expression::NumericLiteral(n) = var.as_ref() {
                    Ok(record.get_field(*n as usize))
                } else {
                    todo!();
                }
            },
            _ => todo!()
        }
    }

    pub fn execute_statement(&self, record: &Record, statement: &Statement) -> Result<(), ExecutionError> {
        match statement {
            Statement::Command(BuiltinCommand::Print, args) if args.len() == 0 => self.execute_statement(record, &NO_ARGS_PRINT_SUBSTITUION.clone())?,
            Statement::Command(BuiltinCommand::Print, args) => {
                let mut output: Vec<String> = Vec::with_capacity(args.len());
                for expr in args {
                    let val = self.evaluate_expression(record, expr)?.to_string()?;
                    output.push(val);
                } 
                
                let sep = self._closure.get_variable(OUTPUT_FIELD_SEPARATOR_NAME)?.to_string()?;
                let terminator = self._closure.get_variable(OUTPUT_RECORD_SEPARATOR_NAME)?.to_string()?;

                let mut data = output.join(&sep[..]);
                data.push_str(terminator.as_str());

                self.env.print(data.as_str());
            },
            _ => todo!()
        }

        Ok(())
    }

    pub fn execute_statements(&self, record: &Record, statements: &[Statement]) -> Result<(), ExecutionError> {
        for statement in statements {
            self.execute_statement(record, statement)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::ast::{BuiltinCommand, Statement}, executor::{test_utils::TestEnvironment, ExecutionError, input::Record}};
    use super::ExecutionEngine;
    use std::rc::Rc;

    #[test]
    fn test_default_print_command() -> Result<(), ExecutionError> {
        let env = Rc::new(TestEnvironment::default());
        let engine = ExecutionEngine::new(env.clone());
        let record = Record::new(String::from("foo bar"), vec![]);

        engine.execute_statement(&record, &Statement::Command(BuiltinCommand::Print, vec![]))?;
        
        assert_eq!(env.printed_lines.borrow().clone(), vec![String::from("foo bar\n")]);
        Ok(())
    }
}