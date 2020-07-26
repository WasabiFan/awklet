use super::{
    input::Record, Closure, Environment, ExecutionError, VariableValue,
    OUTPUT_FIELD_SEPARATOR_NAME, OUTPUT_RECORD_SEPARATOR_NAME,
};
use crate::parser::ast::{BuiltinCommand, Expression, Statement, UnOp};
use std::rc::Rc;

use lazy_static::lazy_static;

lazy_static! {
    static ref NO_ARGS_PRINT_SUBSTITUTION: Statement = Statement::Command(
        BuiltinCommand::Print,
        vec![Expression::UnaryOperation(
            UnOp::FieldReference,
            Box::new(Expression::NumericLiteral(0.))
        )]
    );
}

pub struct ExecutionEngine {
    env: Rc<dyn Environment>,
    // in awk, all variables are scoped globally (ignoring user functions, which aren't supported).
    closure: Closure,
}

impl ExecutionEngine {
    pub fn new(env: Rc<dyn Environment>) -> ExecutionEngine {
        ExecutionEngine {
            env,
            closure: Closure::default(),
        }
    }

    pub fn set_variable(&mut self, name: &str, value: VariableValue) {
        self.closure.set_variable(name, value);
    }

    pub fn get_variable(&mut self, name: &str) -> Result<&VariableValue, ExecutionError> {
        self.closure.get_variable(name)
    }

    fn evaluate_unary_operation(
        &mut self,
        record: &Record,
        op: &UnOp,
        exp: &Expression,
    ) -> Result<VariableValue, ExecutionError> {
        match op {
            UnOp::Decrement => {
                // TODO: arbitrary lvalues
                if let Expression::VariableValue(name) = exp {
                    let new_value =
                        VariableValue::Numeric(self.closure.get_variable(name)?.to_numeric()? - 1.);
                    self.closure.set_variable(name, new_value.clone());
                    Ok(new_value)
                } else {
                    Err(ExecutionError::NonVariableAsLvalue(exp.clone()))
                }
            }
            UnOp::FieldReference => {
                let value = self.evaluate_expression(record, exp)?.to_numeric()?;
                if (value as usize) as f64 == value {
                    Ok(record.get_field(value as usize))
                } else {
                    Err(ExecutionError::InvalidFieldReference(value))
                }
            }
            _ => todo!(),
        }
    }

    pub fn evaluate_expression(
        &mut self,
        record: &Record,
        expression: &Expression,
    ) -> Result<VariableValue, ExecutionError> {
        match expression {
            Expression::NumericLiteral(num) => Ok(VariableValue::Numeric(*num)),
            Expression::UnaryOperation(op, exp) => self.evaluate_unary_operation(record, op, exp),
            _ => panic!("Unimplemented expression: {:?}", expression),
        }
    }

    pub fn execute_statement(
        &mut self,
        record: &Record,
        statement: &Statement,
    ) -> Result<(), ExecutionError> {
        match statement {
            Statement::Command(BuiltinCommand::Print, args) if args.len() == 0 => {
                self.execute_statement(record, &NO_ARGS_PRINT_SUBSTITUTION.clone())?
            }
            Statement::Command(BuiltinCommand::Print, args) => {
                let mut output: Vec<String> = Vec::with_capacity(args.len());
                for expr in args {
                    let val = self.evaluate_expression(record, expr)?.to_string()?;
                    output.push(val);
                }

                let sep = self
                    .closure
                    .get_variable(OUTPUT_FIELD_SEPARATOR_NAME)?
                    .to_string()?;
                let terminator = self
                    .closure
                    .get_variable(OUTPUT_RECORD_SEPARATOR_NAME)?
                    .to_string()?;

                let mut data = output.join(&sep[..]);
                data.push_str(terminator.as_str());

                self.env.print(data.as_str());
            }
            _ => todo!(),
        }

        Ok(())
    }

    pub fn execute_statements(
        &mut self,
        record: &Record,
        statements: &[Statement],
    ) -> Result<(), ExecutionError> {
        for statement in statements {
            self.execute_statement(record, statement)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::ExecutionEngine;
    use crate::{
        executor::{input::Record, test_utils::TestEnvironment, ExecutionError, VariableValue},
        parser::ast::{BuiltinCommand, Expression, Statement, UnOp},
    };
    use std::rc::Rc;

    #[test]
    fn test_default_print_command() -> Result<(), ExecutionError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let record = Record::new(String::from("foo bar"), vec![]);
        engine.execute_statement(&record, &Statement::Command(BuiltinCommand::Print, vec![]))?;

        assert_eq!(env.get_printed_lines(), vec![String::from("foo bar\n")]);
        Ok(())
    }

    #[test]
    fn test_field_reference_basic() -> Result<(), ExecutionError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let record = Record::new(
            String::from("foo bar baz"),
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("baz"),
            ],
        );
        let value = engine.evaluate_expression(
            &record,
            &Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(2.)),
            ),
        )?;

        assert_eq!(value, VariableValue::NumericString(String::from("bar")));
        Ok(())
    }

    #[test]
    fn test_field_reference_nonexistent() -> Result<(), ExecutionError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let record = Record::new(
            String::from("foo bar baz"),
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("baz"),
            ],
        );
        let value = engine.evaluate_expression(
            &record,
            &Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(4.)),
            ),
        )?;

        assert_eq!(value, VariableValue::NumericString(String::from("")));
        Ok(())
    }

    #[test]
    fn test_decrement() -> Result<(), ExecutionError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let record = Record::default();
        engine.set_variable("myvar", VariableValue::Numeric(5.));
        let value = engine.evaluate_expression(
            &record,
            &Expression::UnaryOperation(
                UnOp::Decrement,
                Box::new(Expression::VariableValue(String::from("myvar"))),
            ),
        )?;

        assert_eq!(value, VariableValue::Numeric(4.));
        assert_eq!(engine.get_variable("myvar")?, &VariableValue::Numeric(4.));
        Ok(())
    }
}
