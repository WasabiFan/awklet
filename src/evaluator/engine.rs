use super::{
    input::Record, Closure, Environment, EvaluationError, VariableValue,
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

    pub fn get_variable(&mut self, name: &str) -> Result<&VariableValue, EvaluationError> {
        self.closure.get_variable(name)
    }

    fn mutate_lvalue<F>(
        &mut self,
        record: &mut Record,
        exp: &Expression,
        mutate_fn: F,
    ) -> Result<VariableValue, EvaluationError>
    where
        F: FnOnce(&VariableValue) -> Result<VariableValue, EvaluationError>,
    {
        match exp {
            Expression::VariableValue(name) => {
                let current_value = self.closure.get_variable(name)?;
                let new_value = mutate_fn(current_value)?;
                self.closure.set_variable(name, new_value.clone());
                Ok(new_value)
            }
            Expression::UnaryOperation(UnOp::FieldReference, field_spec) => {
                let index = self.resolve_to_field_index(record, field_spec)?;
                let current_value = record.get_field(index);
                let new_value = mutate_fn(&current_value)?;
                record.set_field(&self.closure, index, new_value.clone())?;
                Ok(new_value)
            }
            _ => Err(EvaluationError::NonVariableAsLvalue(exp.clone())),
        }
    }

    fn resolve_to_field_index(
        &mut self,
        record: &mut Record,
        expression: &Expression,
    ) -> Result<usize, EvaluationError> {
        let value = self.evaluate_expression(record, expression)?.to_numeric()?;
        if (value as usize) as f64 == value {
            Ok(value as usize)
        } else {
            Err(EvaluationError::InvalidFieldReference(value))
        }
    }

    fn evaluate_unary_operation(
        &mut self,
        record: &mut Record,
        op: &UnOp,
        exp: &Expression,
    ) -> Result<VariableValue, EvaluationError> {
        match op {
            UnOp::Decrement => self.mutate_lvalue(record, exp, |existing_value| {
                Ok(VariableValue::Numeric(existing_value.to_numeric()? - 1.))
            }),
            UnOp::FieldReference => {
                let index = self.resolve_to_field_index(record, exp)?;
                Ok(record.get_field(index))
            }
            _ => todo!(),
        }
    }

    pub fn evaluate_expression(
        &mut self,
        record: &mut Record,
        expression: &Expression,
    ) -> Result<VariableValue, EvaluationError> {
        match expression {
            Expression::NumericLiteral(num) => Ok(VariableValue::Numeric(*num)),
            Expression::UnaryOperation(op, exp) => self.evaluate_unary_operation(record, op, exp),
            _ => panic!("Unimplemented expression: {:?}", expression),
        }
    }

    pub fn execute_statement(
        &mut self,
        record: &mut Record,
        statement: &Statement,
    ) -> Result<(), EvaluationError> {
        match statement {
            Statement::Command(BuiltinCommand::Print, args) if args.len() == 0 => {
                self.execute_statement(record, &NO_ARGS_PRINT_SUBSTITUTION.clone())?
            }
            Statement::Command(BuiltinCommand::Print, args) => {
                let mut output: Vec<String> = Vec::with_capacity(args.len());
                for expr in args {
                    let val = self.evaluate_expression(record, expr)?.to_string();
                    output.push(val);
                }

                let sep = self
                    .closure
                    .get_variable(OUTPUT_FIELD_SEPARATOR_NAME)?
                    .to_string();
                let terminator = self
                    .closure
                    .get_variable(OUTPUT_RECORD_SEPARATOR_NAME)?
                    .to_string();

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
        record: &mut Record,
        statements: &[Statement],
    ) -> Result<(), EvaluationError> {
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
        evaluator::{input::Record, test_utils::TestEnvironment, EvaluationError, VariableValue},
        parser::ast::{BuiltinCommand, Expression, Statement, UnOp},
    };
    use std::rc::Rc;

    #[test]
    fn default_print_command() -> Result<(), EvaluationError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::new(String::from("foo bar"), vec![]);
        engine.execute_statement(
            &mut record,
            &Statement::Command(BuiltinCommand::Print, vec![]),
        )?;

        assert_eq!(env.get_printed_lines(), vec![String::from("foo bar\n")]);
        Ok(())
    }

    #[test]
    fn multi_print() -> Result<(), EvaluationError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );
        engine.execute_statement(
            &mut record,
            &Statement::Command(
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
                    Expression::UnaryOperation(
                        UnOp::FieldReference,
                        Box::new(Expression::NumericLiteral(2.)),
                    ),
                ],
            ),
        )?;

        assert_eq!(
            env.get_printed_lines(),
            vec![String::from("foo bar foo bar\n")]
        );
        Ok(())
    }

    #[test]
    fn field_reference_basic() -> Result<(), EvaluationError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::new(
            String::from("foo bar baz"),
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("baz"),
            ],
        );
        let value = engine.evaluate_expression(
            &mut record,
            &Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(2.)),
            ),
        )?;

        assert_eq!(value, VariableValue::NumericString(String::from("bar")));
        Ok(())
    }

    #[test]
    fn field_reference_nonexistent() -> Result<(), EvaluationError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::new(
            String::from("foo bar baz"),
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("baz"),
            ],
        );
        let value = engine.evaluate_expression(
            &mut record,
            &Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(4.)),
            ),
        )?;

        assert_eq!(value, VariableValue::NumericString(String::from("")));
        Ok(())
    }

    #[test]
    fn decrement() -> Result<(), EvaluationError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::default();
        engine.set_variable("myvar", VariableValue::Numeric(5.));
        let value = engine.evaluate_expression(
            &mut record,
            &Expression::UnaryOperation(
                UnOp::Decrement,
                Box::new(Expression::VariableValue(String::from("myvar"))),
            ),
        )?;

        assert_eq!(value, VariableValue::Numeric(4.));
        assert_eq!(engine.get_variable("myvar")?, &VariableValue::Numeric(4.));
        Ok(())
    }

    #[test]
    fn decrement_field() -> Result<(), EvaluationError> {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::new(
            String::from("some 4 record"),
            vec![
                String::from("some"),
                String::from("4"),
                String::from("record"),
            ],
        );
        let value = engine.evaluate_expression(
            &mut record,
            &Expression::UnaryOperation(
                UnOp::Decrement,
                Box::new(Expression::UnaryOperation(
                    UnOp::FieldReference,
                    Box::new(Expression::NumericLiteral(2.)),
                )),
            ),
        )?;

        assert_eq!(value, VariableValue::Numeric(3.));
        assert_eq!(
            record.get_field(2),
            VariableValue::NumericString(String::from("3"))
        );
        assert_eq!(
            record.get_field(0),
            VariableValue::NumericString(String::from("some 3 record"))
        );
        Ok(())
    }

    #[test]
    fn decrement_string() {
        let env = Rc::new(TestEnvironment::default());
        let mut engine = ExecutionEngine::new(env.clone());

        let mut record = Record::new(
            String::from("some 4 record"),
            vec![
                String::from("some"),
                String::from("4"),
                String::from("record"),
            ],
        );
        let result = engine.evaluate_expression(
            &mut record,
            &Expression::UnaryOperation(
                UnOp::Decrement,
                Box::new(Expression::UnaryOperation(
                    UnOp::FieldReference,
                    Box::new(Expression::NumericLiteral(1.)),
                )),
            ),
        );

        assert_matches!(result, Err(EvaluationError::InvalidNumericLiteral(_)));
    }
}
