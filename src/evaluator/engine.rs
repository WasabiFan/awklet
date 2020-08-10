use super::{
    closure::{Closure, OUTPUT_FIELD_SEPARATOR_NAME, OUTPUT_RECORD_SEPARATOR_NAME},
    record::Record,
    Environment, EvaluationError, VariableValue,
};
use crate::parser::ast::{BinOp, BuiltinCommand, Expression, Statement, UnOp};
use std::{cmp::Ordering, rc::Rc};

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

    pub fn get_variable(&self, name: &str) -> Option<&VariableValue> {
        self.closure.get_variable(name)
    }

    pub fn set_record(&mut self, record: Record) {
        self.closure.set_record(record);
    }

    #[cfg(test)]
    pub fn get_record(&self) -> &Record {
        self.closure.get_record()
    }

    fn mutate_lvalue<F>(
        &mut self,
        exp: &Expression,
        mutate_fn: F,
    ) -> Result<(VariableValue, VariableValue), EvaluationError>
    where
        F: FnOnce(&VariableValue) -> Result<VariableValue, EvaluationError>,
    {
        match exp {
            Expression::VariableValue(name) => {
                let current_value = self.closure.get_variable_or_default(name);
                let new_value = mutate_fn(&current_value)?;
                self.closure.set_variable(name, new_value.clone());
                Ok((current_value, new_value))
            }
            Expression::UnaryOperation(UnOp::FieldReference, field_spec) => {
                let index = self.resolve_to_field_index(field_spec)?;
                let current_value = self.closure.get_field(index);
                let new_value = mutate_fn(&current_value)?;
                self.closure
                    .perform_field_assignment(index, new_value.clone())?;
                Ok((current_value, new_value))
            }
            _ => Err(EvaluationError::NonVariableAsLvalue(exp.clone())),
        }
    }

    fn mutate_lvalue_get_old<F>(
        &mut self,
        exp: &Expression,
        mutate_fn: F,
    ) -> Result<VariableValue, EvaluationError>
    where
        F: FnOnce(&VariableValue) -> Result<VariableValue, EvaluationError>,
    {
        self.mutate_lvalue(exp, mutate_fn).map(|(old, _)| old)
    }

    fn mutate_lvalue_get_new<F>(
        &mut self,
        exp: &Expression,
        mutate_fn: F,
    ) -> Result<VariableValue, EvaluationError>
    where
        F: FnOnce(&VariableValue) -> Result<VariableValue, EvaluationError>,
    {
        self.mutate_lvalue(exp, mutate_fn).map(|(_, new)| new)
    }

    fn resolve_to_field_index(
        &mut self,
        expression: &Expression,
    ) -> Result<usize, EvaluationError> {
        let value = self.evaluate_expression(expression)?.to_numeric()?;
        if (value as usize) as f64 == value {
            Ok(value as usize)
        } else {
            Err(EvaluationError::InvalidFieldReference(value))
        }
    }

    fn evaluate_unary_operation(
        &mut self,
        op: &UnOp,
        exp: &Expression,
    ) -> Result<VariableValue, EvaluationError> {
        match op {
            UnOp::Increment => self
                .mutate_lvalue_get_old(exp, |existing_value| {
                    Ok(VariableValue::Numeric(existing_value.to_numeric()? + 1.))
                })
                .map(|v| VariableValue::Numeric(v.to_numeric().unwrap())),
            UnOp::Decrement => self
                .mutate_lvalue_get_old(exp, |existing_value| {
                    Ok(VariableValue::Numeric(existing_value.to_numeric()? - 1.))
                })
                .map(|v| VariableValue::Numeric(v.to_numeric().unwrap())),
            UnOp::FieldReference => {
                let index = self.resolve_to_field_index(exp)?;
                Ok(self.closure.get_field(index))
            }
            UnOp::Negation => Ok(VariableValue::Numeric(
                -self.evaluate_expression(exp)?.to_numeric()?,
            )),
        }
    }

    fn evaluate_binary_numeric_op<F>(
        &mut self,
        left: &Expression,
        right: &Expression,
        operation: F,
    ) -> Result<VariableValue, EvaluationError>
    where
        F: Fn(f64, f64) -> f64,
    {
        let left_val = self.evaluate_expression(left)?.to_numeric()?;
        let right_val = self.evaluate_expression(right)?.to_numeric()?;

        Ok(VariableValue::Numeric(operation(left_val, right_val)))
    }

    fn evaluate_relational_op<F>(
        &mut self,
        left: &Expression,
        right: &Expression,
        cmp: F,
    ) -> Result<VariableValue, EvaluationError>
    where
        F: Fn(Ordering) -> bool,
    {
        let left_val = self.evaluate_expression(left)?;
        let right_val = self.evaluate_expression(right)?;

        let ord = match (left_val, right_val) {
            (VariableValue::String(a), VariableValue::String(b)) => a.cmp(&b),
            (VariableValue::String(a), b) => a.cmp(&b.to_string()),
            (a, VariableValue::String(b)) => a.to_string().cmp(&b),
            (a, b) => a
                .to_numeric()?
                .partial_cmp(&b.to_numeric()?)
                .unwrap_or(Ordering::Equal),
        };

        Ok(VariableValue::Numeric(if cmp(ord) { 1. } else { 0. }))
    }

    fn evaluate_binary_operation(
        &mut self,
        op: &BinOp,
        left: &Expression,
        right: &Expression,
    ) -> Result<VariableValue, EvaluationError> {
        match op {
            BinOp::Add => self.evaluate_binary_numeric_op(left, right, |a, b| a + b),
            BinOp::Subtract => self.evaluate_binary_numeric_op(left, right, |a, b| a - b),
            BinOp::Multiply => self.evaluate_binary_numeric_op(left, right, |a, b| a * b),
            BinOp::Divide => self.evaluate_binary_numeric_op(left, right, |a, b| a / b),
            BinOp::Mod => self.evaluate_binary_numeric_op(left, right, |a, b| a % b),
            BinOp::Assign => {
                let new_value = self.evaluate_expression(right)?;
                self.mutate_lvalue_get_new(left, |_| Ok(new_value))
            }
            BinOp::AddAssign => {
                let new_value = self.evaluate_expression(right)?;
                self.mutate_lvalue_get_new(left, |v| {
                    Ok(VariableValue::Numeric(
                        v.to_numeric()? + new_value.to_numeric()?,
                    ))
                })
            }
            BinOp::SubtractAssign => {
                let new_value = self.evaluate_expression(right)?;
                self.mutate_lvalue_get_new(left, |v| {
                    Ok(VariableValue::Numeric(
                        v.to_numeric()? - new_value.to_numeric()?,
                    ))
                })
            }
            BinOp::Concatenate => {
                let left_val = self.evaluate_expression(left)?.to_string();
                let right_val = self.evaluate_expression(right)?.to_string();

                Ok(VariableValue::String(format!("{}{}", left_val, right_val)))
            }
            BinOp::CompareLess => {
                self.evaluate_relational_op(left, right, |ord| ord == Ordering::Less)
            }
            BinOp::CompareLessEquals => self.evaluate_relational_op(left, right, |ord| {
                matches!(ord, Ordering::Less | Ordering::Equal)
            }),
            BinOp::CompareEquals => {
                self.evaluate_relational_op(left, right, |ord| ord == Ordering::Equal)
            }
            BinOp::CompareNotEquals => {
                self.evaluate_relational_op(left, right, |ord| ord != Ordering::Equal)
            }
            BinOp::CompareGreaterEquals => self.evaluate_relational_op(left, right, |ord| {
                matches!(ord, Ordering::Greater | Ordering::Equal)
            }),
            BinOp::CompareGreater => {
                self.evaluate_relational_op(left, right, |ord| ord == Ordering::Greater)
            }
        }
    }

    pub fn evaluate_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<VariableValue, EvaluationError> {
        match expression {
            Expression::NumericLiteral(num) => Ok(VariableValue::Numeric(*num)),
            Expression::StringLiteral(text) => Ok(VariableValue::String(text.clone())),
            Expression::VariableValue(name) => Ok(self.closure.get_variable_or_default(name)),
            Expression::UnaryOperation(op, exp) => self.evaluate_unary_operation(op, exp),
            Expression::BinaryOperation(op, left, right) => {
                self.evaluate_binary_operation(op, left, right)
            }
            Expression::RegexLiteral(_pattern) => todo!(),
            Expression::FunctionCall(_name, _args) => todo!(),
        }
    }

    pub fn execute_statement(&mut self, statement: &Statement) -> Result<(), EvaluationError> {
        match statement {
            Statement::Command(BuiltinCommand::Print, args) if args.len() == 0 => {
                self.execute_statement(&NO_ARGS_PRINT_SUBSTITUTION.clone())?
            }
            Statement::Command(BuiltinCommand::Print, args) => {
                let mut output: Vec<String> = Vec::with_capacity(args.len());
                for expr in args {
                    let val = self.evaluate_expression(expr)?.to_string();
                    output.push(val);
                }

                let sep = self
                    .closure
                    .get_variable_or_default(OUTPUT_FIELD_SEPARATOR_NAME)
                    .to_string();
                let terminator = self
                    .closure
                    .get_variable_or_default(OUTPUT_RECORD_SEPARATOR_NAME)
                    .to_string();

                let mut data = output.join(&sep[..]);
                data.push_str(terminator.as_str());

                self.env.print(data.as_str());
            }
            Statement::Expression(exp) => {
                let _result = self.evaluate_expression(exp)?;
            }
        }

        Ok(())
    }

    pub fn execute_statements(&mut self, statements: &[Statement]) -> Result<(), EvaluationError> {
        for statement in statements {
            self.execute_statement(statement)?;
        }

        Ok(())
    }
}
