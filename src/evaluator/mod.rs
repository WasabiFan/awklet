use crate::parser::ast::{Action, Ast, BuiltinCommand, Expression, Pattern, Statement};
use closure::NUMBER_RECORDS_NAME;
use engine::ExecutionEngine;
use record::Record;
use std::{cell::RefCell, cmp::Ordering, rc::Rc};

mod closure;
mod engine;
mod record;

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    NoSuchVariable(String),
    InvalidNumericLiteral(String),
    NonVariableAsLvalue(Expression),
    InvalidFieldReference(f64),
}

pub trait Environment {
    fn print(&self, string: &str);
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableValue {
    String(String),
    Numeric(f64),
    NumericString(f64, String),
}

impl VariableValue {
    fn parse_longest_prefix(value: &str) -> f64 {
        (1..=value.len())
            .rev()
            .find_map(|i| value[..i].parse().ok())
            .unwrap_or(0.)
    }

    pub fn to_numeric(&self) -> f64 {
        match self {
            VariableValue::String(string) => Self::parse_longest_prefix(&string[..]),
            VariableValue::NumericString(num, _) => *num,
            VariableValue::Numeric(val) => *val,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            VariableValue::String(string) => string.clone(),
            VariableValue::NumericString(_, string) => string.clone(),
            VariableValue::Numeric(val) => val.to_string(),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            VariableValue::String(string) => !string.is_empty(),
            VariableValue::NumericString(val, _) => val.partial_cmp(&0.) != Some(Ordering::Equal),
            VariableValue::Numeric(val) => val.partial_cmp(&0.) != Some(Ordering::Equal),
        }
    }
}

pub struct ProgramEvaluator {
    program: Ast,
    engine: RefCell<ExecutionEngine>,
}

impl ProgramEvaluator {
    pub fn new(program: Ast, env: Rc<dyn Environment>) -> ProgramEvaluator {
        ProgramEvaluator {
            program,
            engine: RefCell::new(ExecutionEngine::new(env)),
        }
    }

    fn run_all_with_static_pattern(&self, pat: Pattern) -> Result<(), EvaluationError> {
        for rule in self.program.rules.iter().filter(|r| r.pattern == pat) {
            self.execute_action(&rule.action)?;
        }

        Ok(())
    }

    pub fn begin(&self) -> Result<(), EvaluationError> {
        self.run_all_with_static_pattern(Pattern::Begin)
    }

    pub fn end(&self) -> Result<(), EvaluationError> {
        self.run_all_with_static_pattern(Pattern::End)
    }

    fn pattern_matches_record(&self, pattern: &Pattern) -> Result<bool, EvaluationError> {
        match pattern {
            Pattern::Empty => Ok(true),
            Pattern::SingleCondition(exp) => Ok(self
                .engine
                .borrow_mut()
                .evaluate_expression(&exp)?
                .as_boolean()),
            Pattern::Range(_, _) => todo!(),
            Pattern::Begin => Ok(false),
            Pattern::End => Ok(false),
        }
    }

    fn increment_num_records(&self) -> Result<(), EvaluationError> {
        let old_value = self
            .engine
            .borrow_mut()
            .get_variable(NUMBER_RECORDS_NAME)
            .unwrap_or(&VariableValue::Numeric(0.))
            .to_numeric();
        self.engine
            .borrow_mut()
            .set_variable(NUMBER_RECORDS_NAME, VariableValue::Numeric(old_value + 1.));

        Ok(())
    }

    pub fn process_record(&self, record: Record) -> Result<(), EvaluationError> {
        self.increment_num_records()?;
        self.engine.borrow_mut().set_record(record);

        for rule in self.program.rules.iter() {
            let should_execute = self.pattern_matches_record(&rule.pattern)?;
            if should_execute {
                self.execute_action(&rule.action)?;
            }
        }

        Ok(())
    }

    fn execute_action(&self, action: &Action) -> Result<(), EvaluationError> {
        match action {
            Action::Empty => self
                .engine
                .borrow_mut()
                .execute_statements(&[Statement::Command(BuiltinCommand::Print, vec![])]),
            Action::Present(statements) => self.engine.borrow_mut().execute_statements(statements),
        }
    }

    pub fn set_variable(&self, name: &str, value: VariableValue) {
        self.engine.borrow_mut().set_variable(name, value);
    }

    pub fn get_variable(&mut self, name: &str) -> Result<VariableValue, EvaluationError> {
        Ok(self
            .engine
            .borrow_mut()
            .get_variable(name)
            .ok_or(EvaluationError::NoSuchVariable(String::from(name)))?
            .clone())
    }
}

#[cfg(test)]
mod tests;
