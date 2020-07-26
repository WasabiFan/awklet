use crate::parser::ast::{Action, Ast, BuiltinCommand, Expression, Pattern, Statement};
use engine::ExecutionEngine;
use input::Record;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod engine;
mod input;

const INPUT_FIELD_SEPARATOR_NAME: &str = "FS";
const OUTPUT_FIELD_SEPARATOR_NAME: &str = "OFS";
const INPUT_RECORD_SEPARATOR_NAME: &str = "RS";
const OUTPUT_RECORD_SEPARATOR_NAME: &str = "ORS";

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
    pub fn to_numeric(&self) -> Result<f64, EvaluationError> {
        // TODO: officially, any numeric prefix should be allowable, not just the whole string
        match self {
            VariableValue::String(string) => string
                .parse()
                .map_err(|_| EvaluationError::InvalidNumericLiteral(string.clone())),
            VariableValue::NumericString(num, _) => Ok(*num),
            VariableValue::Numeric(val) => Ok(*val),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            VariableValue::String(string) => string.clone(),
            VariableValue::NumericString(_, string) => string.clone(),
            VariableValue::Numeric(val) => val.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Closure {
    // TODO: variable types
    variables: HashMap<String, VariableValue>,
}

impl Closure {
    pub fn get_variable_or_default(&self, name: &str) -> VariableValue {
        self.get_variable(name)
            .map_or(VariableValue::NumericString(0., String::from("")), |v| {
                v.clone()
            })
    }

    pub fn get_variable(&self, name: &str) -> Option<&VariableValue> {
        self.variables.get(name)
    }

    pub fn set_variable(&mut self, name: &str, value: VariableValue) {
        self.variables.insert(String::from(name), value);
    }
}

impl Default for Closure {
    fn default() -> Self {
        Closure {
            variables: hashmap![
                String::from(INPUT_FIELD_SEPARATOR_NAME) => VariableValue::String(String::from(" ")),
                String::from(OUTPUT_FIELD_SEPARATOR_NAME) => VariableValue::String(String::from(" ")),
                String::from(INPUT_RECORD_SEPARATOR_NAME) => VariableValue::String(String::from("\n")),
                String::from(OUTPUT_RECORD_SEPARATOR_NAME) => VariableValue::String(String::from("\n"))
            ],
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

    pub fn begin(&self) -> Result<(), EvaluationError> {
        let mut record = Record::default();
        for rule in self
            .program
            .rules
            .iter()
            .filter(|r| r.pattern == Pattern::Begin)
        {
            self.execute_action(&rule.action, &mut record)?;
        }

        Ok(())
    }

    fn execute_action(&self, action: &Action, record: &mut Record) -> Result<(), EvaluationError> {
        match action {
            Action::Empty => self
                .engine
                .borrow_mut()
                .execute_statements(record, &[Statement::Command(BuiltinCommand::Print, vec![])]),
            Action::Present(statements) => self
                .engine
                .borrow_mut()
                .execute_statements(record, statements),
        }
    }

    pub fn set_variable(&mut self, name: &str, value: VariableValue) {
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
