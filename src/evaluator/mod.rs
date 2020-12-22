use crate::parser::ast::{Action, Ast, BuiltinCommand, Expression, Pattern, Statement};
use closure::{INPUT_RECORD_SEPARATOR_NAME, NUMBER_RECORDS_NAME};
use engine::ExecutionEngine;
use record::Record;
use std::{cell::RefCell, ops::Range, rc::Rc};
use variable::VariableValue;

mod closure;
mod engine;
mod record;
mod variable;

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    NoSuchVariable(String),
    NoSuchFunction(String),
    InvalidNumericLiteral(String),
    NonVariableAsLvalue(Expression),
    InvalidFieldReference(f64),
}

#[derive(Debug, PartialEq)]
pub enum RecordProcessingError {
    Evaluation(EvaluationError),
    ConsumeRecord(EvaluationError),
}

pub trait Environment {
    fn print(&self, string: &str);
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

    fn find_next_separator(&self, source: &str) -> Result<Option<Range<usize>>, EvaluationError> {
        let record_sep = self.get_variable(INPUT_RECORD_SEPARATOR_NAME)?.to_string();
        match record_sep.len() {
            0 => {
                // Special case: "one or more blank lines"
                todo!();
            }
            1 => {
                let sep_start = source.find(record_sep.as_str());
                Ok(sep_start.map(|start| start..start + 1))
            }
            _ => {
                // In POSIX awk, unsupported; in gawk, a regex.
                todo!();
            }
        }
    }

    pub fn consume_next_record(
        &self,
        source: &str,
    ) -> Result<Option<(usize, Record)>, EvaluationError> {
        if source.is_empty() {
            return Ok(None);
        }

        let range = self.find_next_separator(source)?.unwrap_or(Range {
            start: source.len(),
            end: source.len(),
        });
        let record_text = &source[0..range.start];
        let consumed_chars = range.end;

        Ok(Some((
            consumed_chars,
            self.engine.borrow().parse_record_from(record_text)?,
        )))
    }

    pub fn consume_and_process_all(&self, source: &str) -> Result<(), RecordProcessingError> {
        let mut remaining_source = source;
        while let Some((consumed_chars, record)) = self
            .consume_next_record(remaining_source)
            .map_err(RecordProcessingError::ConsumeRecord)?
        {
            self.process_record(record)
                .map_err(RecordProcessingError::Evaluation)?;
            remaining_source = &remaining_source[consumed_chars..];
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

    pub fn get_variable(&self, name: &str) -> Result<VariableValue, EvaluationError> {
        Ok(self
            .engine
            .borrow()
            .get_variable(name)
            .ok_or_else(|| EvaluationError::NoSuchVariable(String::from(name)))?
            .clone())
    }
}

#[cfg(test)]
mod tests;
