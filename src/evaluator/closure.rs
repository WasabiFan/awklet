use super::{record::Record, variable::VariableValue, EvaluationError};
use std::collections::HashMap;

use regex::Regex;

use lazy_static::lazy_static;
lazy_static! {
    static ref SPACE_REGEX: Regex = Regex::new("[ \t\n]+").unwrap();
    static ref START_SPACE_REGEX: Regex = Regex::new("^[ \t\n]+").unwrap();
    static ref END_SPACE_REGEX: Regex = Regex::new("[ \t\n]+$").unwrap();
}

pub const INPUT_FIELD_SEPARATOR_NAME: &str = "FS";
pub const OUTPUT_FIELD_SEPARATOR_NAME: &str = "OFS";
pub const INPUT_RECORD_SEPARATOR_NAME: &str = "RS";
pub const OUTPUT_RECORD_SEPARATOR_NAME: &str = "ORS";
pub const NUMBER_FIELDS_NAME: &str = "NF";
pub const NUMBER_RECORDS_NAME: &str = "NR";

#[derive(Debug)]
pub struct Closure {
    variables: HashMap<String, VariableValue>,
    current_record: Record,
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

    pub fn perform_variable_assignment(&mut self, name: &str, value: VariableValue) {
        match name {
            NUMBER_FIELDS_NAME => self.force_update_num_fields(value.to_numeric() as usize),
            _ => (),
        }

        self.set_variable(name, value);
    }

    fn force_update_num_fields(&mut self, new_size: usize) {
        let field_separator = self.get_field_separator();
        self.current_record
            .rebuild_with_expanded_or_truncated_num_fields(new_size, field_separator.as_str());
    }

    fn set_variable(&mut self, name: &str, value: VariableValue) {
        self.variables.insert(String::from(name), value);
    }

    pub fn set_record(&mut self, record: Record) {
        self.set_variable(
            NUMBER_FIELDS_NAME,
            VariableValue::Numeric(record.num_fields() as f64),
        );
        self.current_record = record;
    }

    #[cfg(test)]
    pub fn get_record(&self) -> &Record {
        &self.current_record
    }

    fn parse_fields_from(&self, full_text: &str) -> Result<Vec<String>, EvaluationError> {
        match self
            .get_variable_or_default(INPUT_FIELD_SEPARATOR_NAME)
            .to_string()
            .as_str()
        {
            " " => {
                let data_start = START_SPACE_REGEX.find(full_text).map_or(0, |m| m.end());
                let data_end = END_SPACE_REGEX
                    .find(full_text)
                    .map_or(full_text.len(), |m| m.start());
                let data = &full_text[data_start..data_end];
                Ok(SPACE_REGEX.split(data).map(String::from).collect())
            }
            _ => todo!(),
        }
    }

    pub fn parse_record_from(&self, full_text: &str) -> Result<Record, EvaluationError> {
        Ok(Record::new(
            String::from(full_text),
            self.parse_fields_from(full_text)?,
        ))
    }

    fn get_field_separator(&self) -> String {
        self.get_variable_or_default(OUTPUT_FIELD_SEPARATOR_NAME)
            .to_string()
    }

    pub fn perform_field_assignment(
        &mut self,
        field: usize,
        value: VariableValue,
    ) -> Result<(), EvaluationError> {
        match field {
            0 => {
                self.set_record(self.parse_record_from(value.to_string().as_str())?);
            }
            _ => {
                let field_separator = self.get_field_separator();
                let initial_num_fields = self.current_record.num_fields();
                self.current_record
                    .set_field(field, value, field_separator.as_str());

                if self.current_record.num_fields() > initial_num_fields {
                    self.set_variable(
                        NUMBER_FIELDS_NAME,
                        VariableValue::Numeric(self.current_record.num_fields() as f64),
                    );
                }
            }
        }

        Ok(())
    }

    pub fn get_field(&self, field: usize) -> VariableValue {
        self.current_record.get_field(field)
    }
}

impl Default for Closure {
    fn default() -> Self {
        Closure {
            variables: hashmap![
                String::from(INPUT_FIELD_SEPARATOR_NAME) => VariableValue::String(String::from(" ")),
                String::from(OUTPUT_FIELD_SEPARATOR_NAME) => VariableValue::String(String::from(" ")),
                String::from(INPUT_RECORD_SEPARATOR_NAME) => VariableValue::String(String::from("\n")),
                String::from(OUTPUT_RECORD_SEPARATOR_NAME) => VariableValue::String(String::from("\n")),
                String::from(NUMBER_FIELDS_NAME) => VariableValue::Numeric(0.),
                String::from(NUMBER_RECORDS_NAME) => VariableValue::Numeric(0.),
            ],
            current_record: Record::default(),
        }
    }
}
