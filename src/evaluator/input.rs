use super::{
    Closure, EvaluationError, VariableValue, INPUT_FIELD_SEPARATOR_NAME,
    OUTPUT_FIELD_SEPARATOR_NAME,
};

use regex::Regex;

use lazy_static::lazy_static;
use std::iter;

lazy_static! {
    static ref SPACE_REGEX: Regex = Regex::new("[ \t\n]+").unwrap();
    static ref START_SPACE_REGEX: Regex = Regex::new("^[ \t\n]+").unwrap();
    static ref END_SPACE_REGEX: Regex = Regex::new("[ \t\n]+$").unwrap();
}

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct Record {
    full_text: String,
    fields: Vec<String>,
}

impl Record {
    #[cfg(test)]
    pub fn new(full_text: String, fields: Vec<String>) -> Record {
        Record { full_text, fields }
    }

    fn parse_fields_from(
        full_text: &str,
        closure: &Closure,
    ) -> Result<Vec<String>, EvaluationError> {
        match closure
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

    pub fn parse_from(full_text: &str, closure: &Closure) -> Result<Record, EvaluationError> {
        Ok(Record {
            full_text: String::from(full_text),
            fields: Self::parse_fields_from(full_text, closure)?,
        })
    }

    pub fn get_field(&self, field: usize) -> VariableValue {
        let str_val = match field {
            0 => self.full_text.clone(),
            _ => self
                .fields
                .get(field - 1)
                .map_or(String::from(""), |val| val.clone()),
        };

        if let Ok(num) = str_val.parse() {
            VariableValue::NumericString(num, str_val)
        } else {
            VariableValue::String(str_val)
        }
    }

    pub fn set_field(
        &mut self,
        closure: &Closure,
        field: usize,
        value: VariableValue,
    ) -> Result<(), EvaluationError> {
        let new_value = value.to_string();

        match field {
            0 => {
                let Record { fields, full_text } = Record::parse_from(new_value.as_str(), closure)?;
                self.fields = fields;
                self.full_text = full_text;
            }
            _ => {
                let fields_idx = field - 1;
                if fields_idx >= self.fields.len() {
                    let num_additional_items = fields_idx + 1 - self.fields.len();
                    self.fields
                        .extend(iter::repeat(String::from("")).take(num_additional_items));
                    assert_eq!(self.fields.len(), fields_idx + 1);
                }

                self.fields[fields_idx] = new_value;
                self.full_text = self.fields.join(
                    closure
                        .get_variable_or_default(OUTPUT_FIELD_SEPARATOR_NAME)
                        .to_string()
                        .as_str(),
                )
            }
        }

        Ok(())
    }
}
