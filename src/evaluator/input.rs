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
        match field {
            0 => VariableValue::NumericString(self.full_text.clone()),
            _ => VariableValue::NumericString(
                self.fields
                    .get(field - 1)
                    .map_or(String::from(""), |val| val.clone()),
            ),
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

#[cfg(test)]
mod tests {
    use super::Record;
    use crate::evaluator::{Closure, EvaluationError, VariableValue};

    #[test]
    fn parse_space_internal_separator() -> Result<(), EvaluationError> {
        let closure = Closure::default();
        let record = Record::parse_from("foo \nbar \t abc", &closure)?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("foo \nbar \t abc"),
                fields: vec![
                    String::from("foo"),
                    String::from("bar"),
                    String::from("abc"),
                ]
            }
        );

        Ok(())
    }

    #[test]
    fn parse_space_trimming() -> Result<(), EvaluationError> {
        let closure = Closure::default();
        let record = Record::parse_from("\t  foo bar abc\n ", &closure)?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("\t  foo bar abc\n "),
                fields: vec![
                    String::from("foo"),
                    String::from("bar"),
                    String::from("abc"),
                ]
            }
        );

        Ok(())
    }

    #[test]
    fn get_full_record() {
        let record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        let field_val = record.get_field(0);
        assert_eq!(
            field_val,
            VariableValue::NumericString(String::from("foo bar"))
        );
    }

    #[test]
    fn get_intermediate_fields() {
        let record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        let field_val_1 = record.get_field(1);
        let field_val_2 = record.get_field(2);
        assert_eq!(
            field_val_1,
            VariableValue::NumericString(String::from("foo"))
        );
        assert_eq!(
            field_val_2,
            VariableValue::NumericString(String::from("bar"))
        );
    }

    #[test]
    fn get_nonexistent_field() {
        let record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        let field_val = record.get_field(3);
        assert_eq!(field_val, VariableValue::NumericString(String::from("")));
    }

    #[test]
    fn update_whole_record() -> Result<(), EvaluationError> {
        let closure = Closure::default();
        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        record.set_field(&closure, 0, VariableValue::String(String::from("abc 123")))?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("abc 123"),
                fields: vec![String::from("abc"), String::from("123"),]
            }
        );

        Ok(())
    }

    #[test]
    fn update_single_field() -> Result<(), EvaluationError> {
        let closure = Closure::default();
        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        record.set_field(&closure, 1, VariableValue::String(String::from("abc")))?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("abc bar"),
                fields: vec![String::from("abc"), String::from("bar"),]
            }
        );

        Ok(())
    }

    #[test]
    fn update_create_next_field() -> Result<(), EvaluationError> {
        let closure = Closure::default();
        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        record.set_field(&closure, 3, VariableValue::String(String::from("abc")))?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("foo bar abc"),
                fields: vec![
                    String::from("foo"),
                    String::from("bar"),
                    String::from("abc"),
                ]
            }
        );

        Ok(())
    }

    #[test]
    fn update_create_gap_field() -> Result<(), EvaluationError> {
        let closure = Closure::default();
        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        record.set_field(&closure, 5, VariableValue::String(String::from("abc")))?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("foo bar   abc"),
                fields: vec![
                    String::from("foo"),
                    String::from("bar"),
                    String::from(""),
                    String::from(""),
                    String::from("abc"),
                ]
            }
        );

        Ok(())
    }

    #[test]
    fn ofs_without_write() -> Result<(), EvaluationError> {
        let mut closure = Closure::default();
        closure.set_variable("OFS", VariableValue::String(String::from(",")));
        let record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        assert_eq!(
            record.get_field(1),
            VariableValue::NumericString(String::from("foo"))
        );

        assert_eq!(
            record,
            Record {
                full_text: String::from("foo bar"),
                fields: vec![String::from("foo"), String::from("bar"),]
            }
        );

        Ok(())
    }

    #[test]
    fn update_whole_record_custom_ofs() -> Result<(), EvaluationError> {
        let mut closure = Closure::default();
        closure.set_variable("OFS", VariableValue::String(String::from(",")));
        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        record.set_field(&closure, 0, VariableValue::String(String::from("abc 123")))?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("abc 123"),
                fields: vec![String::from("abc"), String::from("123"),]
            }
        );

        Ok(())
    }

    #[test]
    fn update_single_field_custom_ofs() -> Result<(), EvaluationError> {
        let mut closure = Closure::default();
        closure.set_variable("OFS", VariableValue::String(String::from(",")));
        let mut record = Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")],
        );

        record.set_field(&closure, 1, VariableValue::String(String::from("abc")))?;

        assert_eq!(
            record,
            Record {
                full_text: String::from("abc,bar"),
                fields: vec![String::from("abc"), String::from("bar"),]
            }
        );

        Ok(())
    }
}
