use super::VariableValue;
use std::iter;

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct Record {
    full_text: String,
    fields: Vec<String>,
}

impl Record {
    pub fn new(full_text: String, fields: Vec<String>) -> Record {
        Record { full_text, fields }
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

    pub fn set_field(&mut self, field: usize, value: VariableValue, field_separator: &str) {
        let fields_idx = field - 1;
        if fields_idx >= self.fields.len() {
            let num_additional_items = fields_idx + 1 - self.fields.len();
            self.fields
                .extend(iter::repeat(String::from("")).take(num_additional_items));
            assert_eq!(self.fields.len(), fields_idx + 1);
        }

        self.fields[fields_idx] = value.to_string();
        self.full_text = self.fields.join(field_separator)
    }
}
