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
        self.expand_num_fields(fields_idx + 1);

        self.fields[fields_idx] = value.to_string();
        self.rebuild_fulltext(field_separator);
    }

    pub fn num_fields(&self) -> usize {
        self.fields.len()
    }

    fn rebuild_fulltext(&mut self, field_separator: &str) {
        self.full_text = self.fields.join(field_separator);
    }

    pub fn rebuild_with_expanded_or_truncated_num_fields(
        &mut self,
        new_size: usize,
        field_separator: &str,
    ) {
        self.expand_num_fields(new_size);
        self.truncate_num_fields(new_size);
        self.rebuild_fulltext(field_separator);
    }

    fn expand_num_fields(&mut self, new_size: usize) {
        if new_size > self.fields.len() {
            let num_additional_items = new_size - self.fields.len();
            self.fields
                .extend(iter::repeat(String::from("")).take(num_additional_items));
            assert_eq!(self.fields.len(), new_size);
        }
    }

    fn truncate_num_fields(&mut self, new_size: usize) {
        if new_size < self.fields.len() {
            self.fields.truncate(new_size);
        }
    }
}
