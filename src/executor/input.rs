use super::VariableValue;

#[derive(Default)]
pub struct Record {
    full_text: String,
    fields: Vec<String>
}

impl Record {
    #[cfg(test)]
    pub fn new(full_text: String, fields: Vec<String>) -> Record {
        Record { full_text, fields }
    }

    pub fn get_field(&self, field: usize) -> VariableValue {
        // TODO: use OFS for output
        match field {
            0 => VariableValue::NumericString(self.full_text.clone()),
            _ => VariableValue::NumericString(self.fields.get(field - 1).map_or(String::from(""), |val| val.clone()))
        }
    }
}