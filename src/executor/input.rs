#[derive(Default)]
pub struct Record {
    full_text: String,
    fields: Vec<String>
}

impl Record {
    pub fn get_field(&self, field: usize) -> &str {
        match field {
            0 => &self.full_text[..],
            _ => self.fields.get(field - 1).map_or("", |val| &val[..])
        }
    }
}