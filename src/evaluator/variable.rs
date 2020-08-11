use std::cmp::Ordering;

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
