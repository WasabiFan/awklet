use crate::evaluator::VariableValue;

#[test]
fn number_string_to_number() {
    let val = VariableValue::String(String::from("12.3"));
    assert_eq!(12.3, val.to_numeric());
}

#[test]
fn partial_prefix_number_to_number() {
    let val = VariableValue::String(String::from("12aa"));
    assert_eq!(12., val.to_numeric());
}

#[test]
fn number_string_single_char_to_number() {
    let val = VariableValue::String(String::from("1"));
    assert_eq!(1., val.to_numeric());
}

#[test]
fn invalid_number_string_to_number() {
    let val = VariableValue::String(String::from("a1"));
    assert_eq!(0., val.to_numeric());
}

#[test]
fn numeric_string() {
    let val = VariableValue::NumericString(2., String::from("notanumber"));
    assert_eq!(2., val.to_numeric());
    assert_eq!("notanumber", val.to_string().as_str());
}
