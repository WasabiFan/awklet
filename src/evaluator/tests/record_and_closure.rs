use crate::evaluator::{closure::Closure, record::Record, EvaluationError, VariableValue};

#[test]
fn parse_space_internal_separator() -> Result<(), EvaluationError> {
    let closure = Closure::default();
    let record = closure.parse_record_from("foo \nbar \t abc")?;

    assert_eq!(
        record,
        Record::new(
            String::from("foo \nbar \t abc"),
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("abc"),
            ]
        )
    );

    Ok(())
}

#[test]
fn parse_space_trimming() -> Result<(), EvaluationError> {
    let closure = Closure::default();
    let record = closure.parse_record_from("\t  foo bar abc\n ")?;

    assert_eq!(
        record,
        Record::new(
            String::from("\t  foo bar abc\n "),
            vec![
                String::from("foo"),
                String::from("bar"),
                String::from("abc"),
            ]
        )
    );

    Ok(())
}

#[test]
fn get_full_record() {
    let record = spaced_record!["foo", "bar"];

    let field_val = record.get_field(0);
    assert_eq!(field_val, VariableValue::String(String::from("foo bar")));
}

#[test]
fn get_intermediate_fields() {
    let record = spaced_record!["foo", "bar"];

    let field_val_1 = record.get_field(1);
    let field_val_2 = record.get_field(2);
    assert_eq!(field_val_1, VariableValue::String(String::from("foo")));
    assert_eq!(field_val_2, VariableValue::String(String::from("bar")));
}

#[test]
fn get_numeric_fields() {
    let record = spaced_record!["foo", "314e-2", "bar"];

    let field_val = record.get_field(2);
    assert_eq!(
        field_val,
        VariableValue::NumericString(3.14, String::from("314e-2"))
    );
}

#[test]
fn get_nonexistent_field() {
    let record = spaced_record!["foo", "bar"];

    let field_val = record.get_field(3);
    assert_eq!(field_val, VariableValue::String(String::from("")));
}

#[test]
fn update_whole_record() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(0, VariableValue::String(String::from("abc 123")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "123"]);

    Ok(())
}

#[test]
fn update_single_field() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(1, VariableValue::String(String::from("abc")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "bar"]);

    Ok(())
}

#[test]
fn update_create_next_field() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(3, VariableValue::String(String::from("abc")))?;

    assert_eq!(closure.get_record(), &spaced_record!["foo", "bar", "abc"]);

    Ok(())
}

#[test]
fn update_create_gap_field() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(5, VariableValue::String(String::from("abc")))?;

    assert_eq!(
        closure.get_record(),
        &spaced_record!["foo", "bar", "", "", "abc"]
    );

    Ok(())
}

#[test]
fn ofs_without_write() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);
    closure.set_variable("OFS", VariableValue::String(String::from(",")));

    assert_eq!(
        closure.get_field(1),
        VariableValue::String(String::from("foo"))
    );

    assert_eq!(closure.get_record(), &spaced_record!["foo", "bar"]);

    Ok(())
}

#[test]
fn update_whole_record_custom_ofs() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_variable("OFS", VariableValue::String(String::from(",")));
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(0, VariableValue::String(String::from("abc 123")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "123"]);

    Ok(())
}

#[test]
fn update_single_field_custom_ofs() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_variable("OFS", VariableValue::String(String::from(",")));
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(1, VariableValue::String(String::from("abc")))?;

    assert_eq!(
        closure.get_record(),
        &Record::new(
            String::from("abc,bar"),
            vec![String::from("abc"), String::from("bar"),]
        )
    );

    Ok(())
}
