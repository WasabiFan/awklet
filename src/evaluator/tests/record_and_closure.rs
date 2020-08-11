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
fn update_whole_record_string() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(0, VariableValue::String(String::from("abc 123")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "123"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(2.)
    );

    Ok(())
}

#[test]
fn update_whole_record_different_size() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(0, VariableValue::String(String::from("foo bar baz")))?;

    assert_eq!(closure.get_record(), &spaced_record!["foo", "bar", "baz"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(3.)
    );

    Ok(())
}

#[test]
fn update_whole_record_same_size_updates_nf() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);
    closure.perform_variable_assignment("NF", VariableValue::String(String::from("2foo")));

    closure.perform_field_assignment(0, VariableValue::String(String::from("abc 123")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "123"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(2.)
    );

    Ok(())
}

#[test]
fn update_single_field() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(1, VariableValue::String(String::from("abc")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "bar"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(2.)
    );

    Ok(())
}

#[test]
fn update_single_field_preserves_nf() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);
    closure.perform_variable_assignment("NF", VariableValue::String(String::from("2foo")));

    closure.perform_field_assignment(1, VariableValue::String(String::from("abc")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "bar"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::String(String::from("2foo"))
    );

    Ok(())
}

#[test]
fn update_create_next_field() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(3, VariableValue::String(String::from("abc")))?;

    assert_eq!(closure.get_record(), &spaced_record!["foo", "bar", "abc"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(3.)
    );

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
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(5.)
    );

    Ok(())
}

#[test]
fn ofs_without_write() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(spaced_record!["foo", "bar"]);
    closure.perform_variable_assignment("OFS", VariableValue::String(String::from(",")));

    assert_eq!(
        closure.get_field(1),
        VariableValue::String(String::from("foo"))
    );

    assert_eq!(closure.get_record(), &spaced_record!["foo", "bar"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(2.)
    );

    Ok(())
}

#[test]
fn update_whole_record_custom_ofs() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.perform_variable_assignment("OFS", VariableValue::String(String::from(",")));
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(0, VariableValue::String(String::from("abc 123")))?;

    assert_eq!(closure.get_record(), &spaced_record!["abc", "123"]);
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(2.)
    );

    Ok(())
}

#[test]
fn update_single_field_custom_ofs() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.perform_variable_assignment("OFS", VariableValue::String(String::from(",")));
    closure.set_record(spaced_record!["foo", "bar"]);

    closure.perform_field_assignment(1, VariableValue::String(String::from("abc")))?;

    assert_eq!(
        closure.get_record(),
        &Record::new(
            String::from("abc,bar"),
            vec![String::from("abc"), String::from("bar"),]
        )
    );
    assert_eq!(
        closure.get_variable("NF").unwrap(),
        &VariableValue::Numeric(2.)
    );

    Ok(())
}

#[test]
fn assign_to_larger_nf_rebuilds() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(Record::new(
        String::from("foo   bar"),
        vec![String::from("foo"), String::from("bar")],
    ));

    closure.perform_variable_assignment("NF", VariableValue::Numeric(3.));

    assert_eq!(
        closure.get_record(),
        &Record::new(
            String::from("foo bar "),
            vec![String::from("foo"), String::from("bar"), String::from("")]
        )
    );

    Ok(())
}

#[test]
fn assign_to_equal_nf_rebuilds() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(Record::new(
        String::from("foo   bar"),
        vec![String::from("foo"), String::from("bar")],
    ));

    closure.perform_variable_assignment("NF", VariableValue::Numeric(2.));

    assert_eq!(
        closure.get_record(),
        &Record::new(
            String::from("foo bar"),
            vec![String::from("foo"), String::from("bar")]
        )
    );

    Ok(())
}

#[test]
fn assign_to_smaller_nf_rebuilds() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.set_record(Record::new(
        String::from("foo   bar"),
        vec![String::from("foo"), String::from("bar")],
    ));

    closure.perform_variable_assignment("NF", VariableValue::Numeric(1.));

    assert_eq!(
        closure.get_record(),
        &Record::new(String::from("foo"), vec![String::from("foo")])
    );

    Ok(())
}

#[test]
fn set_record_updates_record() {
    let mut closure = Closure::default();

    closure.set_record(spaced_record!["foo", "bar"]);

    assert_eq!(closure.get_record(), &spaced_record!["foo", "bar"]);
}

#[test]
fn set_record_updates_nf() -> Result<(), EvaluationError> {
    let mut closure = Closure::default();
    closure.perform_variable_assignment("NF", VariableValue::Numeric(3.));

    closure.set_record(spaced_record!["foo", "bar"]);

    assert_eq!(
        closure.get_variable("NF"),
        Some(&VariableValue::Numeric(2.))
    );

    Ok(())
}
