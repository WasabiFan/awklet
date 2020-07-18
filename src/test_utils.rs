#[macro_export]
macro_rules! assert_matches {
    ($expression:expr, $pattern:pat) => {
        let value = $expression;
        assert!(
            matches!(value, $pattern),
            "{} = {:?}",
            stringify!($expression),
            value
        );
    };
}
