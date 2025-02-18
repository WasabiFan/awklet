use crate::evaluator::Environment;
use std::cell::RefCell;

#[derive(Debug, Default)]
pub struct TestEnvironment {
    printed_lines: RefCell<Vec<String>>,
}

impl TestEnvironment {
    pub fn get_printed_lines(&self) -> Vec<String> {
        self.printed_lines.borrow().clone()
    }
}

impl Environment for TestEnvironment {
    fn print(&self, string: &str) {
        self.printed_lines.borrow_mut().push(String::from(string));
    }
}

macro_rules! spaced_record {
    ($($x:expr),+ $(,)?) => ({
        let strings = vec![$(String::from($x)),+];
        crate::evaluator::record::Record::new(
            strings.join(" "),
            strings
        )
    });
    () => ({
        crate::evaluator::record::Record::default()
    })
}
