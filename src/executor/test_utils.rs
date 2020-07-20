use super::Environment;
use std::cell::RefCell;

#[derive(Debug, Default)]
pub struct TestEnvironment {
    pub printed_lines: RefCell<Vec<String>>,
}

impl Environment for TestEnvironment {
    fn print(&self, string: &str) {
        self.printed_lines.borrow_mut().push(String::from(string));
    }
}
