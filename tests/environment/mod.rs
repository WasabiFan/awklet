use awklet::evaluator::Environment;
use std::cell::RefCell;

#[derive(Debug, Default)]
pub struct IntegrationTestEnvironment {
    output: RefCell<String>,
}

impl IntegrationTestEnvironment {
    pub fn get_output(&self) -> String {
        self.output.borrow().clone()
    }
}

impl Environment for IntegrationTestEnvironment {
    fn print(&self, string: &str) {
        self.output.borrow_mut().push_str(string);
    }
}
