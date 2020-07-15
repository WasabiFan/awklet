use crate::lexer::Token;
use crate::parser::ast::Action;
use crate::parser::parse_error::ParseError;

pub fn parse_action(_: &[Token]) -> Result<(usize, Action), ParseError> {
    Ok((0, Action::Empty))
}
