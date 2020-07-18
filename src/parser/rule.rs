use crate::lexer::Token;
use crate::parser::ast::Rule;
use crate::parser::parse_error::ParseError;

use crate::parser::action::parse_action;
use crate::parser::pattern::parse_pattern;

pub fn parse_rule(tokens: &[Token]) -> Result<(usize, Rule), ParseError> {
    let (pattern_consumed_tokens, pattern) = parse_pattern(&tokens)?;
    let (action_consumed_tokens, action) = parse_action(&tokens[pattern_consumed_tokens..])?;
    return Ok((
        pattern_consumed_tokens + action_consumed_tokens,
        Rule { pattern, action },
    ));
}
