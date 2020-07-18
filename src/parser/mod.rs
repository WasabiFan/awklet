mod ast;
mod parse_error;

mod action;
mod expression;
mod pattern;
mod rule;

use crate::lexer::tokenize;
use ast::{Ast, Rule};

use parse_error::ParseError;

use rule::parse_rule;

pub fn parse(source: &str) -> Result<Ast, ParseError> {
    let tokens = tokenize(source).map_err(|err| ParseError::TokenizeError(err))?;
    let mut remaining_tokens = &tokens[..];

    let mut rules: Vec<Rule> = Vec::new();
    while !remaining_tokens.is_empty() {
        let (consumed_tokens, rule) = parse_rule(remaining_tokens)?;
        remaining_tokens = &remaining_tokens[consumed_tokens..];
        rules.push(rule)
    }

    Ok(Ast { rules })
}
