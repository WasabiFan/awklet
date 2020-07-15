use crate::lexer::Token;
use crate::parser::ast::{Pattern, Rule};
use crate::parser::parse_error::ParseError;

use crate::parser::action::parse_action;

pub fn parse_rule(tokens: &[Token]) -> Result<(usize, Rule), ParseError> {
    match tokens.get(0).ok_or(ParseError::SyntaxError)? {
        Token::BeginKeyword => {
            let (consumed_tokens, action) = parse_action(&tokens[1..])?;
            Ok((
                consumed_tokens + 1,
                Rule {
                    pattern: Pattern::Begin,
                    action: action,
                },
            ))
        }
        _ => Err(ParseError::SyntaxError),
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseError, Pattern, Rule, Token};
    use crate::parser::ast::Action;

    #[test]
    fn test_begin_empty_action() -> Result<(), ParseError> {
        // This is not truly valid in practice, since "$0" has no meaning in a BEGIN block, but it's
        // fine for testing the parser.
        let tokens: &[Token] = &[Token::BeginKeyword];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::Begin,
                action: Action::Empty
            }
        );
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }
}
