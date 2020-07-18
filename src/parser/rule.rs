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
        },
        Token::EndKeyword => {
            let (consumed_tokens, action) = parse_action(&tokens[1..])?;
            Ok((
                consumed_tokens + 1,
                Rule {
                    pattern: Pattern::End,
                    action: action,
                },
            ))
        },
        Token::OpenBrace => {
            let (consumed_tokens, action) = parse_action(&tokens)?;
            Ok((
                consumed_tokens,
                Rule {
                    pattern: Pattern::Empty,
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

    #[test]
    fn test_end_empty_action() -> Result<(), ParseError> {
        // This is not truly valid in practice, since "$0" has no meaning in a BEGIN block, but it's
        // fine for testing the parser.
        let tokens: &[Token] = &[Token::EndKeyword];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::End,
                action: Action::Empty
            }
        );
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_empty_pattern() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::FieldReference,
            Token::NumericLiteral(0.),
            Token::CloseBrace
        ];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::Empty,
                // TODO: update once action parsing is implemented
                action: Action::Empty
            }
        );
        // TODO: update once action parsing is implemented
        assert_eq!(consumed_tokens, 0);

        Ok(())
    }
}
