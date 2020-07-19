use crate::lexer::Token;
use crate::parser::ast::Rule;
use crate::parser::parse_error::ParseError;

use crate::parser::action::parse_action;
use crate::parser::pattern::parse_pattern;
use super::utils::consume_all_statement_separators;

pub fn parse_rule(tokens: &[Token]) -> Result<(usize, Rule), ParseError> {
    let (pattern_consumed_tokens, pattern) = parse_pattern(&tokens)?;
    let (action_consumed_tokens, action) = parse_action(&tokens[pattern_consumed_tokens..])?;
    let consumed_separators = consume_all_statement_separators(&tokens[pattern_consumed_tokens + action_consumed_tokens..]);
    return Ok((
        pattern_consumed_tokens + action_consumed_tokens + consumed_separators,
        Rule { pattern, action },
    ));
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Token,
        parser::{
            ast::{Action, Expression, Pattern, Rule},
            parse_error::ParseError,
        },
    };

    #[test]
    fn test_only_pattern() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::RegexLiteral(String::from("foo"))];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::SingleCondition(Expression::RegexLiteral(String::from("foo"))),
                action: Action::Empty
            }
        );
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_only_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::OpenBrace, Token::CloseBrace];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::Empty,
                action: Action::Present(vec![])
            }
        );
        assert_eq!(consumed_tokens, 2);

        Ok(())
    }

    #[test]
    fn test_full_rule() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::RegexLiteral(String::from("foo")),
            Token::OpenBrace,
            Token::CloseBrace,
        ];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::SingleCondition(Expression::RegexLiteral(String::from("foo"))),
                action: Action::Present(vec![])
            }
        );
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_rule_after_rule() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::RegexLiteral(String::from("foo")),
            Token::OpenBrace,
            Token::CloseBrace,
            Token::RegexLiteral(String::from("bar")),
        ];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::SingleCondition(Expression::RegexLiteral(String::from("foo"))),
                action: Action::Present(vec![])
            }
        );
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_separator_after_rule() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::RegexLiteral(String::from("foo")),
            Token::OpenBrace,
            Token::CloseBrace,
            Token::StatementSeparator,
        ];

        let (consumed_tokens, rule) = super::parse_rule(tokens)?;

        assert_eq!(
            rule,
            Rule {
                pattern: Pattern::SingleCondition(Expression::RegexLiteral(String::from("foo"))),
                action: Action::Present(vec![])
            }
        );
        assert_eq!(consumed_tokens, 4);

        Ok(())
    }
}
