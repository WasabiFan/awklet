use crate::lexer::Token;
use crate::parser::ast::Rule;
use crate::parser::parse_error::ParseError;

use super::utils::consume_all_statement_separators;
use crate::parser::action::parse_action;
use crate::parser::pattern::parse_pattern;

pub fn parse_rule(tokens: &[Token]) -> Result<(usize, Rule), ParseError> {
    let trimmed_leading_tokens = tokens
        .iter()
        .position(|t| !matches!(t, Token::StatementSeparator))
        .unwrap_or(0);
    let (pattern_consumed_tokens, pattern) = parse_pattern(&tokens[trimmed_leading_tokens..])?;
    let (action_consumed_tokens, action) =
        parse_action(&tokens[trimmed_leading_tokens + pattern_consumed_tokens..])?;
    let consumed_separators = consume_all_statement_separators(
        &tokens[trimmed_leading_tokens + pattern_consumed_tokens + action_consumed_tokens..],
    );

    Ok((
        trimmed_leading_tokens
            + pattern_consumed_tokens
            + action_consumed_tokens
            + consumed_separators,
        Rule { pattern, action },
    ))
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
    fn only_pattern() -> Result<(), ParseError> {
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
    fn only_action() -> Result<(), ParseError> {
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
    fn full_rule() -> Result<(), ParseError> {
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
    fn rule_after_rule() -> Result<(), ParseError> {
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
    fn separator_after_rule() -> Result<(), ParseError> {
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
