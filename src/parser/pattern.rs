use super::{ast::SingleConditionPattern, expression::parse_expression};
use crate::lexer::Token;
use crate::parser::ast::Pattern;
use crate::parser::parse_error::ParseError;

pub fn parse_pattern(tokens: &[Token]) -> Result<(usize, Pattern), ParseError> {
    match tokens.get(0).ok_or(ParseError::SyntaxError)? {
        Token::BeginKeyword => Ok((1, Pattern::Begin)),
        Token::EndKeyword => Ok((1, Pattern::End)),
        Token::OpenBrace => Ok((0, Pattern::Empty)),
        _ => {
            let (consumed_tokens, expression) = parse_expression(tokens)?;
            // TODO: range patterns
            Ok((
                consumed_tokens,
                Pattern::SingleCondition(SingleConditionPattern::Expression(expression)),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseError, Pattern, Token};
    use crate::parser::ast::{BinOp, Expression, SingleConditionPattern};

    #[test]
    fn test_begin() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::BeginKeyword];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(pattern, Pattern::Begin);
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_end() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::EndKeyword];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(pattern, Pattern::End);
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_empty() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::FieldReference,
            Token::NumericLiteral(0.),
            Token::CloseBrace,
        ];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(pattern, Pattern::Empty);
        assert_eq!(consumed_tokens, 0);

        Ok(())
    }

    #[test]
    fn test_single_pattern_assignment() -> Result<(), ParseError> {
        // This is a silly program, but we first implemented assignment, so we'll use it.
        let tokens: &[Token] = &[
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::NumericLiteral(2.),
        ];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(
            pattern,
            Pattern::SingleCondition(SingleConditionPattern::Expression(
                Expression::BinaryOperation(
                    BinOp::Assign,
                    Box::new(Expression::VariableValue(String::from("myvar"))),
                    Box::new(Expression::NumericLiteral(2.))
                )
            ))
        );
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }
}
