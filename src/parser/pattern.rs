use super::expression::parse_greedy_comma_separated_expressions;
use crate::lexer::Token;
use crate::parser::ast::Pattern;
use crate::parser::parse_error::ParseError;

pub fn parse_pattern(tokens: &[Token]) -> Result<(usize, Pattern), ParseError> {
    match tokens.get(0).ok_or(ParseError::SyntaxError)? {
        Token::BeginKeyword => Ok((1, Pattern::Begin)),
        Token::EndKeyword => Ok((1, Pattern::End)),
        Token::OpenBrace => Ok((0, Pattern::Empty)),
        _ => {
            let (consumed_tokens, expressions) = parse_greedy_comma_separated_expressions(tokens)?;
            Ok((
                consumed_tokens,
                match &expressions[..] {
                    &[ref single_exp] => Pattern::SingleCondition(single_exp.clone()),
                    &[ref range_start, ref range_end] => Pattern::Range(
                        range_start.clone(),
                        range_end.clone(),
                    ),
                    _ => return Err(ParseError::SyntaxError)
                }
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseError, Pattern, Token};
    use crate::parser::ast::{BinOp, Expression, UnOp};

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
            Pattern::SingleCondition(
                Expression::BinaryOperation(
                    BinOp::Assign,
                    Box::new(Expression::VariableValue(String::from("myvar"))),
                    Box::new(Expression::NumericLiteral(2.))
                )
            )
        );
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_range_pattern() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::FieldReference,
            Token::Identifier(String::from("myvar")),
            Token::CompareEquals,
            Token::NumericLiteral(2.),
            Token::Comma,
            Token::RegexLiteral(String::from("foo|bar")),
            Token::OpenBrace
        ];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(
            pattern,
            Pattern::Range(
                Expression::BinaryOperation(
                    BinOp::CompareEquals,
                    Box::new(Expression::UnaryOperation(
                        UnOp::FieldReference,
                        Box::new(Expression::VariableValue(String::from("myvar")))
                    )),
                    Box::new(Expression::NumericLiteral(2.))
                ),
                Expression::RegexLiteral(String::from("foo|bar"))
            )
        );
        assert_eq!(consumed_tokens, 6);

        Ok(())
    }
}
