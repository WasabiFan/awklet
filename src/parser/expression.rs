use crate::lexer::Token;
use crate::parser::ast::Expression;
use crate::parser::parse_error::ParseError;

fn parse_greedy_comma_separated_expressions(
    tokens: &[Token],
) -> Result<(usize, Vec<Expression>), ParseError> {
    parse_expression(tokens).map_or_else(
        |_| Ok((0, vec![])),
        |(consumed_tokens, expression)| {
            if let Some(Token::Comma) = tokens.get(consumed_tokens) {
                let (remaining_consumed_tokens, remaining_exprs) =
                    parse_greedy_comma_separated_expressions(&tokens[consumed_tokens + 1..])?;
                let mut exprs = vec![expression];
                exprs.extend(remaining_exprs);
                Ok((consumed_tokens + 1 + remaining_consumed_tokens, exprs))
            } else {
                Ok((consumed_tokens, vec![expression]))
            }
        },
    )
}

pub fn parse_expression(tokens: &[Token]) -> Result<(usize, Expression), ParseError> {
    let next_two = (tokens.get(0).ok_or(ParseError::SyntaxError)?, tokens.get(1));
    match next_two {
        (Token::Identifier(fn_name), Some(Token::OpenParen)) => {
            let remaining_tokens = &tokens[2..];
            let (consumed_tokens, exprs) =
                parse_greedy_comma_separated_expressions(remaining_tokens)?;
            if tokens.get(2 + consumed_tokens) != Some(&Token::CloseParen) {
                return Err(ParseError::SyntaxError);
            }

            Ok((
                2 + consumed_tokens + 1,
                Expression::FunctionCall(fn_name.clone(), exprs),
            ))
        }
        (Token::Identifier(var_name), _) => Ok((1, Expression::VariableValue(var_name.clone()))),
        _ => Err(ParseError::SyntaxError),
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, ParseError, Token};

    #[test]
    fn test_variable() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::Identifier(String::from("foo"))];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(expression, Expression::VariableValue(String::from("foo")));
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_function_call_no_params() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("foo")),
            Token::OpenParen,
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::FunctionCall(String::from("foo"), vec![])
        );
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_function_call_one_var_param() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("foo")),
            Token::OpenParen,
            Token::Identifier(String::from("my_var_1")),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::FunctionCall(
                String::from("foo"),
                vec![Expression::VariableValue(String::from("my_var_1"))]
            )
        );
        assert_eq!(consumed_tokens, 4);

        Ok(())
    }

    #[test]
    fn test_function_call_two_var_param() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("foo")),
            Token::OpenParen,
            Token::Identifier(String::from("my_var_1")),
            Token::Comma,
            Token::Identifier(String::from("my_var_2")),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::FunctionCall(
                String::from("foo"),
                vec![
                    Expression::VariableValue(String::from("my_var_1")),
                    Expression::VariableValue(String::from("my_var_2"))
                ]
            )
        );
        assert_eq!(consumed_tokens, 6);

        Ok(())
    }
}
