use super::ast::{BinOp, UnOp};
use crate::lexer::Token;
use crate::parser::ast::Expression;
use crate::parser::expression_ops::PartialAstBuilder;
use crate::parser::parse_error::ParseError;

fn parse_greedy_comma_separated_expressions(
    tokens: &[Token],
) -> Result<(usize, Vec<Expression>), ParseError> {
    parse_expression(tokens).map_or_else(
        |_| Ok((0, vec![])),
        |(consumed_tokens, expression)| {
            // TODO: support newline after comma
            // TODO: do we need to know newline vs. semicolon?
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

fn parse_single_expression_unit(tokens: &[Token]) -> Result<(usize, Expression), ParseError> {
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
        (Token::NumericLiteral(val), _) => Ok((1, Expression::NumericLiteral(*val))),
        (Token::FieldReference, _) => {
            let remaining_tokens = &tokens[1..];
            let (consumed_tokens, child_expression) =
                parse_single_expression_unit(&remaining_tokens)?;
            Ok((
                1 + consumed_tokens,
                Expression::UnaryOperation(UnOp::FieldReference, Box::new(child_expression)),
            ))
        }
        (Token::OpenParen, _) => {
            let remaining_tokens = &tokens[1..];
            let (consumed_tokens, inner_expression) = parse_expression(remaining_tokens)?;
            if tokens.get(1 + consumed_tokens) != Some(&Token::CloseParen) {
                return Err(ParseError::SyntaxError);
            }

            Ok((1 + consumed_tokens + 1, inner_expression))
        }
        (Token::StringLiteral(val), _) => Ok((1, Expression::StringLiteral(val.clone()))),
        _ => Err(ParseError::SyntaxError),
    }
}

pub fn parse_expression(tokens: &[Token]) -> Result<(usize, Expression), ParseError> {
    // TODO: we must be able to check whether the next token is an operator
    let mut parser = PartialAstBuilder::new();
    let mut position = 0;
    while position < tokens.len() {
        let remaining_tokens = &tokens[position..];
        if let Ok((consumed_tokens, expression)) = parse_single_expression_unit(remaining_tokens) {
            position = position + consumed_tokens;
            parser.add_known_expression(expression);
        } else if let Some(_) = BinOp::partial_from_token(&remaining_tokens[0]) {
            // TODO: unary op
            position = position + 1;
            parser.add_operator_token(remaining_tokens[0].clone());
        } else {
            break;
        }
    }

    // We can't greedily add any more tokens; try to parse what we've found, and return.
    let expression = parser.parse()?;
    Ok((position, expression))
}

#[cfg(test)]
mod tests {
    use super::{Expression, ParseError, Token};
    use crate::parser::ast::{BinOp, UnOp};

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

    #[test]
    fn test_single_numeric_literal() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::NumericLiteral(5.)];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(expression, Expression::NumericLiteral(5.));
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_single_negative_numeric_literal() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::Minus, Token::NumericLiteral(5.)];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::UnaryOperation(UnOp::Negation, Box::new(Expression::NumericLiteral(5.)))
        );
        assert_eq!(consumed_tokens, 2);

        Ok(())
    }

    #[test]
    fn test_unary_negative_complex() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Minus,
            Token::OpenParen,
            Token::NumericLiteral(5.),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::UnaryOperation(UnOp::Negation, Box::new(Expression::NumericLiteral(5.)))
        );
        assert_eq!(consumed_tokens, 4);

        Ok(())
    }

    #[test]
    fn test_single_item_paren() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenParen,
            Token::NumericLiteral(5.),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(expression, Expression::NumericLiteral(5.));
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_mismatched_paren_group() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::OpenParen, Token::NumericLiteral(5.)];

        assert_matches!(
            super::parse_expression(tokens),
            Err(ParseError::SyntaxError)
        );

        Ok(())
    }

    #[test]
    fn test_mismatched_function_call_parens() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("foo")),
            Token::OpenParen,
            Token::NumericLiteral(5.),
        ];

        assert_matches!(
            super::parse_expression(tokens),
            Err(ParseError::SyntaxError)
        );

        Ok(())
    }

    #[test]
    fn test_single_string_literal() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::StringLiteral(String::from("foobar"))];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::StringLiteral(String::from("foobar"))
        );
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_field_reference_numeric() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::FieldReference, Token::NumericLiteral(2.)];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::UnaryOperation(
                UnOp::FieldReference,
                Box::new(Expression::NumericLiteral(2.))
            )
        );
        assert_eq!(consumed_tokens, 2);

        Ok(())
    }

    #[test]
    fn test_function_call_within_paren() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenParen,
            Token::Identifier(String::from("foo")),
            Token::OpenParen,
            Token::Identifier(String::from("my_var_1")),
            Token::Comma,
            Token::Identifier(String::from("my_var_2")),
            Token::CloseParen,
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
        assert_eq!(consumed_tokens, 8);

        Ok(())
    }

    #[test]
    fn test_complex_function_call() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenParen,
            Token::Identifier(String::from("foo")),
            Token::OpenParen,
            Token::OpenParen,
            Token::Minus,
            Token::NumericLiteral(10.),
            Token::CloseParen,
            Token::Comma,
            Token::FieldReference,
            Token::Identifier(String::from("my_var_2")),
            Token::CloseParen,
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::FunctionCall(
                String::from("foo"),
                vec![
                    Expression::UnaryOperation(
                        UnOp::Negation,
                        Box::new(Expression::NumericLiteral(10.))
                    ),
                    Expression::UnaryOperation(
                        UnOp::FieldReference,
                        Box::new(Expression::VariableValue(String::from("my_var_2")))
                    )
                ]
            )
        );
        assert_eq!(consumed_tokens, 12);

        Ok(())
    }

    #[test]
    fn test_single_binary_operator() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::NumericLiteral(5.),
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::NumericLiteral(5.))
            )
        );

        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_single_binary_operator_multi_unit() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::Identifier(String::from("sqrt")),
            Token::OpenParen,
            Token::NumericLiteral(5.),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::FunctionCall(
                    String::from("sqrt"),
                    vec![Expression::NumericLiteral(5.)]
                ))
            )
        );

        assert_eq!(consumed_tokens, 6);

        Ok(())
    }

    #[test]
    fn test_single_binary_operator_nested_no_assoc() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::OpenParen,
            Token::Identifier(String::from("myvar2")),
            Token::AssignEquals,
            Token::NumericLiteral(5.),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::BinaryOperation(
                    BinOp::Assign,
                    Box::new(Expression::VariableValue(String::from("myvar2"))),
                    Box::new(Expression::NumericLiteral(5.))
                ))
            )
        );

        assert_eq!(consumed_tokens, 7);

        Ok(())
    }
}
