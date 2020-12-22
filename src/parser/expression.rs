use super::ast::{BinOp, UnOp};
use crate::lexer::Token;
use crate::parser::ast::Expression;
use crate::parser::expression_ops::OperatorHierarchyParser;
use crate::parser::parse_error::ParseError;

pub fn parse_greedy_comma_separated_expressions(
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
        (Token::OpenParen, _) => {
            let remaining_tokens = &tokens[1..];
            let (consumed_tokens, inner_expression) = parse_expression(remaining_tokens)?;
            if tokens.get(1 + consumed_tokens) != Some(&Token::CloseParen) {
                return Err(ParseError::SyntaxError);
            }

            Ok((1 + consumed_tokens + 1, inner_expression))
        }
        (Token::StringLiteral(val), _) => Ok((1, Expression::StringLiteral(val.clone()))),
        (Token::RegexLiteral(val), _) => Ok((1, Expression::RegexLiteral(val.clone()))),
        _ => Err(ParseError::SyntaxError),
    }
}

pub fn parse_expression(tokens: &[Token]) -> Result<(usize, Expression), ParseError> {
    let mut parser = OperatorHierarchyParser::new();
    let mut position = 0;
    while position < tokens.len() {
        let remaining_tokens = &tokens[position..];
        if let Ok((consumed_tokens, expression)) = parse_single_expression_unit(remaining_tokens) {
            position += consumed_tokens;
            parser.add_known_expression(expression);
        } else if BinOp::is_valid_op(&remaining_tokens[0])
            || UnOp::is_valid_op(&remaining_tokens[0])
        {
            position += 1;
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
    use matches::assert_matches;

    #[test]
    fn variable() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::Identifier(String::from("foo"))];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(expression, Expression::VariableValue(String::from("foo")));
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn function_call_no_params() -> Result<(), ParseError> {
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
    fn function_call_one_var_param() -> Result<(), ParseError> {
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
    fn function_call_two_var_param() -> Result<(), ParseError> {
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
    fn single_numeric_literal() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::NumericLiteral(5.)];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(expression, Expression::NumericLiteral(5.));
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn single_negative_numeric_literal() -> Result<(), ParseError> {
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
    fn unary_negative_complex() -> Result<(), ParseError> {
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
    fn single_item_paren() -> Result<(), ParseError> {
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
    fn mismatched_paren_group() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::OpenParen, Token::NumericLiteral(5.)];

        assert_matches!(
            super::parse_expression(tokens),
            Err(ParseError::SyntaxError)
        );

        Ok(())
    }

    #[test]
    fn mismatched_function_call_parens() -> Result<(), ParseError> {
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
    fn single_string_literal() -> Result<(), ParseError> {
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
    fn single_regex_literal() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::RegexLiteral(String::from("a.*b"))];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(expression, Expression::RegexLiteral(String::from("a.*b")));
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn field_reference_numeric() -> Result<(), ParseError> {
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
    fn function_call_within_paren() -> Result<(), ParseError> {
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
    fn complex_function_call() -> Result<(), ParseError> {
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
    fn single_binary_operator() -> Result<(), ParseError> {
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
    fn single_binary_operator_multi_unit() -> Result<(), ParseError> {
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
    fn single_binary_operator_nested_no_assoc() -> Result<(), ParseError> {
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

    #[test]
    fn nested_misc() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenParen,
            Token::OpenParen,
            Token::NumericLiteral(2.),
            Token::CloseParen,
            Token::CloseParen,
            Token::Plus,
            Token::NumericLiteral(5.),
            Token::Star,
            Token::Identifier(String::from("somefunc")),
            Token::OpenParen,
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::NumericLiteral(5.),
            Token::Slash,
            Token::Minus,
            Token::NumericLiteral(5.),
            Token::CloseParen,
        ];

        let (consumed_tokens, expression) = super::parse_expression(tokens)?;

        assert_eq!(
            expression,
            Expression::BinaryOperation(
                BinOp::Add,
                Box::new(Expression::NumericLiteral(2.)),
                Box::new(Expression::BinaryOperation(
                    BinOp::Multiply,
                    Box::new(Expression::NumericLiteral(5.)),
                    Box::new(Expression::FunctionCall(
                        String::from("somefunc"),
                        vec![Expression::BinaryOperation(
                            BinOp::Assign,
                            Box::new(Expression::VariableValue(String::from("myvar"))),
                            Box::new(Expression::BinaryOperation(
                                BinOp::Divide,
                                Box::new(Expression::NumericLiteral(5.)),
                                Box::new(Expression::UnaryOperation(
                                    UnOp::Negation,
                                    Box::new(Expression::NumericLiteral(5.))
                                ))
                            ))
                        )]
                    ))
                ))
            )
        );

        assert_eq!(consumed_tokens, 17);

        Ok(())
    }

    #[test]
    fn comma_separated_regex_expression() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::RegexLiteral(String::from("a.*b")),
            Token::Comma,
            Token::FieldReference,
            Token::NumericLiteral(1.),
            Token::CompareEquals,
            Token::StringLiteral(String::from("foo")),
        ];

        let (consumed_tokens, expressions) =
            super::parse_greedy_comma_separated_expressions(tokens)?;

        assert_eq!(
            expressions,
            vec![
                Expression::RegexLiteral(String::from("a.*b")),
                Expression::BinaryOperation(
                    BinOp::CompareEquals,
                    Box::new(Expression::UnaryOperation(
                        UnOp::FieldReference,
                        Box::new(Expression::NumericLiteral(1.))
                    )),
                    Box::new(Expression::StringLiteral(String::from("foo")))
                )
            ]
        );
        assert_eq!(consumed_tokens, 6);

        Ok(())
    }
}
