use super::{
    ast::{BuiltinCommand, Statement},
    expression::parse_expression, utils::consume_all_statement_separators,
};
use crate::lexer::Token;
use crate::parser::ast::Action;
use crate::parser::expression::parse_greedy_comma_separated_expressions;
use crate::parser::parse_error::ParseError;

pub fn parse_action(tokens: &[Token]) -> Result<(usize, Action), ParseError> {
    if let Some(Token::OpenBrace) = tokens.get(0) {
        let (consumed_tokens, statements) = parse_statements(&tokens[1..])?;
        if let Some(Token::CloseBrace) = tokens.get(1 + consumed_tokens) {
            Ok((1 + consumed_tokens + 1, Action::Present(statements)))
        } else {
            Err(ParseError::SyntaxError)
        }
    } else {
        Ok((0, Action::Empty))
    }
}

pub fn parse_statements(tokens: &[Token]) -> Result<(usize, Vec<Statement>), ParseError> {
    let mut position = 0usize;
    let mut statements = Vec::new();
    while let Ok((consumed_tokens, statement)) = parse_statement(&tokens[position..]) {
        statements.push(statement);

        position = position + consumed_tokens;
        position = position + consume_all_statement_separators(&tokens[position..]);
    }

    Ok((position, statements))
}

pub fn parse_statement(tokens: &[Token]) -> Result<(usize, Statement), ParseError> {
    match tokens.get(0) {
        None => Err(ParseError::SyntaxError),
        Some(Token::Identifier(name)) if name == "print" => {
            parse_greedy_comma_separated_expressions(&tokens[1..]).map(|(consumed_tokens, args)| {
                (
                    1 + consumed_tokens,
                    Statement::Command(BuiltinCommand::Print, args),
                )
            })
        }
        _ => parse_expression(tokens).map(|(consumed_tokens, expression)| {
            (consumed_tokens, Statement::Expression(expression))
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Token,
        parser::{
            ast::{Action, BinOp, BuiltinCommand, Expression, Statement, UnOp},
            parse_error::ParseError,
        },
    };

    #[test]
    fn test_empty_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[];

        let (consumed_tokens, action) = super::parse_action(tokens)?;

        assert_eq!(action, Action::Empty);
        assert_eq!(consumed_tokens, 0);

        Ok(())
    }

    #[test]
    fn test_no_statements_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::OpenBrace, Token::CloseBrace];

        let (consumed_tokens, action) = super::parse_action(tokens)?;

        assert_eq!(action, Action::Present(vec![]));
        assert_eq!(consumed_tokens, 2);

        Ok(())
    }

    #[test]
    fn test_single_no_args_command_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::CloseBrace,
        ];

        let (consumed_tokens, action) = super::parse_action(tokens)?;

        assert_eq!(
            action,
            Action::Present(vec![Statement::Command(BuiltinCommand::Print, vec![])])
        );
        assert_eq!(consumed_tokens, 3);

        Ok(())
    }

    #[test]
    fn test_single_multi_args_command_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::StringLiteral(String::from("foo")),
            Token::Comma,
            Token::NumericLiteral(2.),
            Token::CloseBrace,
        ];

        let (consumed_tokens, action) = super::parse_action(tokens)?;

        assert_eq!(
            action,
            Action::Present(vec![Statement::Command(
                BuiltinCommand::Print,
                vec![
                    Expression::StringLiteral(String::from("foo")),
                    Expression::NumericLiteral(2.)
                ]
            )])
        );
        assert_eq!(consumed_tokens, 6);

        Ok(())
    }

    #[test]
    fn test_single_expression_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::NumericLiteral(5.),
            Token::Plus,
            Token::FieldReference,
            Token::NumericLiteral(1.),
            Token::CloseBrace,
        ];

        let (consumed_tokens, action) = super::parse_action(tokens)?;

        assert_eq!(
            action,
            Action::Present(vec![Statement::Expression(Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::BinaryOperation(
                    BinOp::Add,
                    Box::new(Expression::NumericLiteral(5.)),
                    Box::new(Expression::UnaryOperation(
                        UnOp::FieldReference,
                        Box::new(Expression::NumericLiteral(1.)),
                    ))
                ))
            ))])
        );
        assert_eq!(consumed_tokens, 8);

        Ok(())
    }

    #[test]
    fn test_multi_statement_action() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::StatementSeparator,
            Token::Identifier(String::from("myvar")),
            Token::AssignEquals,
            Token::NumericLiteral(2.),
            Token::StatementSeparator,
            Token::Identifier(String::from("print")),
            Token::NumericLiteral(5.),
            Token::CloseBrace,
        ];

        let (consumed_tokens, action) = super::parse_action(tokens)?;

        assert_eq!(
            action,
            Action::Present(vec![
                Statement::Command(BuiltinCommand::Print, vec![]),
                Statement::Expression(Expression::BinaryOperation(
                    BinOp::Assign,
                    Box::new(Expression::VariableValue(String::from("myvar"))),
                    Box::new(Expression::NumericLiteral(2.))
                )),
                Statement::Command(BuiltinCommand::Print, vec![Expression::NumericLiteral(5.)]),
            ])
        );
        assert_eq!(consumed_tokens, 10);

        Ok(())
    }
}
