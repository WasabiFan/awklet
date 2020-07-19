mod ast;
mod parse_error;

mod utils;

mod action;
mod expression;
mod expression_ops;
mod pattern;
mod rule;

use ast::{Ast, Rule};

use parse_error::ParseError;

use rule::parse_rule;
use crate::lexer::Token;

pub fn parse(tokens: &[Token]) -> Result<Ast, ParseError> {
    let mut remaining_tokens = &tokens[..];

    let mut rules: Vec<Rule> = Vec::new();
    while !remaining_tokens.is_empty() {
        let (consumed_tokens, rule) = parse_rule(remaining_tokens)?;
        remaining_tokens = &remaining_tokens[consumed_tokens..];
        rules.push(rule)
    }

    Ok(Ast { rules })
}

#[cfg(test)]
mod tests {
    use super::{
        ast::{Action, Ast, BinOp, BuiltinCommand, Expression, Pattern, Rule, Statement},
        parse,
        parse_error::ParseError,
    };
    use crate::lexer::tokenize;

    #[test]
    fn test_parse_word_count() -> Result<(), ParseError> {
        let program = "{
            words += NF
            chars += length + 1
        }
        END { print NR, words, chars }";

        let tokens = tokenize(program).unwrap();
        let ast = parse(&tokens[..])?;

        assert_eq!(
            ast,
            Ast {
                rules: vec![
                    Rule {
                        pattern: Pattern::Empty,
                        action: Action::Present(vec![
                            Statement::Expression(Expression::BinaryOperation(
                                BinOp::AddAssign,
                                Box::new(Expression::VariableValue(String::from("words"))),
                                Box::new(Expression::VariableValue(String::from("NF")))
                            )),
                            Statement::Expression(Expression::BinaryOperation(
                                BinOp::AddAssign,
                                Box::new(Expression::VariableValue(String::from("chars"))),
                                Box::new(Expression::BinaryOperation(
                                    BinOp::Add,
                                    Box::new(Expression::VariableValue(String::from("length"))),
                                    Box::new(Expression::NumericLiteral(1.))
                                ))
                            ))
                        ])
                    },
                    Rule {
                        pattern: Pattern::End,
                        action: Action::Present(vec![Statement::Command(
                            BuiltinCommand::Print,
                            vec![
                                Expression::VariableValue(String::from("NR")),
                                Expression::VariableValue(String::from("words")),
                                Expression::VariableValue(String::from("chars")),
                            ]
                        )])
                    }
                ]
            }
        );

        Ok(())
    }
}
