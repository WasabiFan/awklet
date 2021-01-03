pub mod ast;
mod parse_error;

mod utils;

mod action;
mod expression;
mod expression_ops;
mod pattern;
mod rule;

use ast::{Ast, Rule};

pub use parse_error::ParseError;

use crate::lexer::Token;
use rule::parse_rule;

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
    use crate::lexer::{tokenize, SpannedToken};

    #[test]
    fn parse_word_count() -> Result<(), ParseError> {
        let program = "{
            words += NF
            chars += length + 1
        }
        END { print NR, words, chars }";

        let tokens = tokenize(program).unwrap();
        // TODO: remove once parser supports ingesting SpannedTokens
        let unspanned_tokens: Vec<_> = tokens.into_iter().map(|SpannedToken(tok, _)| tok).collect();
        let ast = parse(&unspanned_tokens[..])?;

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

    #[test]
    fn parse_leading_newlines() -> Result<(), ParseError> {
        let program = "
        {}";

        let tokens = tokenize(program).unwrap();
        // TODO: remove once parser supports ingesting SpannedTokens
        let unspanned_tokens: Vec<_> = tokens.into_iter().map(|SpannedToken(tok, _)| tok).collect();
        let ast = parse(&unspanned_tokens[..])?;

        assert_eq!(
            ast,
            Ast {
                rules: vec![Rule {
                    pattern: Pattern::Empty,
                    action: Action::Present(vec![])
                },]
            }
        );

        Ok(())
    }
}
