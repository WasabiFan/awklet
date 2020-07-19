use super::{ast::BinOp, parse_error::ParseError};
use crate::lexer::Token;
use crate::parser::ast::Expression;

pub struct PartialAstBuilder {
    op_tree_roots: Vec<PartialAstNode>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum OpPrecedenceClass {
    Assignment = 1,
    // Conditional = 2,
    // LogicalOr = 3,
    // LogicalAnd = 4,
    // ArrayMembership = 5,
    // Matching = 6,
    // RelationalAndRedirection = 7,
    // TODO: How do we handle string concat?
    AddSub = 9,
    MultDivMod = 10,
    UnaryPlusMinusNot = 11,
    // Exponentiation = 12,
    IncrementDecrement = 13,
    FieldReference = 14,
    // Grouping (parens) are highest, but handled separately
}

impl OpPrecedenceClass {
    pub fn iterator_descending() -> impl Iterator<Item = OpPrecedenceClass> {
        [
            OpPrecedenceClass::FieldReference,
            OpPrecedenceClass::IncrementDecrement,
            OpPrecedenceClass::UnaryPlusMinusNot,
            OpPrecedenceClass::MultDivMod,
            OpPrecedenceClass::AddSub,
            OpPrecedenceClass::Assignment,
        ]
        .iter()
        .copied()
    }

    pub fn does_contain(&self, token: &Token) -> bool {
        match (&self, &token) {
            (OpPrecedenceClass::Assignment, Token::AssignEquals) => true,
            // TODO
            _ => false,
        }
    }
}

enum OperatorOperandType {
    Binary,
    UnaryPrefix,
    UnaryPostfix,
}

impl OperatorOperandType {
    pub fn for_class(class: &OpPrecedenceClass) -> OperatorOperandType {
        match class {
            OpPrecedenceClass::Assignment => OperatorOperandType::Binary,
            OpPrecedenceClass::AddSub => OperatorOperandType::Binary,
            OpPrecedenceClass::MultDivMod => OperatorOperandType::Binary,
            OpPrecedenceClass::UnaryPlusMinusNot => OperatorOperandType::UnaryPrefix,
            OpPrecedenceClass::IncrementDecrement => OperatorOperandType::UnaryPostfix,
            OpPrecedenceClass::FieldReference => OperatorOperandType::UnaryPrefix,
        }
    }
}

// TODO: rename this
impl PartialAstBuilder {
    pub fn new() -> PartialAstBuilder {
        PartialAstBuilder {
            op_tree_roots: Vec::new(),
        }
    }

    pub fn add_known_expression(&mut self, expr: Expression) {
        self.op_tree_roots
            .push(PartialAstNode::ParsedExpression(expr));
    }

    pub fn add_operator_token(&mut self, tok: Token) {
        self.op_tree_roots.push(PartialAstNode::OperatorToken(tok));
    }

    pub fn parse(self) -> Result<Expression, ParseError> {
        OperatorParser::parse(self.op_tree_roots)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum PartialAstNode {
    ParsedExpression(Expression),
    OperatorToken(Token),
}

struct OperatorParser {
    op_tree_roots: Vec<PartialAstNode>,
}

impl OperatorParser {
    pub fn parse(op_tree_roots: Vec<PartialAstNode>) -> Result<Expression, ParseError> {
        let mut parser = OperatorParser { op_tree_roots };
        for class in OpPrecedenceClass::iterator_descending() {
            parser.coalesce_operator_class(class)?;
        }

        if parser.op_tree_roots.len() != 1 {
            return Err(ParseError::SyntaxError);
        }

        if let Some(PartialAstNode::ParsedExpression(expr)) = parser.op_tree_roots.get(0) {
            Ok(expr.clone())
        } else {
            Err(ParseError::SyntaxError)
        }
    }

    fn find_next_op_token_of_class(
        &self,
        class: OpPrecedenceClass,
        start: usize,
    ) -> Option<(usize, &Token)> {
        if start >= self.op_tree_roots.len() {
            return None;
        }

        self.op_tree_roots[start..]
            .iter()
            .enumerate()
            .find_map(|node| {
                if let (i, PartialAstNode::OperatorToken(token)) = node {
                    if class.does_contain(token) {
                        Some((i, token))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
    }

    fn get_op_tree_entry(&self, pos: isize) -> Option<&PartialAstNode> {
        if pos < 0 {
            None
        } else {
            self.op_tree_roots.get(pos as usize)
        }
    }

    fn coalesce_operator_class(&mut self, class: OpPrecedenceClass) -> Result<(), ParseError> {
        // TODO: is it ever possible to know we have an error here?
        // TODO: associativity direction
        let mut start = 0usize;
        while let Some((pos, token)) = self.find_next_op_token_of_class(class, start) {
            start = pos + 1;

            let preceding_expression = self.get_op_tree_entry(pos as isize - 1).and_then(|n| {
                if let PartialAstNode::ParsedExpression(exp) = n {
                    Some(exp)
                } else {
                    None
                }
            });
            let following_expression = self.get_op_tree_entry(pos as isize + 1).and_then(|n| {
                if let PartialAstNode::ParsedExpression(exp) = n {
                    Some(exp)
                } else {
                    None
                }
            });

            match (
                preceding_expression,
                OperatorOperandType::for_class(&class),
                following_expression,
            ) {
                (Some(pre), OperatorOperandType::Binary, Some(post)) => {
                    let new_expression = Expression::BinaryOperation(
                        BinOp::partial_from_token(token).ok_or(ParseError::SyntaxError)?,
                        Box::new(pre.clone()),
                        Box::new(post.clone()),
                    );
                    let replacement_slice = &[PartialAstNode::ParsedExpression(new_expression)];
                    self.op_tree_roots
                        .splice(pos - 1..=pos + 1, replacement_slice.iter().cloned());
                }
                // It's possible this operator will be resolved as a different precedence class.
                // For example, we say that unary "-" is only valid if not preceded by an
                // expression; if it were, that should instead be a binary "-", despite the fact
                // that the unary form has higher precedence. Thus, even if we don't match the unary
                // form here, we may later find that the binary form *is* valid.
                _ => (),
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::PartialAstBuilder;
    use crate::{
        lexer::Token,
        parser::{
            ast::{BinOp, Expression},
            parse_error::ParseError,
        },
    };

    #[test]
    fn test_binary_assignment_operator() -> Result<(), ParseError> {
        let mut builder = PartialAstBuilder::new();

        builder.add_known_expression(Expression::VariableValue(String::from("myvar")));
        builder.add_operator_token(Token::AssignEquals);
        builder.add_known_expression(Expression::NumericLiteral(5.));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::NumericLiteral(5.))
            )
        );

        Ok(())
    }
}
