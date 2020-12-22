use super::{
    ast::{BinOp, UnOp},
    parse_error::ParseError,
};
use crate::lexer::Token;
use crate::parser::ast::Expression;
use std::ops::RangeBounds;

pub struct OperatorHierarchyParser {
    op_tree_roots: Vec<PartialAstNode>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum OpPrecedenceClass {
    Assignment,
    // Conditional,
    // LogicalOr,
    // LogicalAnd,
    // ArrayMembership,
    // Matching,
    RelationalAndRedirection,
    StringConcat,
    AddSub,
    MultDivMod,
    UnaryPlusMinusNot,
    // Exponentiation,
    IncrementDecrement,
    FieldReference,
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
            OpPrecedenceClass::StringConcat,
            OpPrecedenceClass::RelationalAndRedirection,
            OpPrecedenceClass::Assignment,
        ]
        .iter()
        .copied()
    }

    pub fn does_contain(&self, token: &Token) -> bool {
        match (&self, &token) {
            (OpPrecedenceClass::Assignment, Token::AssignEquals) => true,
            (OpPrecedenceClass::Assignment, Token::PlusEquals) => true,
            (OpPrecedenceClass::Assignment, Token::MinusEquals) => true,

            (OpPrecedenceClass::RelationalAndRedirection, Token::LeftCaret) => true,
            (OpPrecedenceClass::RelationalAndRedirection, Token::LessEqual) => true,
            (OpPrecedenceClass::RelationalAndRedirection, Token::CompareEquals) => true,
            (OpPrecedenceClass::RelationalAndRedirection, Token::BangEqual) => true,
            (OpPrecedenceClass::RelationalAndRedirection, Token::RightCaret) => true,
            (OpPrecedenceClass::RelationalAndRedirection, Token::GreaterEqual) => true,

            (OpPrecedenceClass::AddSub, Token::Minus) => true,
            (OpPrecedenceClass::AddSub, Token::Plus) => true,

            (OpPrecedenceClass::MultDivMod, Token::Star) => true,
            (OpPrecedenceClass::MultDivMod, Token::Slash) => true,
            (OpPrecedenceClass::MultDivMod, Token::Mod) => true,

            (OpPrecedenceClass::UnaryPlusMinusNot, Token::Plus) => true,
            (OpPrecedenceClass::UnaryPlusMinusNot, Token::Minus) => true,

            (OpPrecedenceClass::IncrementDecrement, Token::Increment) => true,
            (OpPrecedenceClass::IncrementDecrement, Token::Decrement) => true,

            (OpPrecedenceClass::FieldReference, Token::FieldReference) => true,

            _ => false,
        }
    }
}

enum OperatorOperandType {
    Binary,
    // BinaryConcat is specific to string concatenation, done by putting expressions adjacent to
    // each other. BinaryConcat is special-cased.
    BinaryConcat,
    UnaryPrefix,
    UnaryPostfix,
}

impl OperatorOperandType {
    pub fn for_class(class: &OpPrecedenceClass) -> OperatorOperandType {
        match class {
            OpPrecedenceClass::Assignment => OperatorOperandType::Binary,
            OpPrecedenceClass::RelationalAndRedirection => OperatorOperandType::Binary,
            OpPrecedenceClass::StringConcat => OperatorOperandType::BinaryConcat,
            OpPrecedenceClass::AddSub => OperatorOperandType::Binary,
            OpPrecedenceClass::MultDivMod => OperatorOperandType::Binary,
            OpPrecedenceClass::UnaryPlusMinusNot => OperatorOperandType::UnaryPrefix,
            OpPrecedenceClass::IncrementDecrement => OperatorOperandType::UnaryPostfix,
            OpPrecedenceClass::FieldReference => OperatorOperandType::UnaryPrefix,
        }
    }
}

impl OperatorHierarchyParser {
    pub fn new() -> OperatorHierarchyParser {
        OperatorHierarchyParser {
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
                        Some((i + start, token))
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

    fn get_root_expression_if_present(&self, pos: isize) -> Option<&Expression> {
        self.get_op_tree_entry(pos).and_then(|n| {
            if let PartialAstNode::ParsedExpression(exp) = n {
                Some(exp)
            } else {
                None
            }
        })
    }

    fn replace_roots_with_expression<R>(&mut self, root_range: R, expr: Expression) -> usize
    where
        R: RangeBounds<usize> + Clone,
    {
        let replacement_slice = &[PartialAstNode::ParsedExpression(expr)];
        self.op_tree_roots
            .splice(root_range.clone(), replacement_slice.iter().cloned());

        match &root_range.start_bound() {
            std::ops::Bound::Excluded(&i) => i + 1,
            std::ops::Bound::Included(&i) => i,
            std::ops::Bound::Unbounded => 0,
        }
    }

    fn coalesce_operator_class(&mut self, class: OpPrecedenceClass) -> Result<(), ParseError> {
        match class {
            OpPrecedenceClass::StringConcat => self.concat_adjacent_expressions(),
            _ => self.coalesce_discrete_token_class(class)?,
        }

        Ok(())
    }

    fn concat_adjacent_expressions(&mut self) {
        let mut i = 0usize;
        while i < self.op_tree_roots.len() {
            let window = (self.op_tree_roots.get(i), self.op_tree_roots.get(i + 1));

            if let (
                Some(PartialAstNode::ParsedExpression(first)),
                Some(PartialAstNode::ParsedExpression(second)),
            ) = window
            {
                let expr = Expression::BinaryOperation(
                    BinOp::Concatenate,
                    Box::new(first.clone()),
                    Box::new(second.clone()),
                );

                i = self.replace_roots_with_expression(i..=i + 1, expr)
            } else {
                i += 1;
            }
        }
    }

    fn coalesce_discrete_token_class(
        &mut self,
        class: OpPrecedenceClass,
    ) -> Result<(), ParseError> {
        let mut start = 0usize;
        while let Some((operator_position, token)) = self.find_next_op_token_of_class(class, start)
        {
            let preceding_expression =
                self.get_root_expression_if_present(operator_position as isize - 1);
            let following_expression =
                self.get_root_expression_if_present(operator_position as isize + 1);

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

                    start = self.replace_roots_with_expression(
                        operator_position - 1..=operator_position + 1,
                        new_expression,
                    );
                }
                (None, OperatorOperandType::UnaryPrefix, Some(post)) => {
                    // TODO: nested unary ops, such as '- -1'. Perhaps unary prefixes should parse RTL?
                    let new_expression = Expression::UnaryOperation(
                        UnOp::partial_from_token(token).ok_or(ParseError::SyntaxError)?,
                        Box::new(post.clone()),
                    );

                    start = self.replace_roots_with_expression(
                        operator_position..=operator_position + 1,
                        new_expression,
                    );
                }
                (Some(pre), OperatorOperandType::UnaryPostfix, None) => {
                    let new_expression = Expression::UnaryOperation(
                        UnOp::partial_from_token(token).ok_or(ParseError::SyntaxError)?,
                        Box::new(pre.clone()),
                    );

                    start = self.replace_roots_with_expression(
                        operator_position - 1..=operator_position,
                        new_expression,
                    );
                }
                // It's possible this operator will be resolved as a different precedence class.
                // For example, we say that unary "-" is only valid if not preceded by an
                // expression; if it were, that should instead be a binary "-", despite the fact
                // that the unary form has higher precedence. Thus, even if we don't match the unary
                // form here, we may later find that the binary form *is* valid.
                _ => {
                    start = operator_position + 1;
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::OperatorHierarchyParser;
    use crate::{
        lexer::Token,
        parser::{
            ast::{BinOp, Expression, UnOp},
            parse_error::ParseError,
        },
    };

    #[test]
    fn binary_assignment_operator() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

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

    #[test]
    fn simple_binary_subtraction() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("myvar")));
        builder.add_operator_token(Token::Minus);
        builder.add_known_expression(Expression::NumericLiteral(5.));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Subtract,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::NumericLiteral(5.))
            )
        );

        Ok(())
    }

    #[test]
    fn assign_binary_subtraction() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("myvar")));
        builder.add_operator_token(Token::AssignEquals);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar")));
        builder.add_operator_token(Token::Minus);
        builder.add_known_expression(Expression::NumericLiteral(5.));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar"))),
                Box::new(Expression::BinaryOperation(
                    BinOp::Subtract,
                    Box::new(Expression::VariableValue(String::from("myvar"))),
                    Box::new(Expression::NumericLiteral(5.))
                ))
            )
        );

        Ok(())
    }

    #[test]
    fn simple_unary_minus_numeric() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_operator_token(Token::Minus);
        builder.add_known_expression(Expression::NumericLiteral(5.));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::UnaryOperation(UnOp::Negation, Box::new(Expression::NumericLiteral(5.)))
        );

        Ok(())
    }

    #[test]
    fn simple_unary_minus_variable() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_operator_token(Token::Minus);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar")));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::UnaryOperation(
                UnOp::Negation,
                Box::new(Expression::VariableValue(String::from("myvar")))
            )
        );

        Ok(())
    }

    #[test]
    fn minus_negative() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("myvar1")));
        builder.add_operator_token(Token::Minus);
        builder.add_operator_token(Token::Minus);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar2")));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Subtract,
                Box::new(Expression::VariableValue(String::from("myvar1"))),
                Box::new(Expression::UnaryOperation(
                    UnOp::Negation,
                    Box::new(Expression::VariableValue(String::from("myvar2")))
                ))
            )
        );

        Ok(())
    }

    #[test]
    #[ignore]
    // Double-negative requires special handling of nested ops, which is not yet implemented.
    fn double_negative() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("myvar1")));
        builder.add_operator_token(Token::AssignEquals);
        builder.add_operator_token(Token::Minus);
        builder.add_operator_token(Token::Minus);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar2")));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Assign,
                Box::new(Expression::VariableValue(String::from("myvar1"))),
                Box::new(Expression::UnaryOperation(
                    UnOp::Negation,
                    Box::new(Expression::UnaryOperation(
                        UnOp::Negation,
                        Box::new(Expression::VariableValue(String::from("myvar2")))
                    ))
                ))
            )
        );

        Ok(())
    }

    #[test]
    fn increment() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("myvar1")));
        builder.add_operator_token(Token::PlusEquals);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar2")));
        builder.add_operator_token(Token::Increment);

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::AddAssign,
                Box::new(Expression::VariableValue(String::from("myvar1"))),
                Box::new(Expression::UnaryOperation(
                    UnOp::Increment,
                    Box::new(Expression::VariableValue(String::from("myvar2")))
                ))
            )
        );

        Ok(())
    }

    #[test]
    fn basic_precedence_1() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_operator_token(Token::FieldReference);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar1")));
        builder.add_operator_token(Token::Plus);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar2")));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Add,
                Box::new(Expression::UnaryOperation(
                    UnOp::FieldReference,
                    Box::new(Expression::VariableValue(String::from("myvar1"))),
                )),
                Box::new(Expression::VariableValue(String::from("myvar2")))
            )
        );

        Ok(())
    }

    #[test]
    fn arithmetic_precedence() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("a")));
        builder.add_operator_token(Token::Plus);
        builder.add_known_expression(Expression::VariableValue(String::from("b")));
        builder.add_operator_token(Token::Star);
        builder.add_known_expression(Expression::VariableValue(String::from("c")));
        builder.add_operator_token(Token::Slash);
        builder.add_known_expression(Expression::VariableValue(String::from("d")));
        builder.add_operator_token(Token::Plus);
        builder.add_known_expression(Expression::VariableValue(String::from("e")));

        let parsed_expression = builder.parse()?;

        //((a+((b*c)/d))+e)
        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Add,
                Box::new(Expression::BinaryOperation(
                    BinOp::Add,
                    Box::new(Expression::VariableValue(String::from("a"))),
                    Box::new(Expression::BinaryOperation(
                        BinOp::Divide,
                        Box::new(Expression::BinaryOperation(
                            BinOp::Multiply,
                            Box::new(Expression::VariableValue(String::from("b"))),
                            Box::new(Expression::VariableValue(String::from("c"))),
                        )),
                        Box::new(Expression::VariableValue(String::from("d"))),
                    )),
                )),
                Box::new(Expression::VariableValue(String::from("e")))
            )
        );

        Ok(())
    }

    #[test]
    fn complex_precedence() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("a")));
        builder.add_operator_token(Token::Minus);
        builder.add_operator_token(Token::Minus);
        builder.add_operator_token(Token::FieldReference);
        builder.add_known_expression(Expression::NumericLiteral(2.));
        builder.add_operator_token(Token::Star);
        builder.add_known_expression(Expression::VariableValue(String::from("myvar2")));
        builder.add_operator_token(Token::Increment);

        let parsed_expression = builder.parse()?;

        // a - ((-($2)) * (myvar2++))
        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Subtract,
                Box::new(Expression::VariableValue(String::from("a"))),
                Box::new(Expression::BinaryOperation(
                    BinOp::Multiply,
                    Box::new(Expression::UnaryOperation(
                        UnOp::Negation,
                        Box::new(Expression::UnaryOperation(
                            UnOp::FieldReference,
                            Box::new(Expression::NumericLiteral(2.)),
                        )),
                    )),
                    Box::new(Expression::UnaryOperation(
                        UnOp::Increment,
                        Box::new(Expression::VariableValue(String::from("myvar2"))),
                    ))
                ))
            )
        );

        Ok(())
    }

    #[test]
    fn string_literal_concat() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::StringLiteral(String::from("a")));
        builder.add_known_expression(Expression::StringLiteral(String::from("b")));

        let parsed_expression = builder.parse()?;

        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Concatenate,
                Box::new(Expression::StringLiteral(String::from("a"))),
                Box::new(Expression::StringLiteral(String::from("b"))),
            )
        );

        Ok(())
    }

    #[test]
    fn complex_concat() -> Result<(), ParseError> {
        let mut builder = OperatorHierarchyParser::new();

        builder.add_known_expression(Expression::VariableValue(String::from("a")));
        builder.add_operator_token(Token::Minus);
        builder.add_operator_token(Token::Minus);
        builder.add_operator_token(Token::FieldReference);
        builder.add_known_expression(Expression::NumericLiteral(2.));
        builder.add_known_expression(Expression::VariableValue(String::from("myvar2")));
        builder.add_operator_token(Token::Increment);

        let parsed_expression = builder.parse()?;

        // (a - (-($2))) concat (myvar2++)
        assert_eq!(
            parsed_expression,
            Expression::BinaryOperation(
                BinOp::Concatenate,
                Box::new(Expression::BinaryOperation(
                    BinOp::Subtract,
                    Box::new(Expression::VariableValue(String::from("a"))),
                    Box::new(Expression::UnaryOperation(
                        UnOp::Negation,
                        Box::new(Expression::UnaryOperation(
                            UnOp::FieldReference,
                            Box::new(Expression::NumericLiteral(2.)),
                        )),
                    )),
                )),
                Box::new(Expression::UnaryOperation(
                    UnOp::Increment,
                    Box::new(Expression::VariableValue(String::from("myvar2"))),
                ))
            )
        );

        Ok(())
    }
}
