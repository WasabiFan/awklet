use super::{
    ast::{BinOp, UnOp},
    parse_error::ParseError,
};
use crate::lexer::Token;
use crate::parser::ast::Expression;

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
    // RelationalAndRedirection,
    // TODO: How do we handle string concat?
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

    fn coalesce_operator_class(&mut self, class: OpPrecedenceClass) -> Result<(), ParseError> {
        // TODO: associativity direction
        let mut start = 0usize;
        while let Some((operator_position, token)) = self.find_next_op_token_of_class(class, start)
        {
            let preceding_expression = self
                .get_op_tree_entry(operator_position as isize - 1)
                .and_then(|n| {
                    if let PartialAstNode::ParsedExpression(exp) = n {
                        Some(exp)
                    } else {
                        None
                    }
                });
            let following_expression = self
                .get_op_tree_entry(operator_position as isize + 1)
                .and_then(|n| {
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
                    self.op_tree_roots.splice(
                        operator_position - 1..=operator_position + 1,
                        replacement_slice.iter().cloned(),
                    );
                    start = operator_position;
                }
                (None, OperatorOperandType::UnaryPrefix, Some(post)) => {
                    // TODO: nested unary ops, such as '- -1'. Perhaps unary prefixes should parse RTL?
                    let new_expression = Expression::UnaryOperation(
                        UnOp::partial_from_token(token).ok_or(ParseError::SyntaxError)?,
                        Box::new(post.clone()),
                    );

                    let replacement_slice = &[PartialAstNode::ParsedExpression(new_expression)];
                    self.op_tree_roots.splice(
                        operator_position..=operator_position + 1,
                        replacement_slice.iter().cloned(),
                    );
                    start = operator_position + 1;
                }
                (Some(pre), OperatorOperandType::UnaryPostfix, None) => {
                    let new_expression = Expression::UnaryOperation(
                        UnOp::partial_from_token(token).ok_or(ParseError::SyntaxError)?,
                        Box::new(pre.clone()),
                    );

                    let replacement_slice = &[PartialAstNode::ParsedExpression(new_expression)];
                    self.op_tree_roots.splice(
                        operator_position - 1..=operator_position,
                        replacement_slice.iter().cloned(),
                    );
                    start = operator_position;
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
    fn test_binary_assignment_operator() -> Result<(), ParseError> {
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
    fn test_simple_binary_subtraction() -> Result<(), ParseError> {
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
    fn test_assign_binary_subtraction() -> Result<(), ParseError> {
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
    fn test_simple_unary_minus_numeric() -> Result<(), ParseError> {
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
    fn test_simple_unary_minus_variable() -> Result<(), ParseError> {
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
    fn test_minus_negative() -> Result<(), ParseError> {
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
    fn test_double_negative() -> Result<(), ParseError> {
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
    fn test_increment() -> Result<(), ParseError> {
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
    fn test_basic_precedence_1() -> Result<(), ParseError> {
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
    fn test_arithmetic_precedence() -> Result<(), ParseError> {
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
    fn test_complex_precedence() -> Result<(), ParseError> {
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

        // a - ((-($2)) * (foo++))
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
}
