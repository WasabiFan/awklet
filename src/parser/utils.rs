use crate::lexer::Token;

pub fn consume_all_statement_separators(tokens: &[Token]) -> usize {
    let mut position = 0;
    while let Some(Token::StatementSeparator) = tokens.get(position) {
        position = position + 1;
    }

    position
}
