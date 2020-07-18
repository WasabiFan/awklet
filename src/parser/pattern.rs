use crate::lexer::Token;
use crate::parser::ast::Pattern;
use crate::parser::parse_error::ParseError;

pub fn parse_pattern(tokens: &[Token]) -> Result<(usize, Pattern), ParseError> {
    match tokens.get(0).ok_or(ParseError::SyntaxError)? {
        Token::BeginKeyword => Ok((1, Pattern::Begin)),
        Token::EndKeyword => Ok((1, Pattern::End)),
        Token::OpenBrace => Ok((0, Pattern::Empty)),
        _ => Err(ParseError::SyntaxError),
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseError, Pattern, Token};

    #[test]
    fn test_begin() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::BeginKeyword];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(pattern, Pattern::Begin,);
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_end() -> Result<(), ParseError> {
        let tokens: &[Token] = &[Token::EndKeyword];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(pattern, Pattern::End,);
        assert_eq!(consumed_tokens, 1);

        Ok(())
    }

    #[test]
    fn test_empty() -> Result<(), ParseError> {
        let tokens: &[Token] = &[
            Token::OpenBrace,
            Token::Identifier(String::from("print")),
            Token::FieldReference,
            Token::NumericLiteral(0.),
            Token::CloseBrace,
        ];

        let (consumed_tokens, pattern) = super::parse_pattern(tokens)?;

        assert_eq!(pattern, Pattern::Empty,);
        assert_eq!(consumed_tokens, 0);

        Ok(())
    }
}
