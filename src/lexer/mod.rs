mod token;
use token::Token;

use regex::Regex;
use lazy_static::lazy_static;

lazy_static! {
    // TODO: are literals beginning with dot (e.g., .7) valid?
    static ref NUMERIC_LITERAL_REGEX: Regex = Regex::new("-?\\d+(.\\d*)?(e(\\+|-)?\\d+)?").unwrap();
}

#[derive(Debug)]
pub enum TokenizeError {
    // SyntaxError()
}

fn try_extract_token_at_start<'t>(source: &'t str, token_regex: &Regex) -> Option<&'t str> {
    if let Some(m) = token_regex.find(source) {
        assert_eq!(m.start(), 0);
        Some(m.as_str())
    } else {
        None
    }
}

fn try_consume_numeric_literal(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*NUMERIC_LITERAL_REGEX)?;
    let num = matched_str.parse().ok()?;

    Some((matched_str.len(), Token::NumericLiteral(num)))
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut current_source = source;

    if let Some((num_bytes_consumed, tok)) = try_consume_numeric_literal(&current_source) {
        current_source = &current_source[num_bytes_consumed..];
        tokens.push(tok);
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests;