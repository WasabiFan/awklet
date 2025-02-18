mod token;
pub use token::Token;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    // TODO: are numeric literals beginning with dot (e.g., .7) valid?
    static ref NUMERIC_LITERAL_REGEX: Regex = Regex::new("\\d+(\\.\\d*)?(e(\\+|-)?\\d+)?").unwrap();
    static ref STRING_LITERAL_REGEX: Regex = Regex::new("\"\"|\"([^\"]*(\\\\\")?)*[^\\\\]\"").unwrap();
    static ref REGEX_LITERAL_REGEX: Regex = Regex::new("//|/([^/\\n]*(\\\\/)?)*[^\\\\]/").unwrap();
    static ref IDENTIFIER_REGEX: Regex = Regex::new("[a-zA-Z_][a-zA-Z_\\d]*").unwrap();
    static ref BRACE_OR_PAREN_REGEX: Regex = Regex::new("\\{|\\}|\\(|\\)").unwrap();
    static ref MATH_OPERATOR_REGEX: Regex = Regex::new("\\+\\+|--|\\+=|-=|\\+|-|\\*|/|%|=").unwrap();
    static ref RELATIONAL_COMPARISON_REGEX: Regex = Regex::new("<=|==|!=|>=|<|>").unwrap();
    static ref COMMA_REGEX: Regex = Regex::new(",").unwrap();
    // Statement separator coalesces adjacent separators and eats intermediate newlines
    static ref STATEMENT_SEPARATOR_REGEX: Regex = Regex::new("((\n|;) *)+").unwrap();
    static ref FIELD_REFERENCE_REGEX: Regex = Regex::new("\\$").unwrap();
}

#[derive(Debug)]
pub enum TokenizeError {
    SyntaxError,
}

fn try_extract_token_at_start<'t>(source: &'t str, token_regex: &Regex) -> Option<&'t str> {
    token_regex.find(source).and_then(|m| {
        if m.start() == 0 {
            Some(m.as_str())
        } else {
            None
        }
    })
}

fn try_consume_numeric_literal(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*NUMERIC_LITERAL_REGEX)?;
    let num_consumed_bytes = matched_str.len();

    if let Some(_) = try_consume_identifier(&current_source[num_consumed_bytes..]) {
        return None;
    }

    let num = matched_str.parse().ok()?;
    Some((num_consumed_bytes, Token::NumericLiteral(num)))
}

fn try_consume_string_literal(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*STRING_LITERAL_REGEX)?;
    let num_consumed_bytes = matched_str.len();

    let contained_string = &matched_str[1..num_consumed_bytes - 1];
    let unescaped_string = contained_string.replace("\\\"", "\"");

    Some((num_consumed_bytes, Token::StringLiteral(unescaped_string)))
}

fn try_consume_regex_literal(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*REGEX_LITERAL_REGEX)?;
    let num_consumed_bytes = matched_str.len();

    // We have no good way to distinguish between between regex and e.g. division. A regex cannot
    // continue onto multiple lines, and this can be used for disambiguation so long as there are
    // not multiple division operators on one line.
    let contained_regex = &matched_str[1..num_consumed_bytes - 1];
    let unescaped_regex = contained_regex.replace("\\/", "/");

    Some((num_consumed_bytes, Token::RegexLiteral(unescaped_regex)))
}

fn try_consume_identifier(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*IDENTIFIER_REGEX)?;

    let token = match matched_str {
        "BEGIN" => Token::BeginKeyword,
        "END" => Token::EndKeyword,
        _ => Token::Identifier(String::from(matched_str)),
    };

    Some((matched_str.len(), token))
}

fn try_consume_brace_or_paren(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*BRACE_OR_PAREN_REGEX)?;

    let token = match matched_str {
        "(" => Token::OpenParen,
        ")" => Token::CloseParen,
        "{" => Token::OpenBrace,
        "}" => Token::CloseBrace,
        _ => panic!("Regex matched unexpected token {}", matched_str),
    };

    Some((matched_str.len(), token))
}

fn try_consume_math_operator(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*MATH_OPERATOR_REGEX)?;

    let token = match matched_str {
        "++" => Token::Increment,
        "--" => Token::Decrement,

        "+" => Token::Plus,
        "-" => Token::Minus,
        "*" => Token::Star,
        "/" => Token::Slash,
        "%" => Token::Mod,

        "=" => Token::AssignEquals,
        "+=" => Token::PlusEquals,
        "-=" => Token::MinusEquals,
        _ => panic!("Regex matched unexpected token {}", matched_str),
    };

    Some((matched_str.len(), token))
}

fn try_consume_relational_comparison(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*RELATIONAL_COMPARISON_REGEX)?;

    let token = match matched_str {
        "<" => Token::LeftCaret,
        "<=" => Token::LessEqual,
        "==" => Token::CompareEquals,
        "!=" => Token::BangEqual,
        ">" => Token::RightCaret,
        ">=" => Token::GreaterEqual,
        _ => panic!("Regex matched unexpected token {}", matched_str),
    };

    Some((matched_str.len(), token))
}

fn try_consume_comma(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*COMMA_REGEX)?;
    Some((matched_str.len(), Token::Comma))
}

fn try_consume_statement_separator(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*STATEMENT_SEPARATOR_REGEX)?;
    Some((matched_str.len(), Token::StatementSeparator))
}

fn try_consume_field_reference(current_source: &str) -> Option<(usize, Token)> {
    let matched_str = try_extract_token_at_start(current_source, &*FIELD_REFERENCE_REGEX)?;
    Some((matched_str.len(), Token::FieldReference))
}

fn try_consume_token(current_source: &str) -> Option<(usize, Token)> {
    try_consume_numeric_literal(&current_source)
        .or_else(|| try_consume_regex_literal(current_source))
        .or_else(|| try_consume_statement_separator(current_source))
        .or_else(|| try_consume_brace_or_paren(current_source))
        .or_else(|| try_consume_comma(current_source))
        .or_else(|| try_consume_field_reference(current_source))
        .or_else(|| try_consume_relational_comparison(current_source))
        .or_else(|| try_consume_math_operator(current_source))
        .or_else(|| try_consume_string_literal(current_source))
        .or_else(|| try_consume_identifier(current_source))
}

fn trim_leading_non_token_whitespace(source: &str) -> &str {
    let non_token_whitespace: &[_] = &[' ', '\t'];
    source.trim_start_matches(non_token_whitespace)
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut current_source = trim_leading_non_token_whitespace(source);

    while !current_source.is_empty() {
        let (num_bytes_consumed, tok) =
            try_consume_token(&current_source).ok_or(TokenizeError::SyntaxError)?;

        current_source = &current_source[num_bytes_consumed..];
        tokens.push(tok);

        current_source = trim_leading_non_token_whitespace(current_source)
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests;
