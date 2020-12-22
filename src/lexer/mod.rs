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

struct LexerState<'s> {
    source_text: &'s str,
    next_char_idx: usize,
    extracted_tokens: Vec<Token>,
}

enum LexerStepOutput<'s> {
    Complete(Vec<Token>),
    Incomplete(LexerState<'s>),
}

impl LexerState<'_> {
    pub fn with_source<'s>(source_text: &'s str) -> LexerState<'s> {
        let mut lexer = LexerState {
            source_text,
            next_char_idx: 0usize,
            extracted_tokens: Vec::default(),
        };

        lexer.consume_leading_non_token_whitespace();

        lexer
    }

    fn remaining_text(&self) -> &str {
        &self.source_text[self.next_char_idx..]
    }

    fn try_consume_token(&mut self) -> Result<(), TokenizeError> {
        let (num_consumed_chars, token) = try_consume_numeric_literal(self.remaining_text())
            .or_else(|| try_consume_regex_literal(self.remaining_text()))
            .or_else(|| try_consume_statement_separator(self.remaining_text()))
            .or_else(|| try_consume_brace_or_paren(self.remaining_text()))
            .or_else(|| try_consume_comma(self.remaining_text()))
            .or_else(|| try_consume_field_reference(self.remaining_text()))
            .or_else(|| try_consume_relational_comparison(self.remaining_text()))
            .or_else(|| try_consume_math_operator(self.remaining_text()))
            .or_else(|| try_consume_string_literal(self.remaining_text()))
            .or_else(|| try_consume_identifier(self.remaining_text()))
            .ok_or(TokenizeError::SyntaxError)?;

        self.next_char_idx += num_consumed_chars;
        self.extracted_tokens.push(token);

        Ok(())
    }

    fn consume_leading_non_token_whitespace(&mut self) {
        let non_token_whitespace: &[_] = &[' ', '\t'];
        let remaining_text = self.remaining_text();

        if let Some((0, m)) = remaining_text.match_indices(non_token_whitespace).next() {
            self.next_char_idx += m.len();
        }
    }

    pub fn next<'s>(mut self) -> Result<LexerStepOutput<'s>, TokenizeError>
    where
        Self: 's,
    {
        self.try_consume_token()?;
        self.consume_leading_non_token_whitespace();

        if self.remaining_text().is_empty() {
            Ok(LexerStepOutput::Complete(self.extracted_tokens))
        } else {
            Ok(LexerStepOutput::<'s>::Incomplete(self))
        }
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut state = LexerState::with_source(source);
    state.consume_leading_non_token_whitespace();

    loop {
        match state.next()? {
            LexerStepOutput::Complete(tokens) => return Ok(tokens),
            LexerStepOutput::Incomplete(new_state) => state = new_state,
        }
    }
}

#[cfg(test)]
mod tests;
