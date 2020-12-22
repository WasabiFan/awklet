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

struct TokenConsumeResult {
    pub token: Token,
    pub bytes_consumed: usize,
}

impl TokenConsumeResult {
    pub fn new(token: Token, bytes_consumed: usize) -> TokenConsumeResult {
        TokenConsumeResult {
            token,
            bytes_consumed,
        }
    }
}

fn try_extract_token_at_start<'l, 't>(
    source: &'l LexerSource<'t>,
    token_regex: &Regex,
) -> Option<usize> {
    token_regex.find(source.remaining_text()).and_then(|m| {
        if m.start() == 0 {
            Some(m.end())
        } else {
            None
        }
    })
}

fn try_consume_numeric_literal(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let num_consumed_bytes = try_extract_token_at_start(&source, &*NUMERIC_LITERAL_REGEX)?;
    let matched_str = source.following_text(num_consumed_bytes);

    let remaining_source = source.clone_advance_by(num_consumed_bytes);
    if try_consume_identifier(&remaining_source).is_some() {
        return None;
    }

    let num = matched_str.parse().ok()?;
    Some(TokenConsumeResult::new(
        Token::NumericLiteral(num),
        num_consumed_bytes,
    ))
}

fn try_consume_string_literal(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let num_consumed_bytes = try_extract_token_at_start(&source, &*STRING_LITERAL_REGEX)?;
    let matched_str = source.following_text(num_consumed_bytes);

    let contained_string = &matched_str[1..num_consumed_bytes - 1];
    let unescaped_string = contained_string.replace("\\\"", "\"");

    Some(TokenConsumeResult::new(
        Token::StringLiteral(unescaped_string),
        num_consumed_bytes,
    ))
}

fn try_consume_regex_literal(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let num_consumed_bytes = try_extract_token_at_start(&source, &*REGEX_LITERAL_REGEX)?;
    let matched_str = source.following_text(num_consumed_bytes);

    // We have no good way to distinguish between between regex and e.g. division. A regex cannot
    // continue onto multiple lines, and this can be used for disambiguation so long as there are
    // not multiple division operators on one line. The regex will only match intra-line pairs.

    let contained_regex = &matched_str[1..num_consumed_bytes - 1];
    let unescaped_regex = contained_regex.replace("\\/", "/");

    Some(TokenConsumeResult::new(
        Token::RegexLiteral(unescaped_regex),
        num_consumed_bytes,
    ))
}

fn try_consume_identifier(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(&source, &*IDENTIFIER_REGEX)?;

    let token = match source.following_text(len) {
        "BEGIN" => Token::BeginKeyword,
        "END" => Token::EndKeyword,
        s => Token::Identifier(String::from(s)),
    };

    Some(TokenConsumeResult::new(token, len))
}

fn try_consume_brace_or_paren(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(&source, &*BRACE_OR_PAREN_REGEX)?;

    let token = match source.following_text(len) {
        "(" => Token::OpenParen,
        ")" => Token::CloseParen,
        "{" => Token::OpenBrace,
        "}" => Token::CloseBrace,
        s => panic!("Regex matched unexpected token {}", s),
    };

    Some(TokenConsumeResult::new(token, len))
}

fn try_consume_math_operator(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(&source, &*MATH_OPERATOR_REGEX)?;

    let token = match source.following_text(len) {
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
        s => panic!("Regex matched unexpected token {}", s),
    };

    Some(TokenConsumeResult::new(token, len))
}

fn try_consume_relational_comparison(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(&source, &*RELATIONAL_COMPARISON_REGEX)?;

    let token = match source.following_text(len) {
        "<" => Token::LeftCaret,
        "<=" => Token::LessEqual,
        "==" => Token::CompareEquals,
        "!=" => Token::BangEqual,
        ">" => Token::RightCaret,
        ">=" => Token::GreaterEqual,
        s => panic!("Regex matched unexpected token {}", s),
    };

    Some(TokenConsumeResult::new(token, len))
}

fn try_consume_comma(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(&source, &*COMMA_REGEX)?;
    Some(TokenConsumeResult::new(Token::Comma, len))
}

fn try_consume_statement_separator(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(&source, &*STATEMENT_SEPARATOR_REGEX)?;
    Some(TokenConsumeResult::new(Token::StatementSeparator, len))
}

fn try_consume_field_reference(source: &LexerSource<'_>) -> Option<TokenConsumeResult> {
    let len = try_extract_token_at_start(source, &*FIELD_REFERENCE_REGEX)?;
    Some(TokenConsumeResult::new(Token::FieldReference, len))
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct LexerSource<'s> {
    pub text: &'s str,
    pub next_char_idx: usize,
}

impl LexerSource<'_> {
    fn remaining_text(&self) -> &str {
        &self.text[self.next_char_idx..]
    }

    fn following_text(&self, len: usize) -> &str {
        &self.text[self.next_char_idx..self.next_char_idx + len]
    }

    fn advance_by(&mut self, len: usize) {
        self.next_char_idx += len;
    }

    fn clone_advance_by(&self, len: usize) -> Self {
        let mut new = self.clone();
        new.advance_by(len);
        new
    }
}

struct LexerState<'s> {
    source: LexerSource<'s>,
    extracted_tokens: Vec<Token>,
}

enum LexerStepOutput<'s> {
    Complete(Vec<Token>),
    Incomplete(LexerState<'s>),
}

impl LexerState<'_> {
    pub fn with_source(text: &str) -> LexerState<'_> {
        let mut lexer = LexerState {
            source: LexerSource {
                text,
                next_char_idx: 0usize,
            },
            extracted_tokens: Vec::default(),
        };

        lexer.consume_leading_non_token_whitespace();

        lexer
    }

    fn try_consume_token(&mut self) -> Result<(), TokenizeError> {
        let TokenConsumeResult {
            token,
            bytes_consumed,
        } = try_consume_numeric_literal(&self.source)
            .or_else(|| try_consume_regex_literal(&self.source))
            .or_else(|| try_consume_statement_separator(&self.source))
            .or_else(|| try_consume_brace_or_paren(&self.source))
            .or_else(|| try_consume_comma(&self.source))
            .or_else(|| try_consume_field_reference(&self.source))
            .or_else(|| try_consume_relational_comparison(&self.source))
            .or_else(|| try_consume_math_operator(&self.source))
            .or_else(|| try_consume_string_literal(&self.source))
            .or_else(|| try_consume_identifier(&self.source))
            .ok_or(TokenizeError::SyntaxError)?;

        self.source.advance_by(bytes_consumed);
        self.extracted_tokens.push(token);

        Ok(())
    }

    fn consume_leading_non_token_whitespace(&mut self) {
        let non_token_whitespace: &[_] = &[' ', '\t'];
        let remaining_text = self.source.remaining_text();

        if let Some((0, m)) = remaining_text.match_indices(non_token_whitespace).next() {
            let size = m.len();
            self.source.advance_by(size);
        }
    }

    pub fn next<'s>(mut self) -> Result<LexerStepOutput<'s>, TokenizeError>
    where
        Self: 's,
    {
        self.try_consume_token()?;
        self.consume_leading_non_token_whitespace();

        if self.source.remaining_text().is_empty() {
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
