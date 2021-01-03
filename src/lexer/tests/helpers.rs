use crate::lexer::token::SourceSpan;

pub struct TokenSpanCursor {
    next_idx: usize,
}

impl TokenSpanCursor {
    pub fn new() -> Self {
        Self { next_idx: 0 }
    }

    pub fn advance_spanned(&mut self, len: usize) -> SourceSpan {
        let result = SourceSpan {
            start: self.next_idx,
            len,
        };

        self.next_idx += len;
        result
    }

    pub fn advance(&mut self, len: usize) {
        self.next_idx += len;
    }

    pub fn advance_spanned_and_skip(&mut self, len: usize, skip: usize) -> SourceSpan {
        let result = self.advance_spanned(len);
        self.advance(skip);
        result
    }
}
