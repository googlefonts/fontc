//! Convert raw tokens into semantic events

use std::fmt::Display;
use std::ops::Range;

use write_fonts::types::Tag;

use super::{
    lexer::{Kind as LexemeKind, Lexeme, Lexer, TokenSet},
    FileId,
};
use crate::token_tree::{AstSink, Kind};

use crate::diagnostic::Diagnostic;

const LOOKAHEAD: usize = 4;
const LOOKAHEAD_MAX: usize = LOOKAHEAD - 1;

/// A parsing context.
///
/// This type wraps a lexer (responsible for generating base tokens) and exposes
/// an API for inspecting, consuming, remapping, and grouping these tokens, as
/// well as recording any encountered errors.
///
/// This type does not implement the parsing *logic*; it is driven by various
/// functions defined in the [`grammar`](super::grammar) module.
pub struct Parser<'a, 'b> {
    lexer: Lexer<'a>,
    // these lifetimes are a hangover from a previous design.
    sink: &'b mut AstSink<'a>,
    text: &'a str,
    buf: [PendingToken; LOOKAHEAD],
    split_buf: Vec<(Range<usize>, Kind)>,
}

/// A non-trivia token, as well as any trivia preceding that token.
///
/// We don't want to worry about trivia for the purposes of most parsing,
/// but we do need to track it in the tree. To achieve this, we collect trivia
/// and store it attached to the subsequent non-trivia token, and then add it
/// to the tree when that token is consumed.
struct PendingToken {
    preceding_trivia: Vec<Lexeme>,
    // the position of the first token, including trivia
    start_pos: usize,
    // total length of trivia
    trivia_len: usize,
    token: Lexeme,
}

/// A tag and its range.
///
/// We often want to associate errors with tag locations, so we handle
/// them specially.
#[derive(Clone, Debug)]
pub(crate) struct TagToken {
    pub tag: Tag,
    pub range: Range<usize>,
}

impl PendingToken {
    const EMPTY: PendingToken = PendingToken {
        preceding_trivia: Vec::new(),
        start_pos: 0,
        trivia_len: 0,
        token: Lexeme::EMPTY,
    };
}

impl<'b, 'a> Parser<'a, 'b> {
    pub(crate) fn new(text: &'a str, sink: &'b mut AstSink<'a>) -> Self {
        let mut this = Parser {
            lexer: Lexer::new(text),
            sink,
            text,
            split_buf: Default::default(),
            buf: [PendingToken::EMPTY; LOOKAHEAD],
        };

        // preload the buffer; this accumulates any errors
        for _ in 0..LOOKAHEAD {
            this.advance();
        }
        this
    }

    pub(crate) fn nth_range(&self, n: usize) -> Range<usize> {
        assert!(n < LOOKAHEAD);
        let start = self.buf[n].start_pos + self.buf[n].trivia_len;
        start..start + self.buf[n].token.len
    }

    pub(crate) fn nth(&self, n: usize) -> Lexeme {
        assert!(n <= LOOKAHEAD_MAX);
        self.buf[n].token
    }

    pub(crate) fn start_node(&mut self, kind: Kind) {
        self.sink.start_node(kind);
    }

    pub(crate) fn finish_node(&mut self) {
        self.sink.finish_node(None);
    }

    pub(crate) fn in_node<R>(&mut self, kind: Kind, f: impl FnOnce(&mut Parser) -> R) -> R {
        self.eat_trivia();
        self.start_node(kind);
        let r = f(self);
        self.finish_node();
        r
    }

    pub(crate) fn finish_and_remap_node(&mut self, new_kind: Kind) {
        self.sink.finish_node(Some(new_kind))
    }

    pub(crate) fn nth_raw(&self, n: usize) -> &[u8] {
        let range = self.nth_range(n);
        &self.text.as_bytes()[range]
    }

    pub(crate) fn current_token_text(&self) -> &str {
        &self.text[self.nth_range(0)]
    }

    fn current_pos(&self) -> usize {
        self.buf[0].start_pos
    }

    /// advance `N` lexemes, remapping to `Kind`.
    fn do_bump<const N: usize>(&mut self, kind: Kind) {
        let mut len = 0;
        for _ in 0..N {
            len += self.nth(0).len;
            self.advance();
        }
        self.sink.token(kind, len);
    }

    fn advance(&mut self) {
        self.eat_trivia();

        let prev_token = &self.buf[LOOKAHEAD_MAX];
        let new_start = prev_token.start_pos + prev_token.trivia_len + prev_token.token.len;
        self.buf.rotate_left(1);

        let pending = &mut self.buf[LOOKAHEAD_MAX];
        pending.start_pos = new_start;
        pending.trivia_len = 0;
        pending.token = loop {
            let token = self.lexer.next_token();
            if token.kind.is_trivia() {
                pending.trivia_len += token.len;
                pending.preceding_trivia.push(token);
            } else {
                break token;
            }
        };

        self.validate_new_token();
    }

    fn validate_new_token(&mut self) {
        if let Some((replace_kind, error)) = match self.nth(LOOKAHEAD_MAX).kind {
            LexemeKind::StringUnterminated => Some((
                LexemeKind::String,
                "Unterminated string (missing trailing '\"')",
            )),
            LexemeKind::HexEmpty => {
                Some((LexemeKind::Hex, "Missing digits after hexidecimal prefix."))
            }
            _ => None,
        } {
            let mut range = self.nth_range(LOOKAHEAD_MAX);
            // for unterminated string, error only points to opening "
            if replace_kind == LexemeKind::String {
                range.end = range.start + 1;
            }
            self.sink
                .error(Diagnostic::error(FileId::CURRENT_FILE, range, error));
            self.buf[LOOKAHEAD_MAX].token.kind = replace_kind;
        }
    }

    /// a hack for working with the glyphsapp syntax extension.
    ///
    /// In this syntax we will encounter things like '$(pad-2)'. because '-' is
    /// a valid character in idents, the whole unit 'pad-2' is lexed as an ident.
    ///
    /// In this particular context, though, we want to parse it as 'pad' '-' '2'.
    ///
    /// This function takes a 'split_fn' argument that is passed the token text
    /// and an empty buffer; if the caller wants to split the token then they
    /// must populate the buffer with the range and kind of each new token.
    ///
    /// If no ranges are added to the buffer, this function does nothing.
    ///
    /// If the token is split, the cursor advances, as if the token had been
    /// eaten.
    ///
    /// Returns `true` if the token was split, and `false` otherwise.`
    ///
    /// # Panics
    ///
    /// The ranges must cover the entire provided text, and cannot overlap, or
    /// this fn will panic.
    pub(crate) fn split_remap_current(
        &mut self,
        // the kind of the current token; if these don't match we nop
        kind: impl TokenComparable,
        split_fn: impl FnOnce(&str, &mut Vec<(Range<usize>, Kind)>),
    ) -> bool {
        if !self.matches(0, kind) {
            return false;
        }
        // temporarily take buffer to get around borrowck
        let mut buf = std::mem::take(&mut self.split_buf);
        let text = self.current_token_text();
        split_fn(text, &mut buf);
        if buf.is_empty() {
            // put back the buffer to reuse later
            self.split_buf = buf;
            return false;
        }

        let mut prev_end = 0;
        for (range, kind) in buf.drain(..) {
            assert_eq!(range.start, prev_end, "split cannot have gaps");
            prev_end = range.end;
            let len = range.end.checked_sub(range.start).unwrap();
            self.sink.token(kind, len);
        }
        assert_eq!(
            prev_end,
            self.current_token_text().len(),
            "split must use whole token"
        );
        self.advance();
        // put the buffer back so we can reuse it
        self.split_buf = buf;

        true
    }

    /// Eat if the current token matches.
    pub(crate) fn eat(&mut self, raw: impl TokenComparable) -> bool {
        if self.matches(0, raw) {
            self.eat_raw();
            return true;
        }
        false
    }

    /// Consumes all tokens until hitting a recovery item.
    pub(crate) fn eat_until(&mut self, recovery: impl TokenComparable) {
        while !self.at_eof() && !self.matches(0, recovery) {
            self.eat_raw();
        }
    }

    /// Consume until first non-matching token
    pub(crate) fn eat_while(&mut self, token: impl TokenComparable) {
        while self.eat(token) {
            continue;
        }
    }

    /// Consume unless token matches.
    pub(crate) fn eat_unless(&mut self, token: impl TokenComparable) {
        if !self.matches(0, token) {
            self.eat_raw();
        }
    }

    /// Eat the next token, regardless of what it is.
    pub(crate) fn eat_raw(&mut self) {
        self.do_bump::<1>(self.nth(0).kind.to_token_kind());
    }

    /// Eat the next token if it matches `expect`, replacing it with `remap`.
    ///
    /// Necessary for handling keywords, which are not known to the lexer.
    pub(crate) fn eat_remap(&mut self, expect: impl TokenComparable, remap: Kind) -> bool {
        if self.matches(0, expect) {
            self.do_bump::<1>(remap);
            return true;
        }
        false
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.nth(0).kind == LexemeKind::Eof
    }

    /// Eat any trivia preceding the current token.
    ///
    /// This is normally not necessary, because trivia is consumed when eating
    /// other tokens. It is useful only when trivia should or should not be
    /// associated with a particular node.
    pub(crate) fn eat_trivia(&mut self) {
        for token in self.buf[0].preceding_trivia.drain(..) {
            self.sink.token(token.kind.to_token_kind(), token.len);
        }
        self.buf[0].start_pos += self.buf[0].trivia_len;
        self.buf[0].trivia_len = 0;
    }

    pub(crate) fn err_and_bump(&mut self, error: impl Into<String>) {
        self.err(error);
        self.eat_raw();
    }

    pub(crate) fn raw_error(&mut self, range: Range<usize>, message: impl Into<String>) {
        self.sink
            .error(Diagnostic::error(FileId::CURRENT_FILE, range, message));
    }

    /// Error, and advance unless the current token matches a predicate.
    pub(crate) fn err_recover(
        &mut self,
        error: impl Into<String>,
        predicate: impl TokenComparable,
    ) {
        self.err(error);
        if !self.matches(0, predicate) {
            self.eat_raw();
        }
    }

    /// write an error, do not advance
    pub(crate) fn err(&mut self, error: impl Into<String>) {
        let err = Diagnostic::error(FileId::CURRENT_FILE, self.nth_range(0), error);
        self.sink.error(err);
    }

    /// write a warning, do not advance
    pub(crate) fn warn(&mut self, message: impl Into<String>) {
        let err = Diagnostic::warning(FileId::CURRENT_FILE, self.nth_range(0), message);
        self.sink.error(err);
    }

    /// Write an error associated *before* the whitespace of the current token.
    ///
    /// In practice this is useful when missing things like semis or braces.
    pub(crate) fn err_before_ws(&mut self, error: impl Into<String>) {
        let pos = self.buf[0].start_pos;
        self.raw_error(pos..pos + 1, error);
    }

    /// Write a *warning* before the whitespace of the associated token.
    ///
    /// This only exists so we can warn if a semi is missing after an include
    /// statement (which is common in the wild)
    pub(crate) fn warn_before_ws(&mut self, error: impl Into<String>) {
        let pos = self.buf[0].start_pos;
        let diagnostic = Diagnostic::warning(FileId::CURRENT_FILE, pos..pos + 1, error);
        self.sink.error(diagnostic);
    }

    /// consume if the token matches, otherwise error without advancing
    pub(crate) fn expect(&mut self, kind: impl TokenComparable) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.err(format!("Expected {}, found {}", kind, self.nth(0).kind));
        false
    }

    /// semi gets special handling, we don't care to print whatever else we find,
    /// and we want to include whitespace in the range (i.e, it hugs the previous line).
    pub(crate) fn expect_semi(&mut self) -> bool {
        if !self.eat(LexemeKind::Semi) {
            self.err_before_ws("Expected ';'");
            return false;
        }
        true
    }

    pub(crate) fn expect_recover(
        &mut self,
        kind: impl TokenComparable,
        recover: impl TokenComparable,
    ) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.err(format!("Expected {} found {}", kind, self.nth(0).kind));
        if !self.matches(0, recover) {
            self.eat_raw();
        }
        false
    }

    pub(crate) fn expect_remap_recover(
        &mut self,
        expect: impl TokenComparable,
        remap: Kind,
        recover: impl TokenComparable,
    ) -> bool {
        if self.eat_remap(expect, remap) {
            return true;
        }
        self.err(format!("Expected {} found {}", remap, self.nth(0).kind));
        if !self.matches(0, recover) {
            self.eat_raw();
        }
        false
    }

    pub(crate) fn eat_tag(&mut self) -> Option<TagToken> {
        if self.matches(0, TokenSet::TAG_LIKE) {
            if let Ok(tag) = Tag::new_checked(self.nth_raw(0)) {
                let range = self.nth_range(0);
                self.do_bump::<1>(Kind::Tag);
                return Some(TagToken { tag, range });
            }
        }
        None
    }

    pub(crate) fn expect_tag(&mut self, recover: impl TokenComparable) -> Option<TagToken> {
        if self.matches(0, TokenSet::TAG_LIKE) {
            match self.eat_tag() {
                Some(tag) => return Some(tag),
                None => self.err_and_bump("invalid tag"),
            }
        } else {
            self.err_recover(format!("expected tag, found {}", self.nth(0).kind), recover);
        }
        None
    }

    pub(crate) fn matches(&self, nth: usize, token: impl TokenComparable) -> bool {
        token.matches(self.nth(nth).kind)
    }

    pub(crate) fn raw_range(&self, range: Range<usize>) -> &[u8] {
        &self.text.as_bytes()[range]
    }

    /// Calls the closure with the parser repeatedly, stopping if the parser has not advanced.
    ///
    /// The closure is expected to behave like an 'eat' or 'expect' function,
    /// returning `true` on success (which is different from advancing the cursor!).
    ///
    /// We return `true` if all calls returned true.
    ///
    /// This is useful for parsing sequences of identical items, where we want
    /// to avoid generating a new error for each failed item.
    pub(crate) fn repeat(
        &mut self,
        mut f: impl FnMut(&mut Parser) -> bool,
        repeat_count: usize,
    ) -> bool {
        let mut pos = self.current_pos();
        let mut result = true;
        for _ in 0..repeat_count {
            result &= f(self);
            if self.current_pos() == pos {
                break;
            }
            pos = self.current_pos();
        }
        result
    }
}

pub(crate) trait TokenComparable: Copy + Display {
    fn matches(&self, kind: LexemeKind) -> bool;
}

impl TokenComparable for LexemeKind {
    fn matches(&self, kind: LexemeKind) -> bool {
        self == &kind
    }
}

impl TokenComparable for Kind {
    fn matches(&self, kind: LexemeKind) -> bool {
        kind.to_token_kind() == *self
    }
}

impl TokenComparable for TokenSet {
    fn matches(&self, kind: LexemeKind) -> bool {
        self.contains(kind)
    }
}
