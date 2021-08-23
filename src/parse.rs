//! Convert raw tokens into semantic events

use std::ops::Range;

use crate::lexer::Lexer;
use crate::token::{Kind, Token};
use crate::token_set::TokenSet;

pub(crate) struct Parser<'a> {
    lexer: Lexer<'a>,
    sink: &'a mut dyn TreeSink,
    text: &'a str,
    pos: usize,
    errors: Vec<SyntaxError>,
    current: Token,
    next: Token,
}

#[derive(Debug, Clone)]
pub(crate) struct SyntaxError {
    message: String,
    range: Range<usize>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(text: &'a str, sink: &'a mut dyn TreeSink) -> Self {
        let mut this = Parser {
            lexer: Lexer::new(text),
            sink,
            text,
            current: Token::EMPTY,
            next: Token::EMPTY,
            errors: Default::default(),
            pos: Default::default(),
        };

        // preload the first two tokens; this accumulates any errors
        this.advance();
        this.advance();
        this
    }

    fn current_range(&self) -> Range<usize> {
        self.pos..self.pos + self.current.len
    }

    pub(crate) fn start_node(&mut self, kind: Kind) {
        self.sink.start_node(kind);
    }

    pub(crate) fn finish_node(&mut self) {
        self.sink.finish_node();
    }

    pub(crate) fn current_token_text(&self) -> &str {
        &self.text[self.current_range()]
    }

    fn do_bump<const N: usize>(&mut self, kind: Kind) {
        let mut len = 0;
        for _ in 0..N {
            len += self.current.len;
            self.advance();
        }
        self.sink.token(kind, len);
    }

    fn advance(&mut self) {
        self.pos += self.current.len;
        self.current = self.next;
        self.next = self.lexer.next_token();

        // replace sentinal tokens.
        //
        // There are several tokens we receive from the lexer that communicate
        // an error condition. We don't use these directly, but record the error
        // and replace them with a different token type.
        if let Some((replace_kind, error)) = match self.next.kind {
            Kind::StringUnterminated => Some((
                Kind::String,
                "Missing trailing `\"` character to terminate string.",
            )),
            Kind::NumberHexEmpty => {
                Some((Kind::NumberHex, "Missing digits after hexidecimal prefix."))
            }
            _ => None,
        } {
            self.next.kind = replace_kind;
            let next_pos = self.pos + self.current.len;
            let range = next_pos..next_pos + self.next.len;
            self.errors.push(SyntaxError {
                range,
                message: error.into(),
            })
        }
    }

    /// Eat if we're at this raw token. Return `true` if we eat.
    pub(crate) fn eat(&mut self, raw: Kind) -> bool {
        if self.current.kind == raw {
            self.eat_raw();
            return true;
        }
        false
    }

    /// Eat the next token, regardless of what it is.
    pub(crate) fn eat_raw(&mut self) {
        self.do_bump::<1>(self.current.kind);
    }

    /// Eat the next token, giving it an explicit kind.
    ///
    /// Necessary for handling keywords, which are not known to the lexer.
    pub(crate) fn eat_remap(&mut self, kind: Kind) {
        self.do_bump::<1>(kind);
    }

    /// combine two tokens into one
    pub(crate) fn eat_remap2(&mut self, kind: Kind) {
        self.do_bump::<2>(kind);
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.current.kind == Kind::Eof
    }

    pub(crate) fn eat_trivia(&mut self) {
        while let Kind::Whitespace | Kind::Comment = self.current.kind {
            self.eat_raw()
        }
    }

    pub(crate) fn err_and_bump(&mut self, error: impl Into<String>) {
        self.err(error);
        self.eat_raw();
    }

    /// Error, and advance unless the current token matches a predicate.
    pub(crate) fn err_recover(
        &mut self,
        error: impl Into<String>,
        predicate: impl TokenComparable,
    ) {
        self.err(error);
        let range = self.current_range();
        if !predicate.matches(self.current.kind, &self.text.as_bytes()[range]) {
            self.eat_raw();
        }
    }

    /// write an error, do not advance
    pub(crate) fn err(&mut self, error: impl Into<String>) {
        let err = SyntaxError {
            range: self.current_range(),
            message: error.into(),
        };
        self.sink.error(err);
    }

    /// consume if the token matches, otherwise error without advancing
    pub(crate) fn expect(&mut self, kind: Kind) -> bool {
        if self.current.kind == kind {
            self.eat_raw();
            true
        } else {
            self.err(format!("Expected {}, found {}", kind, self.current.kind));
            false
        }
    }

    pub(crate) fn expect_tag(&mut self) -> bool {
        if self.current.kind == Kind::Ident {
            if self.current_range().len() <= 4 {
                self.eat_remap(Kind::Tag);
            } else {
                // this is an error, but we continue parsing
                self.eat_raw();
                self.err("Tag must be four or fewer characters.");
            }
            true
        } else {
            self.err(format!("expected tag, found {}", self.current.kind));
            false
        }
    }

    pub(crate) fn current_match(&self, token: impl TokenComparable) -> bool {
        let range = self.current_range();
        token.matches(self.current.kind, &self.text.as_bytes()[range])
    }

    pub(crate) fn next_match(&self, token: impl TokenComparable) -> bool {
        let next_pos = self.pos + self.current.len;
        let next_range = next_pos..next_pos + self.next.len;
        token.matches(self.next.kind, &self.text.as_bytes()[next_range])
    }

    /// If current token is ident, return underlying bytes
    pub(crate) fn ident(&self) -> Option<&[u8]> {
        if self.current.kind == Kind::Ident {
            Some(&self.text.as_bytes()[self.current_range()])
        } else {
            None
        }
    }
}

pub(crate) trait TokenComparable {
    fn matches(&self, kind: Kind, bytes: &[u8]) -> bool;
}

impl TokenComparable for &[u8] {
    fn matches(&self, _: Kind, bytes: &[u8]) -> bool {
        self == &bytes
    }
}

impl TokenComparable for Kind {
    fn matches(&self, kind: Kind, _bytes: &[u8]) -> bool {
        self == &kind
    }
}

impl TokenComparable for &[&[u8]] {
    fn matches(&self, kind: Kind, bytes: &[u8]) -> bool {
        self.contains(&bytes)
    }
}

impl TokenComparable for TokenSet {
    fn matches(&self, kind: Kind, _bytes: &[u8]) -> bool {
        self.contains(kind)
    }
}

// taken from rust-analzyer
/// `TreeSink` abstracts details of a particular syntax tree implementation.
pub(crate) trait TreeSink {
    /// Adds new token to the current branch.
    fn token(&mut self, kind: Kind, len: usize);

    /// Start new branch and make it current.
    fn start_node(&mut self, kind: Kind);

    /// Finish current branch and restore previous
    /// branch as current.
    fn finish_node(&mut self);

    fn error(&mut self, error: SyntaxError);
}

#[derive(Debug, Clone)]
enum Event {
    /// start a new node
    Start(Kind),
    /// finish the current node
    Finish,
    Token(Kind, usize),
    Error(SyntaxError),
}

#[derive(Clone, Debug, Default)]
pub(crate) struct DebugSink(Vec<Event>);

impl TreeSink for DebugSink {
    fn token(&mut self, kind: Kind, len: usize) {
        self.0.push(Event::Token(kind, len))
    }

    fn start_node(&mut self, kind: Kind) {
        self.0.push(Event::Start(kind))
    }

    fn finish_node(&mut self) {
        self.0.push(Event::Finish);
    }

    fn error(&mut self, error: SyntaxError) {
        self.0.push(Event::Error(error))
    }
}

impl DebugSink {
    pub(crate) fn token_tree(&self) -> String {
        self.to_string()
    }
}

impl std::fmt::Display for DebugSink {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut pos = 0;
        let mut indent = 0;
        let mut node_stack = Vec::new();
        static WS: &str = "                                                                                                                        ";
        for event in &self.0 {
            match event {
                Event::Start(kind) => {
                    writeln!(f, "{}START {}", &WS[..indent], kind)?;
                    node_stack.push(kind);
                    indent += 2;
                }
                Event::Token(kind, len) => {
                    writeln!(f, "{}{}..{} {}", &WS[..indent], pos, pos + len, kind)?;
                    pos += len;
                }
                Event::Finish => {
                    indent -= 2;
                    writeln!(f, "{}END {}", &WS[..indent], node_stack.pop().unwrap())?;
                }
                Event::Error(_) => (),
            }
        }
        Ok(())
    }
}
