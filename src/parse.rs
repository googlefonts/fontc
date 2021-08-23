//! Convert raw tokens into semantic events

use std::ops::Range;

use crate::lexer::Lexer;
use crate::token::{Kind, Token};
use crate::token_set::TokenSet;

const LOOKAHEAD: usize = 4;
const LOOKAHEAD_MAX: usize = LOOKAHEAD - 1;

pub(crate) struct Parser<'a> {
    lexer: Lexer<'a>,
    sink: &'a mut dyn TreeSink,
    text: &'a str,
    pos: usize,
    errors: Vec<SyntaxError>,
    buf: [Token; LOOKAHEAD],
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
            buf: [Token::EMPTY; LOOKAHEAD],
            errors: Default::default(),
            pos: Default::default(),
        };

        // preload the buffer; this accumulates any errors
        for _ in 0..LOOKAHEAD {
            this.advance();
        }
        this
    }

    fn nth_range(&self, n: usize) -> Range<usize> {
        let start = match n {
            0 => self.pos,
            idx @ 1..=LOOKAHEAD_MAX => self.pos + self.buf[idx - 1].len,
            _ => panic!("invalid range_n val"),
        };

        let len = self.buf[n].len;
        start..start + len
    }

    fn nth(&self, n: usize) -> Token {
        assert!(n <= LOOKAHEAD_MAX);
        self.buf[n]
    }

    pub(crate) fn start_node(&mut self, kind: Kind) {
        self.sink.start_node(kind);
    }

    pub(crate) fn finish_node(&mut self) {
        self.sink.finish_node();
    }

    pub(crate) fn current_token_text(&self) -> &str {
        &self.text[self.nth_range(0)]
    }

    fn do_bump<const N: usize>(&mut self, kind: Kind) {
        let mut len = 0;
        for _ in 0..N {
            len += self.buf[0].len;
            self.advance();
        }
        self.sink.token(kind, len);
    }

    fn advance(&mut self) {
        self.pos += self.buf[0].len;
        self.buf.rotate_left(1);
        self.buf[LOOKAHEAD_MAX] = self.lexer.next_token();
        // replace sentinal tokens.
        //
        // There are several tokens we receive from the lexer that communicate
        // an error condition. We don't use these directly, but record the error
        // and replace them with a different token type.
        if let Some((replace_kind, error)) = match self.buf[LOOKAHEAD_MAX].kind {
            Kind::StringUnterminated => Some((
                Kind::String,
                "Missing trailing `\"` character to terminate string.",
            )),
            Kind::NumberHexEmpty => {
                Some((Kind::NumberHex, "Missing digits after hexidecimal prefix."))
            }
            _ => None,
        } {
            self.nth(LOOKAHEAD_MAX).kind = replace_kind;
            let range = self.nth_range(LOOKAHEAD_MAX);
            self.errors.push(SyntaxError {
                range,
                message: error.into(),
            })
        }
    }

    /// Eat if we're at this raw token. Return `true` if we eat.
    pub(crate) fn eat(&mut self, raw: Kind) -> bool {
        if self.nth(0).kind == raw {
            self.eat_raw();
            return true;
        }
        false
    }

    /// Eat the next token, regardless of what it is.
    pub(crate) fn eat_raw(&mut self) {
        self.do_bump::<1>(self.nth(0).kind);
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
        self.nth(0).kind == Kind::Eof
    }

    pub(crate) fn eat_trivia(&mut self) {
        while let Kind::Whitespace | Kind::Comment = self.nth(0).kind {
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
        let range = self.nth_range(0);
        if !predicate.matches(self.nth(0).kind, &self.text.as_bytes()[range]) {
            self.eat_raw();
        }
    }

    /// write an error, do not advance
    pub(crate) fn err(&mut self, error: impl Into<String>) {
        let err = SyntaxError {
            range: self.nth_range(0),
            message: error.into(),
        };
        self.sink.error(err);
    }

    /// consume if the token matches, otherwise error without advancing
    pub(crate) fn expect(&mut self, kind: Kind) -> bool {
        if self.nth(0).kind == kind {
            self.eat_raw();
            true
        } else {
            self.err(format!("Expected {}, found {}", kind, self.nth(0).kind));
            false
        }
    }

    pub(crate) fn expect_tag(&mut self) -> bool {
        if self.nth(0).kind == Kind::Ident {
            if self.nth_range(0).len() <= 4 {
                self.eat_remap(Kind::Tag);
            } else {
                // this is an error, but we continue parsing
                self.eat_raw();
                self.err("Tag must be four or fewer characters.");
            }
            true
        } else {
            self.err(format!("expected tag, found {}", self.nth(0).kind));
            false
        }
    }

    pub(crate) fn matches(&self, nth: usize, token: impl TokenComparable) -> bool {
        let range = self.nth_range(nth);
        token.matches(self.nth(nth).kind, &self.text.as_bytes()[range])
    }

    /// If current token is ident, return underlying bytes
    pub(crate) fn ident(&self) -> Option<&[u8]> {
        if self.nth(0).kind == Kind::Ident {
            Some(&self.text.as_bytes()[self.nth_range(0)])
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
