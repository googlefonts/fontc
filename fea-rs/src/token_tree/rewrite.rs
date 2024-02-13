//! node rewriting.
//!
//! In certain instances (specifically the parsing of contextual and chaining-
//! contextual rules) we can't parse the statement correctly without arbitrary
//! lookahead. Instead, when we encounter a mark glyph we parse the statement
//! naively, and then reparse it again afterwards.

use crate::{parse::FileId, Diagnostic, Level, NodeOrToken};

use super::{AstSink, Kind};

/// A transient context for rewriting nodes.
///
///
/// This is sort of a toy version of the main parser, except that its atom
/// is `NodeOrToken`, not lexemes.
pub(crate) struct ReparseCtx<'a, 'b> {
    pub(super) text_pos: usize,
    pub(super) in_buf: &'a [NodeOrToken],
    pub(super) sink: &'a mut AstSink<'b>,
}

impl<'a, 'b> ReparseCtx<'a, 'b> {
    // nth non-trivia token
    fn nth(&self, n: usize) -> Option<&NodeOrToken> {
        self.in_buf.iter().filter(|t| !t.kind().is_trivia()).nth(n)
    }

    fn nth_kind(&self, n: usize) -> Kind {
        self.nth(n).map(NodeOrToken::kind).unwrap_or(Kind::Eof)
    }

    fn matches(&self, n: usize, kind: Kind) -> bool {
        self.nth(n).map(NodeOrToken::kind) == Some(kind)
    }

    // bump the next non-trivia token and any preceding trivia
    fn eat_any(&mut self) {
        self.eat_trivia();
        self.bump_raw();
    }

    fn bump_raw(&mut self) {
        if let Some((next, rest)) = self.in_buf.split_first() {
            self.text_pos += next.text_len();
            self.sink.push_raw(next.clone());
            self.in_buf = rest;
        }
    }

    fn eat_trivia(&mut self) {
        while self
            .in_buf
            .first()
            .map(|t| t.kind().is_trivia())
            .unwrap_or(false)
        {
            self.bump_raw();
        }
    }

    fn eat(&mut self, kind: Kind) -> bool {
        if self.matches(0, kind) {
            self.eat_trivia();
            self.bump_raw();
            return true;
        }
        false
    }

    fn expect(&mut self, kind: Kind) -> bool {
        if !self.eat(kind) {
            self.err_and_bump(format!("expected '{}' found '{}'", kind, self.nth_kind(0)));
            return false;
        }
        true
    }

    fn expect_semi_and_nothing_else(&mut self) {
        // we should have caught any major issues during the first parsing pass.
        // as a precaution, we error if there are extra tokens and we have not already
        // errored.

        if !self.eat(Kind::Semi) && !self.sink.current_node_has_error() {
            self.error("unexpected tokens in rewriter, please file a bug?");
            while !self.in_buf.is_empty() {
                self.bump_raw();
            }
        }
    }

    fn error(&mut self, message: impl Into<String>) {
        self.add_diagnostic(Level::Error, message);
    }

    fn warn(&mut self, message: impl Into<String>) {
        self.add_diagnostic(Level::Warning, message);
    }

    fn add_diagnostic(&mut self, level: Level, message: impl Into<String>) {
        self.eat_trivia();
        let cur_len = self.nth(0).map(NodeOrToken::text_len).unwrap_or(0);
        let range = self.text_pos..self.text_pos + cur_len;
        let error = Diagnostic::new(level, FileId::CURRENT_FILE, range, message);
        self.sink.error(error)
    }

    fn err_and_bump(&mut self, message: impl Into<String>) {
        self.error(message);
        self.eat_any();
    }

    fn in_node(&mut self, kind: Kind, f: impl FnOnce(&mut ReparseCtx<'a, 'b>)) {
        self.eat_trivia();
        self.sink.start_node(kind);
        f(self);
        self.sink.finish_node(None);
    }

    // for debugging
    //#[allow(dead_code)]
    //fn print_contents(&self, msg: &str) {
    //eprint!("[{}] reparse contents: \"", msg);
    //for item in self.in_buf {
    //match item {
    //NodeOrToken::Token(t) => eprint!("{}", t.text),
    //NodeOrToken::Node(node) => {
    //for t in node.iter_tokens() {
    //eprint!("{}", t.text);
    //}
    //}
    //}
    //}
    //eprintln!("\"");
    //}
}

pub(crate) fn reparse_contextual_sub_rule(rewriter: &mut ReparseCtx) -> Kind {
    if rewriter.eat(Kind::IgnoreKw) {
        rewriter.expect(Kind::SubKw);
        reparse_ignore_rule(rewriter);
        return Kind::GsubIgnore;
    }

    let rule_type = if rewriter.eat(Kind::RsubKw) {
        Kind::GsubType8
    } else {
        rewriter.expect(Kind::SubKw);
        Kind::GsubType6
    };

    // the backtrack sequence
    eat_non_marked_seqeunce(rewriter, Kind::BacktrackSequence);
    // the contextual sequence
    rewriter.in_node(Kind::ContextSequence, |rewriter| loop {
        if !at_glyph_or_glyph_class(rewriter.nth_kind(0)) || !rewriter.matches(1, Kind::SingleQuote)
        {
            break;
        }
        rewriter.in_node(Kind::ContextGlyphNode, |rewriter| {
            expect_glyph_or_glyph_class(rewriter);
            rewriter.expect(Kind::SingleQuote);
            eat_contextual_lookups(rewriter);
        })
    });
    // the lookahead sequence
    eat_non_marked_seqeunce(rewriter, Kind::LookaheadSequence);
    if rewriter.matches(0, Kind::ByKw) {
        rewriter.in_node(Kind::InlineSubNode, |rewriter| {
            rewriter.expect(Kind::ByKw);
            if rewriter.eat(Kind::NullKw) {
                return;
            }
            expect_glyph_or_glyph_class(rewriter);
            while at_glyph_or_glyph_class(rewriter.nth_kind(0)) {
                eat_glyph_or_glyph_class(rewriter);
            }
        });
    }
    if rewriter.matches(0, Kind::FromKw) {
        rewriter.err_and_bump("alternate substition rules cannot be specified inline");
        eat_glyph_or_glyph_class(rewriter);
    }
    rewriter.expect_semi_and_nothing_else();
    rule_type
}

pub(crate) fn reparse_contextual_pos_rule(rewriter: &mut ReparseCtx) -> Kind {
    if rewriter.eat(Kind::IgnoreKw) {
        rewriter.expect(Kind::PosKw);
        reparse_ignore_rule(rewriter);
        return Kind::GposIgnore;
    }
    rewriter.expect(Kind::PosKw);
    // the backtrack sequence
    eat_non_marked_seqeunce(rewriter, Kind::BacktrackSequence);
    // the contextual sequence

    // there is one funny special case here,
    // http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#example-3c
    let mut num_marked_glyphs_and_lookups = 0;
    rewriter.in_node(Kind::ContextSequence, |rewriter| loop {
        if !at_glyph_or_glyph_class(rewriter.nth_kind(0)) || !rewriter.matches(1, Kind::SingleQuote)
        {
            break;
        }
        num_marked_glyphs_and_lookups += 1;
        rewriter.in_node(Kind::ContextGlyphNode, |rewriter| {
            expect_glyph_or_glyph_class(rewriter);
            rewriter.expect(Kind::SingleQuote);
            if eat_contextual_lookups(rewriter) || rewriter.eat(Kind::ValueRecordNode) {
                num_marked_glyphs_and_lookups += 1;
            }
        })
    });

    // the lookahead sequence
    let had_lookahead = eat_non_marked_seqeunce(rewriter, Kind::LookaheadSequence);
    if num_marked_glyphs_and_lookups == 1 && had_lookahead {
        // special case: if only one marked glyph, possible to have value record at end
        // NOTE: the spec says something different but this is the behaviour of both
        // afdko and fonttools, see: https://github.com/adobe-type-tools/afdko/issues/1665
        rewriter.eat(Kind::ValueRecordNode);
    } else if rewriter.matches(0, Kind::ValueRecordNode) {
        rewriter.err_and_bump(
            "trailing value record only valid if there is a single marked glyph, \
            no inline lookups, and at least one lookahead glyph.",
        );
    }

    rewriter.expect_semi_and_nothing_else();
    Kind::GposType8
}

// common between GSUB & GPOS
fn reparse_ignore_rule(rewriter: &mut ReparseCtx) {
    expect_ignore_rule_statement(rewriter);
    while rewriter.eat(Kind::Comma) {
        expect_ignore_rule_statement(rewriter);
    }
    rewriter.expect_semi_and_nothing_else();
}

fn expect_ignore_rule_statement(rewriter: &mut ReparseCtx) {
    let has_mark_glyph = rewriter
        .in_buf
        .iter()
        .any(|n| n.kind() == Kind::SingleQuote);

    // the common, well-formed case where there is an actual mark glyph
    if has_mark_glyph {
        rewriter.in_node(Kind::IgnoreRuleStatementNode, |rewriter| {
            eat_non_marked_seqeunce(rewriter, Kind::BacktrackSequence);
            rewriter.in_node(Kind::ContextSequence, |rewriter| loop {
                if !at_glyph_or_glyph_class(rewriter.nth_kind(0))
                    || !rewriter.matches(1, Kind::SingleQuote)
                {
                    break;
                }
                rewriter.in_node(Kind::ContextGlyphNode, |rewriter| {
                    expect_glyph_or_glyph_class(rewriter);
                    rewriter.expect(Kind::SingleQuote);
                })
            });
            eat_non_marked_seqeunce(rewriter, Kind::LookaheadSequence);
        });
        // the degen case, where there are no mark glyphs and so we just make
        // the first non-marked glyph be the input sequence:
    } else {
        rewriter.warn("No marked glyphs in sequence. This glyph will be treated as input, all others as lookahead.");
        rewriter.in_node(Kind::IgnoreRuleStatementNode, |rewriter| {
            rewriter.in_node(Kind::BacktrackSequence, |_| {}); // empty backtrack
            rewriter.in_node(Kind::ContextSequence, |rewriter| {
                rewriter.in_node(Kind::ContextGlyphNode, |rewriter| {
                    expect_glyph_or_glyph_class(rewriter);
                })
            });
            eat_non_marked_seqeunce(rewriter, Kind::LookaheadSequence);
        })
    }
}

// impl common to eating backtrack or lookahead
fn eat_non_marked_seqeunce(rewriter: &mut ReparseCtx, kind: Kind) -> bool {
    let mut ate_something = false;
    rewriter.in_node(kind, |rewriter| loop {
        if rewriter.matches(1, Kind::SingleQuote) || !eat_glyph_or_glyph_class(rewriter) {
            break;
        }
        ate_something = true;
    });
    ate_something
}

// the two lookups in "sub ka' lookup ONE lookup TWO b'"
fn eat_contextual_lookups(rewriter: &mut ReparseCtx) -> bool {
    if !eat_one_contextual_lookup(rewriter) {
        return false;
    }

    while eat_one_contextual_lookup(rewriter) {
        // continue
    }
    true
}

fn eat_one_contextual_lookup(rewriter: &mut ReparseCtx) -> bool {
    rewriter.eat_trivia();
    if rewriter.matches(0, Kind::LookupKw) {
        rewriter.in_node(Kind::LookupRefNode, |rewriter| {
            rewriter.expect(Kind::LookupKw);
            rewriter.expect(Kind::Ident);
        });
        return true;
    }
    false
}

// a helper for expect_ fns
fn expect(
    rewriter: &mut ReparseCtx,
    eat_fn: impl FnOnce(&mut ReparseCtx) -> bool,
    expected_name: &str,
) -> bool {
    if !eat_fn(rewriter) {
        rewriter.err_and_bump(format!(
            "expected '{}', found '{}",
            expected_name,
            rewriter.nth_kind(0)
        ));
        return false;
    }
    true
}

fn at_glyph_or_glyph_class(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::GlyphName | Kind::Cid | Kind::GlyphClass | Kind::NamedGlyphClass
    )
}

fn eat_glyph_or_glyph_class(rewriter: &mut ReparseCtx) -> bool {
    if at_glyph_or_glyph_class(rewriter.nth_kind(0)) {
        rewriter.eat_any();
        return true;
    }
    false
}

fn expect_glyph_or_glyph_class(rewriter: &mut ReparseCtx) -> bool {
    expect(rewriter, eat_glyph_or_glyph_class, "glyph or glyph class")
}
