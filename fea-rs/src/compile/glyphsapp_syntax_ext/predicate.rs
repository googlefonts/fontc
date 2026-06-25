//! Parsing and evaluation of Glyphs.app glyph predicate tokens (`$[...]`).
//!
//! Glyphs.app sources can use a predicate token inside a FEA glyph class to
//! select glyphs by an `NSPredicate`-style expression, e.g.
//! `@ss01 = [ $[name endswith 'ss01'] ];`. At compile time we expand the token
//! into the matching glyphs, in glyph order.
//!
//! This is "Phase 1" of <https://github.com/googlefonts/fontc/issues/92>: only
//! the `name` attribute is supported, with the string operators `beginswith`,
//! `endswith`, `contains`, `==` and `!=`, joined by a flat chain of either
//! `and` or `or` (but not a mix of the two). Anything else -- other attributes
//! (`category`, `case`, `unicode`, ...), the `like`/`matches` operators, `not`,
//! parentheses, or a mix of `and` and `or` -- is rejected, and is tracked as
//! Phase 2 in <https://github.com/googlefonts/fontc/issues/2052>.
//!
//! The reference implementation is glyphsLib's `TokenExpander`
//! (`Lib/glyphsLib/builder/tokens.py`); we mirror its semantics: operator and
//! boolean keywords are case-insensitive, value comparisons are case-sensitive,
//! both single and double quotes are accepted, and matches are emitted in glyph
//! order, de-duplicated. For `or`, glyphsLib accumulates clause-by-clause (all
//! of clause 1's matches in glyph order, then clause 2's new matches, ...)
//! rather than a single glyph-order pass; we reproduce that exactly because the
//! resulting class member order is observable in the compiled tables.

use std::collections::HashSet;
use std::hash::Hash;

/// A comparison operator over a glyph's name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Op {
    BeginsWith,
    EndsWith,
    Contains,
    Eq,
    NotEq,
}

impl Op {
    fn matches(self, name: &str, value: &str) -> bool {
        match self {
            Op::BeginsWith => name.starts_with(value),
            Op::EndsWith => name.ends_with(value),
            Op::Contains => name.contains(value),
            Op::Eq => name == value,
            Op::NotEq => name != value,
        }
    }
}

/// The boolean connective joining the clauses of a predicate.
///
/// Phase 1 only supports a flat chain of a single connective; mixing `and` and
/// `or` is rejected at parse time.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Connective {
    And,
    Or,
}

/// A single `name <op> "value"` comparison.
#[derive(Clone, Debug)]
struct Clause {
    op: Op,
    value: String,
}

impl Clause {
    fn matches(&self, name: &str) -> bool {
        self.op.matches(name, &self.value)
    }
}

/// A parsed, name-only Glyphs.app glyph predicate.
#[derive(Clone, Debug)]
pub(crate) struct Predicate {
    clauses: Vec<Clause>,
    // `None` when there is a single clause.
    connective: Option<Connective>,
}

/// An error encountered while parsing a predicate token.
///
/// The caller is responsible for attaching a source range; the message is
/// self-contained.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PredicateError {
    pub(crate) message: String,
}

impl PredicateError {
    fn new(message: impl Into<String>) -> Self {
        PredicateError {
            message: message.into(),
        }
    }
}

impl Predicate {
    /// Parse a predicate token.
    ///
    /// `token` is the raw text of the token as it appears in the source,
    /// including the leading `$[` and trailing `]` (the brackets are stripped
    /// here). Returns an error for anything outside the Phase 1 subset.
    pub(crate) fn parse(token: &str) -> Result<Self, PredicateError> {
        let inner = strip_brackets(token)?;
        let mut parser = PredicateParser { rest: inner };
        let mut clauses = Vec::new();
        let mut connective: Option<Connective> = None;

        loop {
            clauses.push(parser.parse_clause()?);
            match parser.parse_connective()? {
                None => break,
                Some(conn) => match connective {
                    None => connective = Some(conn),
                    Some(existing) if existing != conn => {
                        return Err(PredicateError::new(
                            "mixing 'and' and 'or' in a glyphs predicate is not yet \
                             supported (see fontc#2052)",
                        ));
                    }
                    Some(_) => {}
                },
            }
        }
        Ok(Predicate {
            clauses,
            connective,
        })
    }

    /// Evaluate the predicate against a glyph set.
    ///
    /// `glyphs` yields `(id, name)` pairs and MUST be in glyph (GID) order; the
    /// returned ids preserve that order. Results are de-duplicated.
    ///
    /// glyphsLib emits predicate matches in *source* glyph order. We only have
    /// the GID-ordered glyph map, which equals source order in the common case
    /// but not for a source with a custom `glyphOrder` parameter that reorders
    /// glyphs relative to the source; in that case a class whose member order is
    /// observable (e.g. a parallel class-to-class substitution) could diverge.
    /// Resolving this would require threading source order through to fea-rs.
    pub(crate) fn evaluate<'a, T>(&self, glyphs: impl IntoIterator<Item = (T, &'a str)>) -> Vec<T>
    where
        T: Copy + Eq + Hash,
    {
        let glyphs: Vec<(T, &str)> = glyphs.into_iter().collect();
        match self.connective {
            // glyphsLib appends each clause's matches in turn, so a glyph that
            // matches an earlier clause keeps its earlier position. A single
            // glyph-order pass would re-interleave them; this does not.
            Some(Connective::Or) => {
                let mut seen = HashSet::new();
                let mut out = Vec::new();
                for clause in &self.clauses {
                    for (id, name) in &glyphs {
                        if clause.matches(name) && seen.insert(*id) {
                            out.push(*id);
                        }
                    }
                }
                out
            }
            // A single clause, or an `and` chain: a glyph is included iff every
            // clause matches. Iterating once in glyph order matches glyphsLib's
            // ordering for `and` (which preserves first-clause order) and
            // naturally de-duplicates.
            None | Some(Connective::And) => glyphs
                .iter()
                .filter(|(_, name)| self.clauses.iter().all(|clause| clause.matches(name)))
                .map(|(id, _)| *id)
                .collect(),
        }
    }
}

fn strip_brackets(token: &str) -> Result<&str, PredicateError> {
    let token = token.trim();
    let inner = token
        .strip_prefix("$[")
        .and_then(|rest| rest.strip_suffix(']'))
        .ok_or_else(|| PredicateError::new("malformed glyphs predicate (expected '$[...]')"))?;
    Ok(inner)
}

struct PredicateParser<'a> {
    rest: &'a str,
}

impl<'a> PredicateParser<'a> {
    fn skip_ws(&mut self) {
        self.rest = self.rest.trim_start();
    }

    fn eat(&mut self, prefix: &str) -> bool {
        if let Some(rest) = self.rest.strip_prefix(prefix) {
            self.rest = rest;
            true
        } else {
            false
        }
    }

    /// Consume a run of `[A-Za-z0-9_]` characters.
    fn eat_word(&mut self) -> &'a str {
        let end = self
            .rest
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
            .unwrap_or(self.rest.len());
        let (word, rest) = self.rest.split_at(end);
        self.rest = rest;
        word
    }

    fn parse_clause(&mut self) -> Result<Clause, PredicateError> {
        self.skip_ws();
        // reject Phase 2 grammar that would otherwise be misread
        if self.rest.starts_with('(') || self.rest.starts_with(')') {
            return Err(PredicateError::new(
                "parentheses in glyphs predicates are not yet supported (see fontc#2052)",
            ));
        }
        let attr = self.eat_word();
        if attr.is_empty() {
            // a leading '!' or 'not' lands here
            return Err(PredicateError::new(
                "expected a glyph predicate attribute (only 'name' is supported)",
            ));
        }
        if attr.eq_ignore_ascii_case("not") {
            return Err(PredicateError::new(
                "'not' in glyphs predicates is not yet supported (see fontc#2052)",
            ));
        }
        if attr != "name" {
            return Err(PredicateError::new(format!(
                "unsupported glyphs predicate attribute '{attr}': only 'name' is supported \
                 (see fontc#2052)"
            )));
        }
        let op = self.parse_op()?;
        let value = self.parse_value()?;
        Ok(Clause { op, value })
    }

    fn parse_op(&mut self) -> Result<Op, PredicateError> {
        self.skip_ws();
        // symbolic operators, longest first
        if self.eat("==") {
            return Ok(Op::Eq);
        }
        if self.eat("!=") || self.eat("<>") {
            return Ok(Op::NotEq);
        }
        // unsupported Phase 2 symbolic operators -- checked before the single
        // '=' so the aliases '=>' / '=<' get the right message rather than
        // parsing as '=' with a dangling '>' / '<'.
        for unsupported in [">=", "<=", "=>", "=<", ">", "<"] {
            if self.rest.starts_with(unsupported) {
                return Err(PredicateError::new(format!(
                    "unsupported glyphs predicate operator '{unsupported}' (see fontc#2052)"
                )));
            }
        }
        if self.eat("=") {
            return Ok(Op::Eq);
        }
        let word = self.eat_word();
        if word.is_empty() {
            return Err(PredicateError::new(
                "expected a comparison operator in glyphs predicate",
            ));
        }
        match word.to_ascii_lowercase().as_str() {
            "beginswith" => Ok(Op::BeginsWith),
            "endswith" => Ok(Op::EndsWith),
            "contains" => Ok(Op::Contains),
            "like" | "matches" | "between" | "in" => Err(PredicateError::new(format!(
                "unsupported glyphs predicate operator '{word}' (see fontc#2052)"
            ))),
            other => Err(PredicateError::new(format!(
                "unknown glyphs predicate operator '{other}'"
            ))),
        }
    }

    fn parse_value(&mut self) -> Result<String, PredicateError> {
        self.skip_ws();
        for quote in ['"', '\''] {
            if let Some(rest) = self.rest.strip_prefix(quote) {
                let end = rest.find(quote).ok_or_else(|| {
                    PredicateError::new("unterminated string in glyphs predicate")
                })?;
                let value = &rest[..end];
                if value.is_empty() {
                    return Err(PredicateError::new("empty value in glyphs predicate"));
                }
                self.rest = &rest[end + 1..];
                return Ok(value.to_string());
            }
        }
        // bare (unquoted) value: glyphsLib accepts a `\w+` run
        let word = self.eat_word();
        if word.is_empty() {
            return Err(PredicateError::new(
                "expected a value after the comparison operator in glyphs predicate",
            ));
        }
        Ok(word.to_string())
    }

    /// Parse the connective between two clauses.
    ///
    /// Returns `Ok(None)` when the predicate is fully consumed, `Ok(Some(_))`
    /// for `and`/`or`, and an error for anything else.
    fn parse_connective(&mut self) -> Result<Option<Connective>, PredicateError> {
        self.skip_ws();
        if self.rest.is_empty() {
            return Ok(None);
        }
        if self.eat("&&") {
            return Ok(Some(Connective::And));
        }
        if self.eat("||") {
            return Ok(Some(Connective::Or));
        }
        let word = self.eat_word();
        match word.to_ascii_lowercase().as_str() {
            "and" => Ok(Some(Connective::And)),
            "or" => Ok(Some(Connective::Or)),
            "" => Err(PredicateError::new(format!(
                "unexpected '{}' in glyphs predicate, expected 'and' or 'or'",
                self.rest.chars().next().unwrap_or_default()
            ))),
            other => Err(PredicateError::new(format!(
                "unexpected '{other}' in glyphs predicate, expected 'and' or 'or'"
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // evaluate against a list of (id, name) pairs given in glyph order.
    fn eval(predicate: &str, glyphs: &[(u16, &str)]) -> Vec<u16> {
        Predicate::parse(predicate)
            .unwrap()
            .evaluate(glyphs.iter().map(|(id, name)| (*id, *name)))
    }

    fn names<'a>(predicate: &str, glyphs: &[(u16, &'a str)]) -> Vec<&'a str> {
        let ids = eval(predicate, glyphs);
        ids.iter()
            .map(|id| glyphs.iter().find(|(g, _)| g == id).unwrap().1)
            .collect()
    }

    fn sample() -> Vec<(u16, &'static str)> {
        // a small glyph order with arabic-ish suffixes plus some plain glyphs
        [
            "A",
            "A.sc",
            "B",
            "behDotless-ar.init",
            "behDotless-ar.init.fbeh2",
            "behDotless-ar.medi",
            "meem-ar.init",
            "meem-ar.medi",
            "ss01.a",
            "x.ss01",
        ]
        .iter()
        .enumerate()
        .map(|(i, n)| (i as u16, *n))
        .collect()
    }

    #[test]
    fn endswith_single_quote() {
        // DynaPuff form
        let glyphs = sample();
        assert_eq!(names("$[name endswith 'ss01']", &glyphs), vec!["x.ss01"]);
    }

    #[test]
    fn contains_double_quote() {
        let glyphs = sample();
        assert_eq!(
            names("$[name contains \"meem-ar\"]", &glyphs),
            vec!["meem-ar.init", "meem-ar.medi"]
        );
    }

    #[test]
    fn beginswith() {
        let glyphs = sample();
        assert_eq!(
            names("$[name beginswith \"behDotless\"]", &glyphs),
            vec![
                "behDotless-ar.init",
                "behDotless-ar.init.fbeh2",
                "behDotless-ar.medi"
            ]
        );
    }

    #[test]
    fn flat_and_with_not_equal() {
        // Nastaliq form: contains X and name != Y and name != Z
        let glyphs = sample();
        assert_eq!(
            names(
                "$[name contains \"behDotless-ar.init\" and name != \"behDotless-ar.init.fbeh2\"]",
                &glyphs
            ),
            vec!["behDotless-ar.init"]
        );
    }

    #[test]
    fn flat_or() {
        let glyphs = sample();
        assert_eq!(
            names(
                "$[name contains \"meem-ar.init\" or name contains \"meem-ar.medi\"]",
                &glyphs
            ),
            vec!["meem-ar.init", "meem-ar.medi"]
        );
    }

    #[test]
    fn or_preserves_clause_order_not_glyph_order() {
        // The advisor's case: a clause-2 match precedes a clause-1 match in glyph
        // order. glyphsLib emits clause-1 matches first, THEN clause-2's new
        // matches -- NOT pure glyph order. Glyph order here is [medi(0), init(1)].
        let glyphs = [(0u16, "x.medi"), (1u16, "x.init")];
        // clause 1 = init, clause 2 = medi -> expect [init, medi], not [medi, init]
        assert_eq!(
            Predicate::parse("$[name endswith \".init\" or name endswith \".medi\"]")
                .unwrap()
                .evaluate(glyphs.iter().map(|(id, n)| (*id, *n))),
            vec![1, 0]
        );
    }

    #[test]
    fn or_dedups() {
        // a glyph matching both clauses appears once, at its first-clause position
        let glyphs = [(0u16, "ab"), (1u16, "ba")];
        assert_eq!(
            Predicate::parse("$[name contains \"a\" or name contains \"b\"]")
                .unwrap()
                .evaluate(glyphs.iter().map(|(id, n)| (*id, *n))),
            vec![0, 1]
        );
    }

    #[test]
    fn empty_result_is_empty() {
        let glyphs = sample();
        assert!(eval("$[name endswith \"zzzz\"]", &glyphs).is_empty());
    }

    #[test]
    fn operator_keywords_case_insensitive() {
        let glyphs = sample();
        assert_eq!(names("$[name ENDSWITH 'ss01']", &glyphs), vec!["x.ss01"]);
        assert_eq!(
            names(
                "$[name contains \"meem-ar.init\" OR name contains \"meem-ar.medi\"]",
                &glyphs
            ),
            vec!["meem-ar.init", "meem-ar.medi"]
        );
    }

    #[test]
    fn value_case_sensitive() {
        let glyphs = [(0u16, "A.sc"), (1u16, "a.sc")];
        assert_eq!(
            Predicate::parse("$[name beginswith \"A\"]")
                .unwrap()
                .evaluate(glyphs.iter().map(|(id, n)| (*id, *n))),
            vec![0]
        );
    }

    #[test]
    fn symbolic_aliases() {
        let glyphs = [(0u16, "a"), (1u16, "b")];
        let g = |p: &str| {
            Predicate::parse(p)
                .unwrap()
                .evaluate(glyphs.iter().map(|(id, n)| (*id, *n)))
        };
        assert_eq!(g("$[name = \"a\"]"), vec![0]);
        assert_eq!(g("$[name == \"a\"]"), vec![0]);
        assert_eq!(g("$[name != \"a\"]"), vec![1]);
        assert_eq!(g("$[name <> \"a\"]"), vec![1]);
    }

    // ---- Phase 2 / error cases: must be rejected cleanly ----

    fn err(predicate: &str) -> String {
        Predicate::parse(predicate).unwrap_err().message
    }

    #[test]
    fn rejects_mixed_and_or() {
        assert!(
            err("$[name contains \"a\" and name contains \"b\" or name contains \"c\"]")
                .contains("mixing 'and' and 'or'")
        );
    }

    #[test]
    fn rejects_non_name_attribute() {
        assert!(err("$[category like \"Letter\"]").contains("only 'name' is supported"));
        assert!(err("$[case == upper]").contains("only 'name' is supported"));
    }

    #[test]
    fn rejects_like_and_matches() {
        assert!(err("$[name like \"A*\"]").contains("like"));
        assert!(err("$[name matches \"A.*\"]").contains("matches"));
    }

    #[test]
    fn rejects_not() {
        assert!(err("$[not name endswith \".sc\"]").contains("not"));
    }

    #[test]
    fn rejects_parens() {
        assert!(err("$[(name endswith \".sc\")]").contains("parentheses"));
    }

    #[test]
    fn rejects_comparison_operators() {
        assert!(err("$[name >= \"a\"]").contains(">="));
        // the '=>' / '=<' aliases must report as unsupported, not parse as '='
        assert!(err("$[name => \"a\"]").contains("=>"));
        assert!(err("$[name <= \"a\"]").contains("<="));
    }

    #[test]
    fn rejects_garbage() {
        assert!(Predicate::parse("$[name endswith \"a\" frobnicate]").is_err());
        assert!(Predicate::parse("$[]").is_err());
        assert!(Predicate::parse("not a predicate").is_err());
    }
}
