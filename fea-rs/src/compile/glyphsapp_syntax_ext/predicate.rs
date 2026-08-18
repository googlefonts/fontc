//! Conversion and evaluation of Glyphs.app glyph predicate tokens (`$[...]`).
//!
//! Glyphs.app sources can use a predicate token inside a FEA glyph class to
//! select glyphs by an `NSPredicate`-style expression, e.g.
//! `@ss01 = [ $[name endswith 'ss01'] ];`. At compile time we expand the token
//! into the matching glyphs, in glyph order.
//!
//! This is "Phase 1" of <https://github.com/googlefonts/fontc/issues/92>: only
//! the `name` attribute is supported, compared against a quoted string value
//! with the operators `beginswith`, `endswith`, `contains`, `==`/`=`,
//! `!=`/`<>`, `<`, `<=`, `>`, `>=` (lexicographic on the name), joined by a
//! flat chain of either `and`/`&&` or `or`/`||` (but not a mix of the two).
//! Anything else -- other attributes (`category`, `case`, `unicode`, ...), the
//! `like`/`matches` operators, unquoted values, `not`, parentheses, or a mix
//! of `and` and `or` -- is rejected, and is tracked as Phase 2 in
//! <https://github.com/googlefonts/fontc/issues/2052>.
//!
//! The grammar builds a typed [`typed::GlyphsAppPredicate`] AST; validation
//! (`compile::validate`) enforces the Phase 1 subset with diagnostics attached
//! to the offending child, and [`Predicate::from_typed`] converts the
//! already-validated tree into the little evaluator below. Like the rest of
//! the compiler, conversion trusts validation: an out-of-scope predicate that
//! reaches it is a bug, and panics.
//!
//! The reference implementation is glyphsLib's `TokenExpander`
//! (`Lib/glyphsLib/builder/tokens.py`); we mirror its semantics: operator
//! keywords are case-insensitive, value comparisons are case-sensitive,
//! both single and double quotes are accepted, and matches are emitted in glyph
//! order, de-duplicated. For `or`, glyphsLib accumulates clause-by-clause (all
//! of clause 1's matches in glyph order, then clause 2's new matches, ...)
//! rather than a single glyph-order pass; we reproduce that exactly because the
//! resulting class member order is observable in the compiled tables.
//!
//! Values must be quoted, as the Glyphs docs require for strings: glyphsLib
//! also accepts bare values, but types some of them as booleans or integers
//! rather than strings, so validation rejects every unquoted value with a
//! quoting diagnostic instead of mirroring that behavior.

use std::collections::HashSet;
use std::hash::Hash;

use crate::typed;

/// A comparison operator over a glyph's name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Op {
    BeginsWith,
    EndsWith,
    Contains,
    Eq,
    NotEq,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl Op {
    fn matches(self, name: &str, value: &str) -> bool {
        match self {
            Op::BeginsWith => name.starts_with(value),
            Op::EndsWith => name.ends_with(value),
            Op::Contains => name.contains(value),
            Op::Eq => name == value,
            Op::NotEq => name != value,
            // glyphsLib compares `name` (a string) against the value with
            // Python's `<`/`<=`/`>`/`>=`, i.e. lexicographic string ordering.
            // Rust's `str` ordering is UTF-8 byte order, which equals Unicode
            // code point order, matching Python for every valid string.
            Op::LessThan => name < value,
            Op::LessThanOrEqual => name <= value,
            Op::GreaterThan => name > value,
            Op::GreaterThanOrEqual => name >= value,
        }
    }
}

/// The boolean connective joining the clauses of a predicate.
///
/// Phase 1 only supports a flat chain of a single connective; mixing `and` and
/// `or` is rejected during validation.
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

impl Predicate {
    /// Convert a validated typed predicate node into an evaluable [`Predicate`].
    ///
    /// Validation (`compile::validate`) has already rejected anything outside
    /// the Phase 1 subset, so conversion trusts its input like the rest of the
    /// compiler; a predicate that violates that invariant is a bug and panics.
    pub(crate) fn from_typed(node: &typed::GlyphsAppPredicate) -> Self {
        let clauses: Vec<_> = node
            .clauses()
            .map(|clause| clause_from_typed(&clause))
            .collect();
        assert!(!clauses.is_empty(), "empty predicates are a parse error");

        let mut connective: Option<Connective> = None;
        for conn in node.connectives() {
            let this = match conn {
                typed::GlyphsAppPredicateConnective::And(_) => Connective::And,
                typed::GlyphsAppPredicateConnective::Or(_) => Connective::Or,
            };
            assert!(
                connective.is_none() || connective == Some(this),
                "mixed connectives are rejected by validation"
            );
            connective = Some(this);
        }
        Predicate {
            clauses,
            connective,
        }
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

fn clause_from_typed(clause: &typed::GlyphsAppPredicateClause) -> Clause {
    // a clause missing any of its parts is a parse error and never compiles
    let attr = clause.attr().expect("checked by the grammar");
    // glyphsLib's object regex is case-sensitive: `name` is valid, `NAME` is not.
    assert_eq!(
        attr.text(),
        "name",
        "non-'name' attributes are rejected by validation"
    );
    let op = op_from_typed(&clause.op().expect("checked by the grammar"));
    let value = value_from_typed(&clause.value().expect("checked by the grammar"));
    Clause { op, value }
}

fn op_from_typed(op: &typed::GlyphsAppPredicateOp) -> Op {
    use typed::GlyphsAppPredicateOp as T;
    match op {
        T::BeginsWith(_) => Op::BeginsWith,
        T::EndsWith(_) => Op::EndsWith,
        T::Contains(_) => Op::Contains,
        T::Eq(_) => Op::Eq,
        T::Ne(_) => Op::NotEq,
        T::Lt(_) => Op::LessThan,
        T::Le(_) => Op::LessThanOrEqual,
        T::Gt(_) => Op::GreaterThan,
        T::Ge(_) => Op::GreaterThanOrEqual,
        T::Like(_) | T::Matches(_) => {
            unreachable!("like/matches are rejected by validation")
        }
    }
}

fn value_from_typed(value: &typed::GlyphsAppPredicateValue) -> String {
    use typed::GlyphsAppPredicateValue as T;
    let text = value.text();
    match value {
        T::Bare(_) | T::Number(_) => {
            unreachable!("unquoted values are rejected by validation")
        }
        _ => {
            assert!(!text.is_empty(), "empty values are rejected by validation");
            text
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_tree::typed::AstNode;

    /// Parse `$[inner]` through the real lexer + grammar and convert the typed
    /// node, so these tests exercise the same path the compiler uses.
    fn convert(inner: &str) -> Predicate {
        let src = format!("@t = [$[{inner}]];\n");
        let (tree, diags) = crate::parse::parse_string(src);
        assert!(
            !diags.has_errors(),
            "`{inner}` produced parse errors that would stop real compilation, so \
             this evaluator test would be exercising a recovered parse: {}",
            diags.to_string(false)
        );
        let node = find_predicate(tree.root())
            .unwrap_or_else(|| panic!("`{inner}` did not parse as a predicate"));
        Predicate::from_typed(&node)
    }

    fn find_predicate(node: &crate::Node) -> Option<typed::GlyphsAppPredicate> {
        for child in node.iter_children() {
            if let Some(pred) = typed::GlyphsAppPredicate::cast(child) {
                return Some(pred);
            }
            if let Some(inner) = child.as_node()
                && let Some(found) = find_predicate(inner)
            {
                return Some(found);
            }
        }
        None
    }

    // evaluate against a list of (id, name) pairs given in glyph order.
    fn eval(inner: &str, glyphs: &[(u16, &str)]) -> Vec<u16> {
        convert(inner).evaluate(glyphs.iter().map(|(id, name)| (*id, *name)))
    }

    fn names<'a>(inner: &str, glyphs: &[(u16, &'a str)]) -> Vec<&'a str> {
        let ids = eval(inner, glyphs);
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
        assert_eq!(names("name endswith 'ss01'", &glyphs), vec!["x.ss01"]);
    }

    #[test]
    fn contains_double_quote() {
        let glyphs = sample();
        assert_eq!(
            names("name contains \"meem-ar\"", &glyphs),
            vec!["meem-ar.init", "meem-ar.medi"]
        );
    }

    #[test]
    fn beginswith() {
        let glyphs = sample();
        assert_eq!(
            names("name beginswith \"behDotless\"", &glyphs),
            vec![
                "behDotless-ar.init",
                "behDotless-ar.init.fbeh2",
                "behDotless-ar.medi"
            ]
        );
    }

    #[test]
    fn flat_and_with_not_equal() {
        // Noto Nastaliq Urdu form: contains X and name != Y and name != Z
        let glyphs = sample();
        assert_eq!(
            names(
                "name contains \"behDotless-ar.init\" and name != \"behDotless-ar.init.fbeh2\"",
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
                "name contains \"meem-ar.init\" or name contains \"meem-ar.medi\"",
                &glyphs
            ),
            vec!["meem-ar.init", "meem-ar.medi"]
        );
    }

    #[test]
    fn or_preserves_clause_order_not_glyph_order() {
        // A clause-2 match precedes a clause-1 match in glyph order. glyphsLib
        // emits clause-1 matches first, THEN clause-2's new matches -- NOT pure
        // glyph order. Glyph order here is [medi(0), init(1)].
        let glyphs = [(0u16, "x.medi"), (1u16, "x.init")];
        // clause 1 = init, clause 2 = medi -> expect [init, medi], not [medi, init]
        assert_eq!(
            eval(
                "name endswith \".init\" or name endswith \".medi\"",
                &glyphs
            ),
            vec![1, 0]
        );
    }

    #[test]
    fn or_dedups() {
        // a glyph matching both clauses appears once, at its first-clause position
        let glyphs = [(0u16, "ab"), (1u16, "ba")];
        assert_eq!(
            eval("name contains \"a\" or name contains \"b\"", &glyphs),
            vec![0, 1]
        );
    }

    #[test]
    fn empty_result_is_empty() {
        let glyphs = sample();
        assert!(eval("name endswith \"zzzz\"", &glyphs).is_empty());
    }

    #[test]
    fn operator_keywords_case_insensitive() {
        let glyphs = sample();
        assert_eq!(names("name ENDSWITH 'ss01'", &glyphs), vec!["x.ss01"]);
        assert_eq!(
            names(
                "name contains \"meem-ar.init\" OR name contains \"meem-ar.medi\"",
                &glyphs
            ),
            vec!["meem-ar.init", "meem-ar.medi"]
        );
    }

    #[test]
    fn value_case_sensitive() {
        let glyphs = [(0u16, "A.sc"), (1u16, "a.sc")];
        assert_eq!(eval("name beginswith \"A\"", &glyphs), vec![0]);
    }

    #[test]
    fn symbolic_aliases() {
        let glyphs = [(0u16, "a"), (1u16, "b")];
        assert_eq!(eval("name = \"a\"", &glyphs), vec![0]);
        assert_eq!(eval("name == \"a\"", &glyphs), vec![0]);
        assert_eq!(eval("name != \"a\"", &glyphs), vec![1]);
        assert_eq!(eval("name <> \"a\"", &glyphs), vec![1]);
    }

    #[test]
    fn relational_operators_are_lexicographic() {
        // brought forward from Phase 2: glyphsLib compares the name string with
        // Python's relational operators, i.e. lexicographic ordering.
        let glyphs = [(0u16, "a"), (1u16, "m"), (2u16, "z")];
        assert_eq!(eval("name < \"m\"", &glyphs), vec![0]);
        assert_eq!(eval("name <= \"m\"", &glyphs), vec![0, 1]);
        assert_eq!(eval("name > \"m\"", &glyphs), vec![2]);
        assert_eq!(eval("name >= \"m\"", &glyphs), vec![1, 2]);
    }

    #[test]
    fn quoted_boolean_word_is_a_plain_string() {
        // glyphsLib types a *bare* value starting with yes/true/no/false as a
        // boolean (validation rejects all unquoted values). Quoting bypasses
        // the boolean typing and selects the named glyph, as it does in
        // glyphsLib.
        let glyphs = [(0u16, "noon"), (1u16, "a")];
        assert_eq!(eval("name == \"noon\"", &glyphs), vec![0]);
    }
}
