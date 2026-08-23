//! Evaluation of Glyphs.app glyph predicate tokens (`$[...]`).
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
//! to the offending child, and [`evaluate_predicate`] runs the
//! already-validated tree directly. Like the rest of the compiler, evaluation
//! trusts validation: an out-of-scope predicate that reaches it is a bug, and
//! panics.
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

use smol_str::SmolStr;

use crate::typed;

/// The boolean connective joining the clauses of a predicate, reduced to the
/// single evaluation strategy it implies.
///
/// Phase 1 only supports a flat chain of a single connective; mixing `and` and
/// `or` is rejected during validation. A single clause evaluates as `And`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Connective {
    And,
    Or,
}

/// Evaluate a validated predicate against a glyph set.
///
/// `glyphs` yields `(id, name)` pairs and MUST be in glyph (GID) order; the
/// returned ids preserve that order. Results are de-duplicated.
///
/// Validation (`compile::validate`) has already rejected anything outside the
/// Phase 1 subset, so evaluation trusts its input like the rest of the
/// compiler; a predicate that violates that invariant is a bug and panics.
///
/// glyphsLib emits predicate matches in *source* glyph order. We only have
/// the GID-ordered glyph map, which equals source order in the common case
/// but not for a source with a custom `glyphOrder` parameter that reorders
/// glyphs relative to the source; in that case a class whose member order is
/// observable (e.g. a parallel class-to-class substitution) could diverge.
/// Resolving this would require threading source order through to fea-rs.
pub(crate) fn evaluate_predicate<'a, T>(
    node: &typed::GlyphsAppPredicate,
    glyphs: impl IntoIterator<Item = (T, &'a str)>,
) -> Vec<T>
where
    T: Copy + Eq + Hash,
{
    // Hoist each clause's operator and value out of the per-glyph loops;
    // `value.text()` allocates, so compute it exactly once per clause.
    let clauses: Vec<(typed::GlyphsAppPredicateOp, SmolStr)> = node
        .clauses()
        .map(|clause| {
            // glyphsLib's object regex is case-sensitive: `name` is valid,
            // `NAME` is not.
            assert_eq!(
                clause.attr().text(),
                "name",
                "non-'name' attributes are rejected by validation"
            );
            (clause.op(), clause.value().text().into())
        })
        .collect();
    assert!(!clauses.is_empty(), "empty predicates are a parse error");

    let connective = node
        .connectives()
        .map(|conn| match conn {
            typed::GlyphsAppPredicateConnective::And(_) => Connective::And,
            typed::GlyphsAppPredicateConnective::Or(_) => Connective::Or,
        })
        .reduce(|prev, this| {
            assert_eq!(prev, this, "mixed connectives are rejected by validation");
            this
        })
        .unwrap_or(Connective::And);

    let glyphs: Vec<(T, &str)> = glyphs.into_iter().collect();
    match connective {
        // glyphsLib appends each clause's matches in turn, so a glyph that
        // matches an earlier clause keeps its earlier position. A single
        // glyph-order pass would re-interleave them; this does not.
        Connective::Or => {
            let mut seen = HashSet::new();
            let mut out = Vec::new();
            for (op, value) in &clauses {
                for (id, name) in &glyphs {
                    if op_matches(op, name, value) && seen.insert(*id) {
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
        Connective::And => glyphs
            .iter()
            .filter(|(_, name)| {
                clauses
                    .iter()
                    .all(|(op, value)| op_matches(op, name, value))
            })
            .map(|(id, _)| *id)
            .collect(),
    }
}

/// Whether `name` satisfies `name <op> "value"`.
fn op_matches(op: &typed::GlyphsAppPredicateOp, name: &str, value: &str) -> bool {
    use typed::GlyphsAppPredicateOp as Op;
    match op {
        Op::BeginsWith(_) => name.starts_with(value),
        Op::EndsWith(_) => name.ends_with(value),
        Op::Contains(_) => name.contains(value),
        Op::Eq(_) => name == value,
        Op::Ne(_) => name != value,
        // glyphsLib compares `name` (a string) against the value with
        // Python's `<`/`<=`/`>`/`>=`, i.e. lexicographic string ordering.
        // Rust's `str` ordering is UTF-8 byte order, which equals Unicode
        // code point order, matching Python for every valid string.
        Op::Lt(_) => name < value,
        Op::Le(_) => name <= value,
        Op::Gt(_) => name > value,
        Op::Ge(_) => name >= value,
        Op::Like(_) | Op::Matches(_) => {
            unreachable!("like/matches are rejected by validation")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_tree::typed::AstNode;

    /// Parse `$[inner]` through the real lexer + grammar, so these tests
    /// exercise the same path the compiler uses.
    fn parse_predicate(inner: &str) -> typed::GlyphsAppPredicate {
        let src = format!("$[{inner}]");
        let (node, diags, err_str) = crate::parse::grammar::debug_parse_output(&src, |parser| {
            crate::parse::grammar::eat_glyphs_predicate(parser, crate::TokenSet::EMPTY);
        });
        assert!(
            !diags.iter().any(|diag| diag.is_error()),
            "`{inner}` produced parse errors that would stop real compilation, so \
             this evaluator test would be exercising a recovered parse:\n{err_str}"
        );
        typed::GlyphsAppPredicate::cast(&node)
            .unwrap_or_else(|| panic!("`{inner}` did not parse as a predicate"))
    }

    // evaluate against a list of (id, name) pairs given in glyph order.
    fn eval(inner: &str, glyphs: &[(u16, &str)]) -> Vec<u16> {
        evaluate_predicate(
            &parse_predicate(inner),
            glyphs.iter().map(|(id, name)| (*id, *name)),
        )
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
