//! Conversion and evaluation of Glyphs.app glyph predicate tokens (`$[...]`).
//!
//! Glyphs.app sources can use a predicate token inside a FEA glyph class to
//! select glyphs by an `NSPredicate`-style expression, e.g.
//! `@ss01 = [ $[name endswith 'ss01'] ];`. At compile time we expand the token
//! into the matching glyphs, in glyph order.
//!
//! This is "Phase 1" of <https://github.com/googlefonts/fontc/issues/92>: only
//! the `name` attribute is supported, with the string operators `beginswith`,
//! `endswith`, `contains`, `==`/`=`, `!=`/`<>`, the relational operators `<`,
//! `<=`, `>`, `>=` (lexicographic on the name), joined by a flat chain of either
//! `and`/`&&` or `or`/`||` (but not a mix of the two). Anything else -- other
//! attributes (`category`, `case`, `unicode`, ...), the `like`/`matches`
//! operators, bare boolean or numeric values, `not`, parentheses, or a mix of
//! `and` and `or` -- is rejected, and is tracked as Phase 2 in
//! <https://github.com/googlefonts/fontc/issues/2052>.
//!
//! The grammar builds a typed [`typed::GlyphsAppPredicate`] AST; validation
//! (`compile::validate`) enforces the Phase 1 subset with diagnostics attached
//! to the offending child, and [`Predicate::from_typed`] converts the
//! already-validated tree into the little evaluator below. `from_typed`'s
//! `Result` is only a defensive guard against a broken invariant -- validation
//! reports the user-facing errors first and halts compilation.
//!
//! The reference implementation is glyphsLib's `TokenExpander`
//! (`Lib/glyphsLib/builder/tokens.py`); we mirror its semantics: operator and
//! boolean keywords are case-insensitive, value comparisons are case-sensitive,
//! both single and double quotes are accepted, and matches are emitted in glyph
//! order, de-duplicated. For `or`, glyphsLib accumulates clause-by-clause (all
//! of clause 1's matches in glyph order, then clause 2's new matches, ...)
//! rather than a single glyph-order pass; we reproduce that exactly because the
//! resulting class member order is observable in the compiled tables.
//!
//! glyphsLib types a bare value that *starts* with `yes`/`true`/`no`/`false`
//! (case-insensitively -- its boolean regexes have no word boundary) as a
//! boolean, and a bare digit run as an integer, both *before* its string
//! fallback. So `name == yes` -- and `name == noon`, which it reads as the
//! boolean `False` with the trailing `on` silently dropped -- compares a string
//! against a bool and matches nothing. We reject those bare spellings with a
//! "quote the value" diagnostic rather than silently treating them as the
//! strings `"yes"`/`"noon"`/`"123"`, which would diverge from the reference
//! build.

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

/// An error encountered while converting a predicate token.
///
/// The caller is responsible for attaching a source range; the message is
/// self-contained. In practice validation reports the user-facing diagnostic
/// on the offending child first, so a conversion error is a defensive fallback.
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
    /// Convert a validated typed predicate node into an evaluable [`Predicate`].
    ///
    /// Validation (`compile::validate`) has already checked the Phase 1 subset
    /// and reported any boundary error on the offending child; the `Result`
    /// here only guards against a broken invariant so we never panic.
    pub(crate) fn from_typed(node: &typed::GlyphsAppPredicate) -> Result<Self, PredicateError> {
        let clauses = node
            .clauses()
            .map(|clause| clause_from_typed(&clause))
            .collect::<Result<Vec<_>, _>>()?;
        if clauses.is_empty() {
            return Err(PredicateError::new("empty glyphs predicate"));
        }

        let mut connective: Option<Connective> = None;
        for conn in node.connectives() {
            let this = match conn {
                typed::GlyphsAppPredicateConnective::And(_)
                | typed::GlyphsAppPredicateConnective::AndAnd(_) => Connective::And,
                typed::GlyphsAppPredicateConnective::Or(_)
                | typed::GlyphsAppPredicateConnective::OrOr(_) => Connective::Or,
            };
            match connective {
                None => connective = Some(this),
                Some(existing) if existing != this => {
                    return Err(PredicateError::new(
                        "mixing 'and' and 'or' in a glyphs predicate is not yet \
                         supported (see fontc#2052)",
                    ));
                }
                Some(_) => {}
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

/// Whether a bare value would be typed as a boolean constant by glyphsLib.
///
/// glyphsLib matches `yes`/`true`/`no`/`false` case-insensitively as a *prefix*
/// (its boolean regexes have no word boundary, and run before the string
/// fallback), so a bare value like `noon`, `note`, `notdef`, `nonbreakingspace`,
/// or `Yes1` is typed as a Python bool with the trailing text silently dropped.
/// Such a value then matches no glyph name under `==` (and every name under
/// `!=`), so treating it as a plain string would silently diverge from the
/// reference build; we reject it with a quoting diagnostic instead. Quoting the
/// value bypasses the boolean typing in both toolchains, so it is always the fix.
pub(crate) fn starts_with_boolean_word(text: &str) -> bool {
    let lower = text.to_ascii_lowercase();
    ["yes", "true", "no", "false"]
        .iter()
        .any(|word| lower.starts_with(word))
}

fn clause_from_typed(clause: &typed::GlyphsAppPredicateClause) -> Result<Clause, PredicateError> {
    let attr = clause
        .attr()
        .ok_or_else(|| PredicateError::new("missing attribute in glyphs predicate clause"))?;
    // glyphsLib's object regex is case-sensitive: `name` is valid, `NAME` is not.
    if attr.text() != "name" {
        return Err(PredicateError::new(format!(
            "unsupported glyphs predicate attribute '{}': only 'name' is supported \
             (see fontc#2052)",
            attr.text()
        )));
    }
    let op = clause
        .op()
        .ok_or_else(|| PredicateError::new("missing operator in glyphs predicate clause"))?;
    let op = op_from_typed(&op)?;
    let value = clause
        .value()
        .ok_or_else(|| PredicateError::new("missing value in glyphs predicate clause"))?;
    let value = value_from_typed(&value)?;
    Ok(Clause { op, value })
}

fn op_from_typed(op: &typed::GlyphsAppPredicateOp) -> Result<Op, PredicateError> {
    use typed::GlyphsAppPredicateOp as T;
    Ok(match op {
        T::BeginsWith(_) => Op::BeginsWith,
        T::EndsWith(_) => Op::EndsWith,
        T::Contains(_) => Op::Contains,
        T::EqualEqual(_) | T::Equal(_) => Op::Eq,
        T::NotEqual(_) | T::AngleNotEqual(_) => Op::NotEq,
        T::LessThan(_) => Op::LessThan,
        T::LessThanOrEqual(_) => Op::LessThanOrEqual,
        T::GreaterThan(_) => Op::GreaterThan,
        T::GreaterThanOrEqual(_) => Op::GreaterThanOrEqual,
        T::Like(_) | T::Matches(_) => {
            return Err(PredicateError::new(format!(
                "unsupported glyphs predicate operator '{}' (see fontc#2052)",
                op.text()
            )));
        }
        T::UnknownKeyword(_) => {
            return Err(PredicateError::new(format!(
                "unknown glyphs predicate operator '{}'",
                op.text()
            )));
        }
    })
}

fn value_from_typed(value: &typed::GlyphsAppPredicateValue) -> Result<String, PredicateError> {
    use typed::GlyphsAppPredicateValue as T;
    let text = value.text();
    match value {
        T::Number(_) => Err(PredicateError::new(
            "numeric values in glyphs predicates must be quoted (see fontc#2052)",
        )),
        T::Bare(_) if starts_with_boolean_word(&text) => Err(PredicateError::new(format!(
            "the bare value '{text}' is treated as a boolean by Glyphs and must be \
             quoted (see fontc#2052)"
        ))),
        _ if text.is_empty() => Err(PredicateError::new("empty value in glyphs predicate")),
        _ => Ok(text),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_tree::typed::AstNode;

    /// Parse `$[inner]` through the real lexer + grammar and convert the typed
    /// node, so these tests exercise the same path the compiler uses.
    fn convert(inner: &str) -> Predicate {
        let (result, diags) = try_convert(inner);
        assert!(
            !diags.has_errors(),
            "`{inner}` produced parse errors that would stop real compilation, so \
             this evaluator test would be exercising a recovered parse: {}",
            diags.to_string(false)
        );
        match result {
            Some(result) => {
                result.unwrap_or_else(|e| panic!("converting `{inner}` failed: {}", e.message))
            }
            None => panic!("`{inner}` did not parse as a predicate"),
        }
    }

    fn try_convert(
        inner: &str,
    ) -> (
        Option<Result<Predicate, PredicateError>>,
        crate::DiagnosticSet,
    ) {
        let src = format!("@t = [$[{inner}]];\n");
        let (tree, diags) = crate::parse::parse_string(src);
        let node = find_predicate(tree.root());
        (node.map(|node| Predicate::from_typed(&node)), diags)
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
        // Nastaliq form: contains X and name != Y and name != Z
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

    // ---- conversion rejections: constructs deferred to fontc#2052 ----
    //
    // (Structurally foreign constructs -- `not`, parens, missing value -- are
    // rejected earlier, by the grammar, and never reach conversion.)

    fn convert_err(inner: &str) -> String {
        let (result, diags) = try_convert(inner);
        assert!(
            !diags.has_errors(),
            "`{inner}` produced parse errors, but this test asserts a *conversion* \
             rejection of a cleanly-parsed predicate: {}",
            diags.to_string(false)
        );
        match result {
            Some(Err(err)) => err.message,
            Some(Ok(_)) => panic!("`{inner}` unexpectedly converted successfully"),
            None => panic!("`{inner}` did not parse as a predicate"),
        }
    }

    #[test]
    fn rejects_mixed_and_or() {
        assert!(
            convert_err("name contains \"a\" and name contains \"b\" or name contains \"c\"")
                .contains("mixing 'and' and 'or'")
        );
    }

    #[test]
    fn rejects_non_name_attribute() {
        assert!(convert_err("category == \"Letter\"").contains("only 'name' is supported"));
        // case-sensitive: uppercase NAME is not the `name` object
        assert!(convert_err("NAME == \"x\"").contains("only 'name' is supported"));
    }

    #[test]
    fn rejects_like_and_matches() {
        assert!(convert_err("name like \"A\"").contains("like"));
        assert!(convert_err("name matches \"A\"").contains("matches"));
    }

    #[test]
    fn rejects_bare_boolean_and_numeric_values() {
        // glyphsLib types these bool/int, so they match nothing; reject rather
        // than silently treat them as the strings "yes"/"123".
        assert!(convert_err("name == yes").contains("must be quoted"));
        assert!(convert_err("name == 123").contains("must be quoted"));
        // glyphsLib types Unicode decimal runs as integers too (int("٤") == 4)
        assert!(convert_err("name == ٤").contains("must be quoted"));
    }

    #[test]
    fn rejects_bare_boolean_prefix_values() {
        // glyphsLib's boolean regexes have no word boundary, so a bare value
        // that merely *starts* with yes/true/no/false is typed as a Python bool
        // (rest dropped) and matches no glyph name: `noon`/`note`/`notdef`/
        // `nonbreakingspace` read as `False`, `Yes1`/`trueType` as `True`. These
        // are realistic ASCII glyph names, so silently treating them as strings
        // would diverge (== would select the literal glyph, != would omit it);
        // reject with a quoting diagnostic instead.
        for value in [
            "noon",
            "note",
            "notdef",
            "nonbreakingspace",
            "Yes1",
            "trueType",
        ] {
            assert!(
                convert_err(&format!("name == {value}")).contains("must be quoted"),
                "bare `{value}` (a boolean-prefixed word) should be rejected"
            );
        }
        // Quoting bypasses the boolean typing and selects the named glyph, as it
        // does in glyphsLib.
        let glyphs = [(0u16, "noon"), (1u16, "a")];
        assert_eq!(eval("name == \"noon\"", &glyphs), vec![0]);
    }
}
