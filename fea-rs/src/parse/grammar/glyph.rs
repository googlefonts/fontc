use crate::parse::{
    Parser,
    lexer::{Kind, TokenSet},
};
use crate::token_tree::Kind as AstKind;

// @class = @class;
// @class = [a b c];
// @class = [a-z A - Z];
// @class = [\1-\40 \45 - \50];
pub(crate) fn named_glyph_class_decl(parser: &mut Parser, recovery: TokenSet) {
    fn glyph_class_body(parser: &mut Parser, recovery: TokenSet) {
        assert!(parser.expect(Kind::NamedGlyphClass));
        parser.expect_recover(
            Kind::Eq,
            recovery.union(TokenSet::new(&[
                Kind::NamedGlyphClass,
                Kind::LSquare,
                Kind::Semi,
            ])),
        );

        if parser.eat(Kind::NamedGlyphClass) {
            // noop
        } else if !parser.matches(0, Kind::LSquare) {
            parser.err_recover(
                "Expected named glyph class or '['.",
                recovery.add(Kind::Semi),
            );
        } else {
            eat_glyph_class_list(parser, recovery.add(Kind::Semi));
        }
    }

    parser.in_node(AstKind::GlyphClassDefNode, |parser| {
        glyph_class_body(parser, recovery);
        parser.expect_semi();
    });
}

// B @class [a b]
pub(crate) fn eat_glyph_or_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    eat_glyph_name_like(parser) || eat_named_or_unnamed_glyph_class(parser, recovery)
}

pub(crate) fn expect_glyph_or_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    if eat_glyph_or_glyph_class(parser, recovery) {
        return true;
    }

    parser.err_recover("Expected glyph or glyph class", recovery);
    false
}

pub(crate) fn eat_named_or_unnamed_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    parser.eat(Kind::NamedGlyphClass) || eat_glyph_class_list(parser, recovery)
}

pub(crate) fn expect_named_or_unnamed_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    if eat_named_or_unnamed_glyph_class(parser, recovery) {
        return true;
    }

    parser.err_recover("Expected glyph class", recovery);
    false
}

// [ a b a-z @hi \0-\40 ]
pub(crate) fn eat_glyph_class_list(parser: &mut Parser, recovery: TokenSet) -> bool {
    let recovery = recovery.add(Kind::RSquare);
    // a `$[...]` glyphs predicate here is outside a glyph class (only class
    // *members* can be predicates); report that, but still parse it in full so
    // recovery resumes cleanly after it
    if parser.matches(0, Kind::Dollar) && parser.matches(1, Kind::LSquare) {
        let range = parser.nth_range(0).start..parser.nth_range(1).end;
        parser.raw_error(
            range,
            "glyphs predicates are only supported inside a glyph class",
        );
        eat_glyphs_predicate(parser, recovery);
        return true;
    }
    if !parser.matches(0, Kind::LSquare) {
        return false;
    }

    parser.in_node(AstKind::GlyphClass, |parser| {
        let range = parser.nth_range(0);
        assert!(parser.eat(Kind::LSquare));
        super::greedy(glyph_class_list_member)(parser, recovery);

        if !parser.eat(Kind::RSquare) {
            parser.err("Unexpected token, expected glyph or glyph class");
            parser.eat_until(recovery);
            if !parser.eat(Kind::RSquare) {
                parser.raw_error(range, "Unclosed glyph class.")
            }
        }
    });
    true
}

fn glyph_class_list_member(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.eat(Kind::NamedGlyphClass) {
        return true;
    }
    if parser.matches(0, Kind::Dollar)
        && parser.matches(1, Kind::LSquare)
        && glyphs_predicate_tokens_are_adjacent(parser, 0, 1)
    {
        return eat_glyphs_predicate(parser, recovery);
    }
    // a glyphname
    // a glyph development name
    // an escaped glyph name
    // an escaped CID

    let looks_like_range = parser.matches(1, Kind::Hyphen)
        || (parser.matches(0, Kind::Backslash) && parser.matches(2, Kind::Hyphen));
    if looks_like_range {
        parser.in_node(AstKind::GlyphRange, |parser| {
            glyph_range(parser, recovery.add(Kind::RSquare));
        });
        true
    } else {
        eat_glyph_name_like(parser)
    }
}

// A Glyphs.app glyph predicate is valid only as a member of a glyph class in
// this phase. The grammar accepts the structural surface one whole token at a
// time and classifies the operators it knows; which attributes, values and
// known operators are actually supported is validation's to decide.
//
// Known divergences from glyphsLib (whose regex `\$\[([^\]]+)\]` captures the
// body opaquely, whereas we run it through the FEA lexer):
//
//  - A `"` or `#` inside a *single*-quoted value trips FEA's own string/comment
//    lexing and breaks the parse -- the same blast radius as any stray quote or
//    `#` elsewhere in a FEA file, and impossible in a real (glyph-name) value.
//  - A stray backslash is accepted where glyphsLib would reject it (fontc
//    accepts more, harmless).
//  - Operators and connectives must be their own tokens. An unspaced spelling
//    that packs an operator or connective into one lexer token
//    (`name contains"x"`, `x&&name`, `namecontains "x"`) is a parse error,
//    though glyphsLib's boundary-free regexes accept some of them. Every
//    operator example in the Glyphs token docs is spaced, so no real source is
//    affected.
//  - glyphsLib also accepts the `in` and `between` operators, including the
//    value-first spelling that flips `"x" in name` into `name contains "x"`.
//    Both are deferred with the rest of #2052 and rejected here at parse time:
//    `in`/`between` as an unknown operator, and the flipped spelling as a
//    missing attribute, since a value cannot open a clause.
//  - Trailing tokens after a complete clause are a parse error; glyphsLib
//    silently drops whatever its capture leaves unconsumed. fontc reports the
//    problem rather than quietly selecting a different set.
//  - A bare value is a single word-shaped token, typed as a number when it
//    starts with an ASCII digit. A dotted glyph name (`a.alt`) must be
//    quoted -- glyphsLib's `\w+` value match stops at the dot and silently
//    selects `a` -- and a value the FEA lexer split into several tokens
//    (`09`, `123abc`) is not a value.
//
// None of these lets a predicate that both toolchains accept select different
// glyphs: where glyphsLib silently evaluates something other than what was
// written (dropped trailing input, boolean/integer typing of bare words),
// fontc reports an error instead. They are documented, not fixed.
pub(crate) fn eat_glyphs_predicate(parser: &mut Parser, recovery: TokenSet) -> bool {
    let recovery = recovery.add(Kind::RSquare);
    parser.in_node(AstKind::GlyphsPredicateNode, |parser| {
        // the caller only enters on a `$` followed by `[`
        assert!(parser.eat(Kind::Dollar));
        assert!(parser.eat(Kind::LSquare));

        if !eat_glyphs_predicate_clause(parser, recovery) {
            parser.eat_until(recovery);
            parser.expect_recover(Kind::RSquare, recovery);
            return;
        }

        while !parser.matches(0, Kind::RSquare) && !parser.at_eof() {
            if !eat_glyphs_predicate_connective(parser) {
                parser.err("expected predicate connective or ']'");
                parser.eat_until(recovery);
                break;
            }
            if !eat_glyphs_predicate_clause(parser, recovery) {
                parser.eat_until(recovery);
                break;
            }
        }

        parser.expect_recover(Kind::RSquare, recovery);
    });
    true
}

fn eat_glyphs_predicate_clause(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.matches(0, Kind::LParen) {
        parser.err_recover(
            "parenthesized predicates are not yet supported (see fontc#2052)",
            recovery,
        );
        return false;
    }
    // A `not` word or a `!` (Bang) at the head of a clause is negation, deferred
    // to #2052. Both `! name` and `!name` begin with a Bang (because `!` delimits
    // idents), so the single Bang check covers both spellings.
    if parser.current_token_text().eq_ignore_ascii_case("not") || parser.matches(0, Kind::Bang) {
        parser.err_recover(
            "negation (not/!) is not yet supported in predicates (see fontc#2052)",
            recovery,
        );
        return false;
    }

    parser.in_node(AstKind::GlyphsPredicateClauseNode, |parser| {
        expect_glyphs_predicate_attr(parser)
            && expect_glyphs_predicate_op(parser, recovery)
            && expect_glyphs_predicate_value(parser, recovery)
    })
}

fn expect_glyphs_predicate_attr(parser: &mut Parser) -> bool {
    if !is_glyphs_predicate_word(parser.current_token_text()) {
        parser.err("expected predicate attribute");
        return false;
    }
    parser.eat_remap(parser.nth(0).kind, AstKind::GlyphsPredicateAttr)
}

/// Either the name of an operation (`beginswith`, `endswith`, `contains`,
/// `like` or `matches`, in any ASCII case) or one of `==`, `=`, `!=`, `<>`,
/// `<=`, `=<`, `>=`, `=>`, `<`, `>`.
///
/// Each spelling becomes a single token of its own kind, so the synonyms
/// (`=`/`==`, `!=`/`<>`, `<=`/`=<`, `>=`/`=>`) are classified in the tree while
/// the token text keeps what was written.
fn expect_glyphs_predicate_op(parser: &mut Parser, recovery: TokenSet) -> bool {
    if is_glyphs_predicate_word(parser.current_token_text()) {
        return expect_glyphs_predicate_op_name(parser);
    }

    // a symbolic operator is one or two adjacent lexemes; `=<` and `=>` are the
    // NSPredicate spellings of `<=` and `>=`. glyphsLib rejects those two (a
    // bug: its comparator regex consumes the leading `=` first), but Glyphs.app
    // accepts them, and being more permissive than glyphsLib on inputs it
    // rejects cannot make the two toolchains select different glyphs.
    if parser.eat_adjacent_remap(Kind::Eq, Kind::Eq, AstKind::GlyphsPredicateOpEq)
        || parser.eat_adjacent_remap(Kind::Bang, Kind::Eq, AstKind::GlyphsPredicateOpNe)
        || parser.eat_adjacent_remap(Kind::LAngle, Kind::RAngle, AstKind::GlyphsPredicateOpNe)
        || parser.eat_adjacent_remap(Kind::LAngle, Kind::Eq, AstKind::GlyphsPredicateOpLe)
        || parser.eat_adjacent_remap(Kind::Eq, Kind::LAngle, AstKind::GlyphsPredicateOpLe)
        || parser.eat_adjacent_remap(Kind::RAngle, Kind::Eq, AstKind::GlyphsPredicateOpGe)
        || parser.eat_adjacent_remap(Kind::Eq, Kind::RAngle, AstKind::GlyphsPredicateOpGe)
        || parser.eat_remap(Kind::Eq, AstKind::GlyphsPredicateOpEq)
        || parser.eat_remap(Kind::LAngle, AstKind::GlyphsPredicateOpLt)
        || parser.eat_remap(Kind::RAngle, AstKind::GlyphsPredicateOpGt)
    {
        return true;
    }

    parser.err_recover("expected predicate operator", recovery);
    false
}

fn expect_glyphs_predicate_op_name(parser: &mut Parser) -> bool {
    let kind = match parser.current_token_text() {
        text if text.eq_ignore_ascii_case("beginswith") => AstKind::GlyphsPredicateOpBeginsWith,
        text if text.eq_ignore_ascii_case("endswith") => AstKind::GlyphsPredicateOpEndsWith,
        text if text.eq_ignore_ascii_case("contains") => AstKind::GlyphsPredicateOpContains,
        text if text.eq_ignore_ascii_case("like") => AstKind::GlyphsPredicateOpLike,
        text if text.eq_ignore_ascii_case("matches") => AstKind::GlyphsPredicateOpMatches,
        _ => {
            parser.err_and_bump("unknown glyphs predicate operator");
            return false;
        }
    };
    parser.eat_remap(parser.nth(0).kind, kind)
}

fn expect_glyphs_predicate_value(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.eat(Kind::String) {
        return true;
    }

    if parser.matches(0, Kind::SingleQuote) {
        // Unlike a double-quoted string, a single-quoted value is not one lexer
        // token: `'` is FEA's glyph marker, and a `#` at a token boundary is
        // still a comment token, which consumes the closing quote and makes the
        // value malformed.
        //
        // The quoted content is otherwise opaque: scan to the closing quote
        // with predicate-local stop points only, not the caller's
        // statement-level recovery set, whose keywords (e.g. `by` in a GSUB
        // rule) may legitimately appear inside the quotes. `]` still bounds
        // the scan; glyphsLib's `$[([^\]]+)]` capture cannot contain one
        // either.
        return parser.in_node(AstKind::GlyphsPredicateSingleQuotedValue, |parser| {
            parser.eat(Kind::SingleQuote);
            parser.eat_until(TokenSet::new(&[Kind::SingleQuote, Kind::RSquare]));
            parser.expect_recover(Kind::SingleQuote, recovery)
        });
    }

    // A bare value is a single word-shaped token, matched on its text and
    // remapped rather than eaten by kind: the parity target is glyphsLib's
    // `\w+` (Unicode-aware), and the lexer may have typed the spelling as
    // something other than Ident/Number (keywords like `mark` and `NULL`;
    // `077` lexes as Octal). Digit-first tokens remap to Number, the rest to
    // Ident (no glyph name starts with a digit). Float and hex lexemes
    // (`1.5`, `0x10`) are not values: glyphsLib's `\d+` typing would silently
    // truncate them, so a bare digit run is the only numeric form. Spellings
    // the FEA lexer split apart (`09`, `123abc`) are not rejoined -- see the
    // divergence note above.
    if !parser.matches(0, TokenSet::new(&[Kind::Float, Kind::Hex]))
        && is_glyphs_predicate_word(parser.current_token_text())
    {
        let target = if parser
            .current_token_text()
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_digit())
        {
            AstKind::Number
        } else {
            AstKind::Ident
        };
        return parser.eat_remap(parser.nth(0).kind, target);
    }

    parser.err_recover("expected predicate value", recovery);
    false
}

fn eat_glyphs_predicate_connective(parser: &mut Parser) -> bool {
    // Whole-token connectives only: `and`/`or` in any case, or a standalone
    // `&&`/`||` token. Spaced `&&`/`||` lex as their own `Ident` tokens; an
    // unspaced form glued to a value is a single foreign token and is rejected.
    // `and`/`&&` remap to one kind and `or`/`||` to another, so the parse tree
    // carries the classification; the token text is preserved, so the spelling
    // is not lost.
    let text = parser.current_token_text();
    let kind = if text.eq_ignore_ascii_case("and") || text == "&&" {
        AstKind::GlyphsPredicateAnd
    } else if text.eq_ignore_ascii_case("or") || text == "||" {
        AstKind::GlyphsPredicateOr
    } else {
        return false;
    };
    parser.eat_remap(parser.nth(0).kind, kind)
}

fn glyphs_predicate_tokens_are_adjacent(parser: &Parser, first: usize, second: usize) -> bool {
    parser.nth_range(first).end == parser.nth_range(second).start
}

fn is_glyphs_predicate_word(text: &str) -> bool {
    // parity with glyphsLib, where the bare-value pattern is Python's `\w+`:
    // Unicode-aware, not ASCII-only
    !text.is_empty() && text.chars().all(is_glyphs_predicate_word_char)
}

fn is_glyphs_predicate_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

//TODO:  this should be eat_glyph_range, and it should do the checking that
//is currently done above?
fn glyph_range(parser: &mut Parser, recovery: TokenSet) -> bool {
    const HYPHEN: TokenSet = TokenSet::new(&[Kind::Hyphen]);

    let first_recovery = recovery.union(HYPHEN);

    expect_glyph_name_like(parser, first_recovery)
        & parser.expect_recover(Kind::Hyphen, recovery)
        & expect_glyph_name_like(parser, recovery)
}

pub(crate) fn expect_glyph_name_like(parser: &mut Parser, recovery: TokenSet) -> bool {
    if eat_glyph_name_like(parser) {
        return true;
    }

    parser.err_recover("Expected glyph name or CID", recovery);
    false
}

pub(crate) fn eat_glyph_name_like(parser: &mut Parser) -> bool {
    if parser.matches(0, TokenSet::IDENT_LIKE) {
        eat_and_validate_glyph_name(parser);
        true
    } else if parser.matches(0, Kind::NullKw) {
        // this is not technically allowed but is common in noto fonts
        // and accepted by feaLib so we will accept it as well
        parser.warn(" when used as glyph name 'NULL' should be escaped ('\\NULL')");
        parser.eat_remap(Kind::NullKw, AstKind::GlyphName);
        true
    } else {
        parser.eat(Kind::Cid)
    }
}

fn eat_and_validate_glyph_name(parser: &mut Parser) {
    debug_assert!(parser.matches(0, TokenSet::IDENT_LIKE));
    let raw = parser.nth_raw(0);
    match validate_glyph_name(raw) {
        NameType::Valid => {
            parser.eat_remap(TokenSet::IDENT_LIKE, AstKind::GlyphName);
        }
        NameType::MaybeRange => {
            parser.eat_remap(TokenSet::IDENT_LIKE, AstKind::GlyphNameOrRange);
        }
        NameType::Invalid(pos) => {
            let err = match std::str::from_utf8(&raw[pos..])
                .ok()
                .and_then(|t| t.chars().next())
            {
                Some(chr) => format!("Invalid char '{chr}' in glyph name"),
                None => "Invalid char in glyph name".to_string(),
            };
            parser.err_and_bump(err);
        }
    }
}

enum NameType {
    Valid,
    MaybeRange,
    Invalid(usize),
}

fn validate_glyph_name(name: &[u8]) -> NameType {
    fn validate_glyph_body(bytes: &[u8]) -> NameType {
        let mut range = false;
        for (idx, byte) in bytes.iter().enumerate() {
            match byte {
                b'a'..=b'z'
                | b'A'..=b'Z'
                | b'0'..=b'9'
                | b'.'
                | b'_'
                | b'*'
                | b'+'
                | b':'
                | b'^'
                | b'|'
                | b'~' => (),
                b'-' => range = true,
                _ => return NameType::Invalid(idx + 1),
            }
        }
        if range {
            NameType::MaybeRange
        } else {
            NameType::Valid
        }
    }

    let (first, rest) = name.split_first().expect("glyph names are not empty");
    match first {
        b'_' | b'a'..=b'z' | b'A'..=b'Z' => validate_glyph_body(rest),
        // .null is technically not allowed per the spec but exists in many
        // existing sources.
        b'.' if name == b".notdef" || name == b".null" => NameType::Valid,
        _ => NameType::Invalid(0),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::GlyphMap;
    use crate::parse::FileId;
    use crate::token_tree::AstSink;

    #[test]
    fn name_like() {
        let fea = "hi \\hi \\mark \\table \\12";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(0), b"hi");
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(0), b"mark");
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(0), b"table");
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(0), b"12");
        assert!(eat_glyph_name_like(&mut parser));
        assert!(!eat_glyph_name_like(&mut parser));
    }

    #[test]
    fn invalid_things() {
        let bad_glyphs = [".hi", "hî"];
        for raw in bad_glyphs {
            let mut sink = AstSink::new(raw, FileId::CURRENT_FILE, None);
            let mut parser = Parser::new(raw, &mut sink);
            eat_glyph_name_like(&mut parser);
            assert_eq!(sink.errors().len(), 1, "'{raw}'");
        }
    }

    #[test]
    fn bang_terminates_glyph_name() {
        // `hi!` once lexed as a single invalid Ident (one "invalid glyph name"
        // error). Now that `!` is `Bang` (an ident delimiter), `hi` is a valid
        // name and the stray `!` is a separate token the caller rejects later --
        // a different error shape on already-invalid input (no legal FEA has `!`).
        let fea = "hi!";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        assert!(eat_glyph_name_like(&mut parser));
        let stray_bang = parser.matches(0, Kind::Bang);
        assert!(sink.errors().is_empty(), "'hi' should parse cleanly");
        assert!(stray_bang, "the stray '!' remains as Bang");
    }

    #[test]
    fn disambiguate_range() {
        let fea = "[a-b]";

        // first we parse without a glyph map
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);

        let (node, errs, _) = sink.finish();
        assert!(errs.is_empty());
        let mut cursor = node.cursor();
        assert_eq!(cursor.next_token().unwrap().kind, AstKind::LSquare);
        let next = cursor.next_token().unwrap();
        assert_eq!(&next.text, "a-b");
        assert_eq!(next.kind, AstKind::GlyphNameOrRange);
        assert_eq!(cursor.next_token().unwrap().kind, AstKind::RSquare);

        // now we parse with a glyph map
        let glyphs = GlyphMap::new(["a", "b"]).unwrap();

        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, Some(&glyphs));
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);

        let (node, errs, _) = sink.finish();
        assert!(errs.is_empty());

        let mut cursor = node.cursor();
        assert_eq!(cursor.next_token().unwrap().kind, AstKind::LSquare);
        assert_eq!(
            cursor.current().unwrap().kind(),
            AstKind::GlyphRange,
            "{node:?}",
        );
        let next = cursor.next_token().unwrap();
        assert_eq!(&next.text, "a");
        assert_eq!(next.kind, AstKind::GlyphName);
        assert_eq!(cursor.next_token().unwrap().kind, AstKind::Hyphen);
        assert_eq!(cursor.next_token().unwrap().kind, AstKind::GlyphName);
        assert_eq!(cursor.next_token().unwrap().kind, AstKind::RSquare);
    }
}
