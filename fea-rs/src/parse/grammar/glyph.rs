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
// this phase. Predicate validation deliberately owns the supported subset;
// this grammar only accepts its structural surface, one whole token at a time.
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
//  - Trailing tokens after a complete clause are a parse error; glyphsLib
//    silently drops whatever its capture leaves unconsumed. fontc reports the
//    problem rather than quietly selecting a different set.
//  - A bare value is a single word-shaped token, typed as a number when it
//    starts with a digit -- ASCII or Unicode, since glyphsLib types `\d+` runs
//    as integers (`int("٤") == 4`). This over-types values starting with a
//    non-digit numeric (`¼`), which glyphsLib would accept as a string; they
//    get the quoting diagnostic, and the quoted form selects identically in
//    both toolchains. A value the FEA lexer split into several tokens (`09`,
//    `123abc`) is not a value.
//
// None of these lets a predicate that both toolchains accept select different
// glyphs: where glyphsLib silently evaluates something other than what was
// written (dropped trailing input, boolean/integer typing of bare words),
// fontc reports an error instead. They are documented, not fixed.
fn eat_glyphs_predicate(parser: &mut Parser, recovery: TokenSet) -> bool {
    let recovery = recovery.add(Kind::RSquare);
    parser.in_node(AstKind::GlyphsPredicateNode, |parser| {
        // The caller only enters on an adjacent `$[`; pin that invariant in
        // debug builds but eat gracefully so a drifted guard cannot panic.
        debug_assert!(parser.matches(0, Kind::Dollar) && parser.matches(1, Kind::LSquare));
        parser.eat(Kind::Dollar);
        parser.eat(Kind::LSquare);

        if !eat_glyphs_predicate_clause(parser, recovery) {
            parser.eat_until(recovery);
            parser.expect_recover(Kind::RSquare, recovery);
            return;
        }

        while !parser.matches(0, Kind::RSquare) && !parser.at_eof() {
            if !eat_glyphs_predicate_connective(parser) {
                parser.err("expected predicate connective");
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
            "not predicates are not yet supported (see fontc#2052)",
            recovery,
        );
        return false;
    }

    parser.in_node(AstKind::GlyphsPredicateClauseNode, |parser| {
        if !eat_glyphs_predicate_attr(parser) {
            parser.err("expected predicate attribute");
            return false;
        }
        if !eat_glyphs_predicate_op(parser, recovery) {
            parser.err("expected predicate operator");
            return false;
        }
        if !eat_glyphs_predicate_value(parser, recovery) {
            parser.err("expected predicate value");
            return false;
        }
        true
    })
}

fn eat_glyphs_predicate_attr(parser: &mut Parser) -> bool {
    if !is_glyphs_predicate_word(parser.current_token_text()) {
        return false;
    }
    parser.eat_remap(parser.nth(0).kind, AstKind::GlyphsPredicateAttr)
}

fn eat_glyphs_predicate_op(parser: &mut Parser, _recovery: TokenSet) -> bool {
    let is_word = is_glyphs_predicate_word(parser.current_token_text());
    let is_symbol = parser.matches(
        0,
        TokenSet::new(&[Kind::Eq, Kind::Bang, Kind::LAngle, Kind::RAngle]),
    );
    if !is_word && !is_symbol {
        return false;
    }
    // A `!` is an operator only as the start of an adjacent `!=`.
    if parser.matches(0, Kind::Bang)
        && !(parser.matches(1, Kind::Eq) && glyphs_predicate_tokens_are_adjacent(parser, 0, 1))
    {
        return false;
    }

    // Composite symbolic operators are two adjacent tokens; check adjacency of
    // the pair (as the `!=` guard above does) before eating, so all sites share
    // `glyphs_predicate_tokens_are_adjacent` rather than re-deriving offsets.
    parser.in_node(AstKind::GlyphsPredicateOpNode, |parser| {
        if is_word {
            parser.eat_raw();
            true
        } else if parser.matches(0, Kind::Eq) {
            // `==`, or `=>`/`=<` as the NSPredicate spellings of `>=`/`<=`.
            let composite = parser
                .matches(1, TokenSet::new(&[Kind::Eq, Kind::RAngle, Kind::LAngle]))
                && glyphs_predicate_tokens_are_adjacent(parser, 0, 1);
            parser.eat(Kind::Eq);
            if composite {
                parser.eat_raw();
            }
            true
        } else if parser.matches(0, Kind::Bang) {
            // `!=`; adjacency was checked in the guard above.
            parser.eat(Kind::Bang);
            parser.eat(Kind::Eq)
        } else if parser.matches(0, Kind::LAngle) {
            // `<`, `<=`, or `<>`.
            let composite = parser.matches(1, TokenSet::new(&[Kind::Eq, Kind::RAngle]))
                && glyphs_predicate_tokens_are_adjacent(parser, 0, 1);
            parser.eat(Kind::LAngle);
            if composite {
                parser.eat_raw();
            }
            true
        } else {
            // `>` or `>=`.
            debug_assert!(parser.matches(0, Kind::RAngle));
            let composite =
                parser.matches(1, Kind::Eq) && glyphs_predicate_tokens_are_adjacent(parser, 0, 1);
            parser.eat(Kind::RAngle);
            if composite {
                parser.eat(Kind::Eq);
            }
            true
        }
    })
}

fn eat_glyphs_predicate_value(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.matches(0, Kind::String) || parser.matches(0, Kind::SingleQuote) {
        return parser.in_node(AstKind::GlyphsPredicateValueNode, |parser| {
            if parser.eat(Kind::String) {
                return true;
            }
            debug_assert!(parser.matches(0, Kind::SingleQuote));
            parser.eat(Kind::SingleQuote);
            // A `#` at a token boundary is still a FEA comment token, unlike a
            // double-quoted string. It therefore consumes the closing quote and
            // makes the single-quoted predicate value malformed.
            //
            // The quoted content is otherwise opaque: scan to the closing quote
            // with predicate-local stop points only, not the caller's
            // statement-level recovery set, whose keywords (e.g. `by` in a GSUB
            // rule) may legitimately appear inside the quotes. `]` still bounds
            // the scan; glyphsLib's `$[([^\]]+)]` capture cannot contain one
            // either.
            parser.eat_until(TokenSet::new(&[Kind::SingleQuote, Kind::RSquare]));
            parser.expect_recover(Kind::SingleQuote, recovery);
            true
        });
    }

    // A bare word or bare number value is a single word-shaped token. Float and
    // hex lexemes (`1.5`, `0x10`) are excluded by D1; of the rest, a token
    // starting with a digit is a number -- ASCII or Unicode, since glyphsLib
    // types `\d+` runs as integers. `char::is_numeric` is broader than
    // Python's `\d`: it also catches non-digit numerics like `¼`, which
    // glyphsLib would accept as a bare string. Those get the quoting
    // diagnostic, and the quoted spelling selects identically in both
    // toolchains, so the overshoot cannot diverge silently.
    // A source value the FEA lexer split across several tokens (`09`,
    // `123abc`) is not rejoined -- see the divergence note above.
    if parser.matches(0, TokenSet::new(&[Kind::Float, Kind::Hex]))
        || !is_glyphs_predicate_word(parser.current_token_text())
    {
        return false;
    }
    let target = if parser
        .current_token_text()
        .chars()
        .next()
        .is_some_and(char::is_numeric)
    {
        AstKind::Number
    } else {
        AstKind::Ident
    };

    parser.in_node(AstKind::GlyphsPredicateValueNode, |parser| {
        parser.eat_remap(parser.nth(0).kind, target)
    })
}

fn eat_glyphs_predicate_connective(parser: &mut Parser) -> bool {
    // Whole-token connectives only: `and`/`or` in any case, or a standalone
    // `&&`/`||` token. Spaced `&&`/`||` lex as their own `Ident` tokens; an
    // unspaced form glued to a value is a single foreign token and is rejected.
    let text = parser.current_token_text();
    if text.eq_ignore_ascii_case("and")
        || text.eq_ignore_ascii_case("or")
        || text == "&&"
        || text == "||"
    {
        return parser.eat_remap(parser.nth(0).kind, AstKind::GlyphsPredicateConnective);
    }
    false
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
    use crate::token_tree::{
        AstSink,
        typed::{self, AstNode},
    };

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
    fn glyphs_predicate_has_structured_children() {
        let fea = "[a $[name != \"x\" or script == 'latn'] b]";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);

        let (node, errs, _) = sink.finish();
        assert!(errs.is_empty(), "{errs:?}");
        let class = typed::GlyphClassLiteral::try_from_node(&node).unwrap();
        let predicate = class
            .items()
            .find_map(typed::GlyphsAppPredicate::cast)
            .unwrap();
        let clauses = predicate.clauses().collect::<Vec<_>>();
        assert_eq!(clauses.len(), 2);
        assert_eq!(clauses[0].attr().unwrap().text(), "name");
        assert!(matches!(
            clauses[0].op(),
            Some(typed::GlyphsAppPredicateOp::NotEqual(_))
        ));
        assert_eq!(clauses[0].value().unwrap().text(), "x");
        assert_eq!(clauses[1].attr().unwrap().text(), "script");
        assert!(matches!(
            clauses[1].op(),
            Some(typed::GlyphsAppPredicateOp::EqualEqual(_))
        ));
        assert_eq!(clauses[1].value().unwrap().text(), "latn");
        assert!(matches!(
            predicate.connectives().next(),
            Some(typed::GlyphsAppPredicateConnective::Or(_))
        ));
    }

    #[test]
    fn glyphs_predicate_accepts_keyword_words_and_connectives() {
        // FEA keywords (`language`, `sub`, `NULL`, `mark`) are accepted as
        // predicate attributes/operators/values by text, and `&&` is a
        // connective. (The `&&` must be its own token; a glued `NULL&&mark`
        // would be a parse error.)
        let fea = "[$[language sub NULL && mark == mark]]";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);

        let (node, errs, _) = sink.finish();
        assert!(errs.is_empty(), "{errs:?}");
        let class = typed::GlyphClassLiteral::try_from_node(&node).unwrap();
        let predicate = class
            .items()
            .find_map(typed::GlyphsAppPredicate::cast)
            .unwrap();
        let clauses = predicate.clauses().collect::<Vec<_>>();

        assert_eq!(clauses.len(), 2);
        assert_eq!(clauses[0].attr().unwrap().text(), "language");
        assert_eq!(clauses[0].attr().unwrap().range(), 3..11);
        assert!(matches!(
            clauses[0].op(),
            Some(typed::GlyphsAppPredicateOp::UnknownKeyword(_))
        ));
        assert!(matches!(
            clauses[0].value(),
            Some(typed::GlyphsAppPredicateValue::Bare(_))
        ));
        assert_eq!(clauses[0].value().unwrap().text(), "NULL");
        assert_eq!(clauses[0].value().unwrap().range(), 16..20);
        assert_eq!(clauses[1].attr().unwrap().text(), "mark");
        assert_eq!(clauses[1].value().unwrap().text(), "mark");
        assert!(matches!(
            predicate.connectives().next(),
            Some(typed::GlyphsAppPredicateConnective::AndAnd(_))
        ));
    }

    #[test]
    fn glyphs_predicate_types_keyword_operators() {
        let fea = "[$[name beginswith x] $[name ENDSWITH x] $[name contains x] $[name like x] $[name MATCHES x] $[name sub x]]";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);

        let (node, errs, _) = sink.finish();
        assert!(errs.is_empty(), "{errs:?}");
        let class = typed::GlyphClassLiteral::try_from_node(&node).unwrap();
        let ops = class
            .items()
            .filter_map(typed::GlyphsAppPredicate::cast)
            .map(|predicate| predicate.clauses().next().unwrap().op().unwrap())
            .collect::<Vec<_>>();

        assert!(matches!(ops[0], typed::GlyphsAppPredicateOp::BeginsWith(_)));
        assert!(matches!(ops[1], typed::GlyphsAppPredicateOp::EndsWith(_)));
        assert!(matches!(ops[2], typed::GlyphsAppPredicateOp::Contains(_)));
        assert!(matches!(ops[3], typed::GlyphsAppPredicateOp::Like(_)));
        assert!(matches!(ops[4], typed::GlyphsAppPredicateOp::Matches(_)));
        assert!(matches!(
            ops[5],
            typed::GlyphsAppPredicateOp::UnknownKeyword(_)
        ));
    }

    #[test]
    fn glyphs_predicate_requires_contiguous_syntax() {
        for fea in [
            "[$ [name == \"x\"]]",
            "[$[name = = \"x\"]]",
            "[$[name ! = \"x\"]]",
            "[$[name < > \"x\"]]",
            "[$[name < = \"x\"]]",
            "[$[name > = \"x\"]]",
        ] {
            let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
            let mut parser = Parser::new(fea, &mut sink);
            eat_glyph_class_list(&mut parser, TokenSet::EMPTY);
            let (_node, errs, _) = sink.finish();
            assert!(!errs.is_empty(), "{fea}");
        }
    }

    #[test]
    fn glyph_names_with_vertical_bars_remain_valid() {
        let fea = "[a|b a||b]";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);
        let (_node, errs, _) = sink.finish();
        assert!(errs.is_empty(), "{errs:?}");
    }

    #[test]
    fn unterminated_glyphs_predicate_errors() {
        let fea = "[a $[name endswith \".sc\"";
        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        eat_glyph_class_list(&mut parser, TokenSet::EMPTY);
        let (_node, errs, _) = sink.finish();
        assert!(
            errs.iter()
                .any(|err| err.text().contains("Expected ] found EOF")),
            "{errs:?}"
        );
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
