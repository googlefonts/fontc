use super::{glyph, metrics};

use crate::parse::{
    lexer::{Kind, TokenSet},
    Parser,
};
use crate::token_tree::Kind as AstKind;

/// Parse a single GPOS statement
// 6.a: pos <glyph|glyphclass> <valuerecord>;
// 6.b: [enum] pos <glyph|glyphclass> <valuerecord>
//          <glyph|glyphclass> <valuerecord>;
// or   [enum] pos <glyph|class> <glyph|class> <valuerecord>;
// 6.c: pos cursive <glyph|class> <anchor> <anchor>;
// 6.d: pos base <glyph|class>
//      <anchor> mark <named mark class>+ ;
// 6.e: pos ligature <glyph|class>
//      <anchor> mark <named mark class> +
//      ligComponent
//      <anchor> mark <named mark class> + ;
// 6.f: pos mark <glyph|class>
//      <anchor> mark <named mark class> + ;
pub(crate) fn gpos_rule(parser: &mut Parser, recovery: TokenSet) {
    fn gpos_body(parser: &mut Parser, recovery: TokenSet) -> AstKind {
        if parser.matches(0, Kind::IgnoreKw) {
            return parse_ignore(parser, recovery);
        }
        let recovery = recovery.union(Kind::Semi.into());
        if parser.eat(Kind::EnumKw) {
            assert!(parser.eat(Kind::PosKw));
            if glyph::expect_glyph_or_glyph_class(parser, recovery)
                && glyph::expect_glyph_or_glyph_class(parser, recovery)
                && metrics::expect_value_record(parser, recovery)
                && parser.expect_semi()
            {
                return AstKind::GposType2;
            } else {
                return AstKind::GposNode;
            }
        }
        assert!(parser.eat(Kind::PosKw));
        if parser.matches(0, Kind::CursiveKw) {
            gpos_cursive(parser, recovery);
            AstKind::GposType3
        } else if parser.matches(0, Kind::MarkKw) {
            gpos_mark_to_mark(parser, recovery);
            AstKind::GposType6
        } else if parser.nth_raw(0) == b"base" {
            gpos_mark_to_base(parser, recovery);
            AstKind::GposType4
        } else if parser.nth_raw(0) == b"ligature" {
            gpos_ligature(parser, recovery);
            AstKind::GposType5
        } else {
            // all other statements start with glyph or glyph class:
            if !glyph::expect_glyph_or_glyph_class(parser, recovery) {
                parser.eat_until(recovery);
                return AstKind::GposNode;
            }
            // now either a single or pair (type A)
            if metrics::eat_value_record(parser, recovery) {
                if glyph::eat_glyph_or_glyph_class(parser, recovery) {
                    // second value record is expected per the spec, but
                    // skipping it is supported in afdko & feaLib:
                    // https://github.com/adobe-type-tools/afdko/issues/1757
                    metrics::eat_value_record(parser, recovery);
                    parser.expect_semi();
                    return AstKind::GposType2;
                }
                parser.expect_semi();
                return AstKind::GposType1;
            }
            // pair type B
            if glyph::eat_glyph_or_glyph_class(parser, recovery)
                && metrics::eat_value_record(parser, recovery)
            {
                parser.expect_semi();
                return AstKind::GposType2;
            }

            // if we have made it this far, it looks like this is a chain rule.
            // in this case, we start needing arbitrary lookahead, so we just
            // eat everything now and then rewrite these rules later.
            finish_chain_rule(parser, recovery)
        }
    }

    parser.eat_trivia();
    parser.start_node(AstKind::GposNode);
    let kind = gpos_body(parser, recovery);
    parser.finish_and_remap_node(kind);
}

fn parse_ignore(parser: &mut Parser, recovery: TokenSet) -> AstKind {
    assert!(parser.eat(Kind::IgnoreKw));
    assert!(parser.eat(Kind::PosKw));

    if super::expect_ignore_pattern_body(parser, recovery) {
        AstKind::GposNodeNeedsRewrite
    } else {
        AstKind::GposNode
    }
}

fn gpos_cursive(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.eat(Kind::CursiveKw));
    glyph::eat_glyph_or_glyph_class(parser, recovery.union(Kind::LAngle.into()));
    metrics::anchor(parser, recovery.union(Kind::LAngle.into()));
    metrics::anchor(parser, recovery);
    parser.expect_semi();
}

fn gpos_mark_to_mark(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.eat(Kind::MarkKw));
    gpos_mark_to_(parser, recovery);
}

fn gpos_mark_to_base(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.nth_raw(0) == b"base");
    parser.eat_remap(Kind::Ident, AstKind::BaseKw);
    gpos_mark_to_(parser, recovery);
}

fn gpos_mark_to_(parser: &mut Parser, recovery: TokenSet) {
    glyph::eat_glyph_or_glyph_class(
        parser,
        recovery.union(TokenSet::new(&[Kind::LAngle, Kind::AnchorKw])),
    );
    super::greedy(anchor_mark)(parser, recovery);
    parser.expect_semi();
}

fn gpos_ligature(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.nth_raw(0) == b"ligature");
    parser.eat_remap(Kind::Ident, AstKind::LigatureKw);
    glyph::eat_glyph_or_glyph_class(
        parser,
        recovery.union(TokenSet::new(&[Kind::LAngle, Kind::AnchorKw])),
    );
    parser.in_node(AstKind::LigatureComponentNode, |parser| {
        super::greedy(anchor_mark)(parser, recovery);
    });

    while parser.nth_raw(0) == b"ligComponent" {
        parser.in_node(AstKind::LigatureComponentNode, |parser| {
            parser.eat_raw();
            super::greedy(anchor_mark)(parser, recovery);
        });
    }
    parser.expect_semi();
}

fn finish_chain_rule(parser: &mut Parser, recovery: TokenSet) -> AstKind {
    const RECOVERY: TokenSet = TokenSet::new(&[
        Kind::LAngle,
        Kind::SingleQuote,
        Kind::Number,
        Kind::LookupKw,
    ]);

    //I should do this more:
    debug_assert!(recovery.contains(Kind::Semi));
    debug_assert!(recovery.contains(Kind::PosKw));
    debug_assert!(recovery.contains(Kind::EnumKw));
    debug_assert!(recovery.contains(Kind::SubKw));

    // finish eating any prefix glyphes/classes
    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery.union(RECOVERY));

    if !parser.matches(0, Kind::SingleQuote) {
        parser.err("expected marked glyph");
        parser.eat_until(recovery);
        return AstKind::GposNode;
    }

    while parser.eat(Kind::SingleQuote) {
        // each marked glyph can be followed by multiple lookup refs
        if !super::greedy(eat_lookup)(parser, recovery) {
            // or(?) one inline single-pos rule
            metrics::eat_value_record(parser, recovery);
        }
        // or nothing, in  which case we eat the next glyph/class & loop
        glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY));
    }

    // eat the lookahead sequence
    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery);

    // special case: for inline pairpos, a value record can come at the very end.
    // accept this now, and then we will handle it in the rewriter
    // (http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#example-3c)
    metrics::eat_value_record(parser, recovery);

    if parser.expect_semi() {
        AstKind::GposNodeNeedsRewrite
    } else {
        AstKind::GposNode
    }
}

fn eat_lookup(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.eat(Kind::LookupKw) {
        if !parser.eat(Kind::Ident) {
            parser.err_recover("expected named lookup", recovery);
        }
        return true;
    }
    false
}

// <anchor> mark <named mark glyphclass>
/// returns true if we advance
fn anchor_mark(parser: &mut Parser, recovery: TokenSet) -> bool {
    const RECOVERY: TokenSet = TokenSet::new(&[Kind::MarkKw, Kind::NamedGlyphClass, Kind::Semi]);

    if !(parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::AnchorKw)) {
        return false;
    }
    parser.in_node(AstKind::AnchorMarkNode, |parser| {
        metrics::anchor(parser, recovery.union(RECOVERY));
        // we will verify later that the anchor was NULL
        if !parser.matches(0, Kind::Semi) {
            parser.expect_recover(Kind::MarkKw, recovery.union(RECOVERY));
            parser.expect_recover(Kind::NamedGlyphClass, recovery.union(RECOVERY));
        }
    });
    true
}
