use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

// B @class [a b]
pub(crate) fn eat_glyph_or_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.eat(Kind::NamedGlyphClass) || parser.eat_remap(Kind::Ident, Kind::GlyphName) {
        return true;
    }
    eat_glyph_class_list(parser, recovery)
}

// [ a b a-z @hi \0-\40 ]
pub(crate) fn eat_glyph_class_list(parser: &mut Parser, recovery: TokenSet) -> bool {
    //FIXME: most of this should come from above?
    const LIST_RECOVERY: TokenSet = TokenSet::TOP_LEVEL
        .union(TokenSet::RSQUARE)
        .union(TokenSet::SEMI);

    if !parser.matches(0, Kind::LSquare) {
        return false;
    }

    parser.eat_trivia();
    parser.start_node(Kind::GlyphClass);
    assert!(parser.eat(Kind::LSquare));
    while !parser.matches(0, LIST_RECOVERY.union(recovery)) {
        glyph_class_list_member(parser, recovery);
    }

    parser.expect_recover(Kind::RSquare, recovery);
    parser.finish_node();
    true
}

fn glyph_class_list_member(parser: &mut Parser, recovery: TokenSet) {
    if parser.eat(Kind::NamedGlyphClass) {
        return;
    }
    // a glyphname
    // a glyph development name
    // an escaped glyph name
    // an escaped CID

    let looks_like_range = parser.matches(1, Kind::Hyphen)
        || (parser.matches(0, Kind::Backslash) && parser.matches(2, Kind::Hyphen));
    if looks_like_range {
        parser.eat_trivia();
        parser.start_node(Kind::GlyphRange);
        glyph_range(parser, TokenSet::RSQUARE.union(recovery));
        parser.finish_node();
    } else {
        glyph_name_like(parser, TokenSet::RSQUARE.union(recovery));
    }
}

fn glyph_range(parser: &mut Parser, recovery: TokenSet) -> bool {
    const HYPHEN: TokenSet = TokenSet::new(&[Kind::Hyphen]);

    let first_recovery = recovery.union(HYPHEN);

    glyph_name_like(parser, first_recovery)
        & parser.expect_recover(Kind::Hyphen, recovery)
        & glyph_name_like(parser, recovery)
}

fn glyph_name_like(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.matches(0, Kind::Backslash) {
        if parser.matches(1, Kind::Ident) {
            parser.eat_remap2(Kind::GlyphName);
            return true;
        } else if parser.matches(1, Kind::Number) {
            parser.eat_remap2(Kind::Cid);
            return true;
        } else {
            parser.eat_raw();
            let kind = parser.nth(0).kind;
            parser.err(format!("Expected glyph name or CID, found {}", kind));
            if !parser.matches(0, recovery) {
                parser.eat_raw();
            }
            return false;
        }
    } else {
        parser.expect_remap_recover(Kind::Ident, Kind::GlyphName, recovery)
    }
}
