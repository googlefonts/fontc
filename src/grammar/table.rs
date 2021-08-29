use std::ops::Range;

use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

pub(crate) fn table(parser: &mut Parser) {
    parser.eat_trivia();
    parser.start_node(Kind::TableKw);
    assert!(parser.eat(Kind::TableKw));
    let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
    match parser.nth_raw(0) {
        b"BASE" => table_impl(parser, b"BASE", base::table_contents),
        _ => {
            parser.expect_recover(Kind::Ident, TokenSet::TOP_LEVEL.union(Kind::LBrace.into()));
            if parser.expect_recover(Kind::LBrace, TokenSet::TOP_LEVEL) {
                raw_label_range.map(|range| unknown_table(parser, range));
            }
        }
    }
}

fn table_impl(parser: &mut Parser, tag: &[u8], table_fn: impl FnOnce(&mut Parser, TokenSet)) {
    assert_eq!(parser.nth_raw(0), tag);
    parser.eat_raw();
    parser.expect_recover(Kind::LBrace, TokenSet::TOP_SEMI);
    table_fn(parser, TokenSet::TOP_LEVEL);
    parser.expect_recover(Kind::RBrace, TokenSet::TOP_SEMI);
    if parser.nth_raw(0) != tag {
        parser.err_and_bump(format!("Expected '{}' tag", String::from_utf8_lossy(tag)));
    } else {
        parser.eat_raw();
    }
    parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
}

// if we don't recognize a table tag we still want to try parsing.
fn unknown_table(parser: &mut Parser, open_tag: Range<usize>) {
    loop {
        match parser.nth(0).kind {
            Kind::RBrace if parser.nth_raw(1) == parser.raw_range(open_tag.clone()) => {
                assert!(parser.eat(Kind::RBrace) && parser.eat(Kind::Ident));
                parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
                break;
            }
            _ => {
                if parser.nth(1).kind == Kind::Eof {
                    parser.err_and_bump("unterminated anonymous block");
                    break;
                }
                parser.eat_raw();
            }
        }
    }
}

mod base {
    use super::*;
    //const BASE_RECOVERY: TokenSet
    fn expect_entry(parser: &mut Parser, recovery: TokenSet) -> bool {
        fn table_node(parser: &mut Parser, f: impl FnOnce(&mut Parser)) {
            parser.eat_trivia();
            parser.start_node(Kind::TableEntryNode);
            f(parser);
            parser.finish_node();
        }

        match parser.nth_raw(0) {
            keywords::HORIZ_AXIS_BASE_TAG_LIST | keywords::VERT_AXIS_BASE_TAG_LIST => {
                table_node(parser, |parser| {
                    assert!(parser.eat(Kind::Ident));
                    parser.expect_recover(Kind::Ident, recovery.union(Kind::Semi.into()));
                    while parser.eat(Kind::Ident) {
                        continue;
                    }
                    parser.expect_recover(Kind::Semi, recovery);
                })
            }
            keywords::HORIZ_AXIS_BASE_SCRIPT_LIST | keywords::VERT_AXIS_BASE_SCRIPT_LIST => {
                table_node(parser, |parser| {
                    assert!(parser.eat(Kind::Ident));
                    expect_script_record(parser, recovery);
                    while parser.eat(Kind::Comma) {
                        expect_script_record(parser, recovery);
                    }
                    parser.expect_recover(Kind::Semi, recovery);
                })
            }
            keywords::VERT_AXIS_MIN_MAX | keywords::HORIZ_AXIS_MIN_MAX => {
                table_node(parser, |parser| {
                    const EAT_UNTIL: TokenSet =
                        TokenSet::TOP_LEVEL.union(TokenSet::new(&[Kind::Semi, Kind::RBrace]));
                    loop {
                        while !parser.matches(0, EAT_UNTIL) {
                            parser.eat_raw();
                        }
                    }
                })
            }
            _ => return false,
        };
        true
    }

    pub(crate) fn table_contents(parser: &mut Parser, recovery: TokenSet) {
        while expect_entry(parser, recovery) {
            continue;
        }
    }

    fn expect_script_record(parser: &mut Parser, recovery: TokenSet) -> bool {
        const RECORD_RECOVERY: TokenSet = TokenSet::new(&[Kind::Tag, Kind::Number, Kind::Comma]);

        fn script_record_body(parser: &mut Parser, recovery: TokenSet) {
            parser.eat_remap(Kind::Ident, Kind::Tag);
            parser.expect_remap_recover(Kind::Ident, Kind::Tag, recovery.union(RECORD_RECOVERY));
            parser.expect_recover(Kind::Number, recovery.union(RECORD_RECOVERY));
            while parser.eat(Kind::Number) {
                continue;
            }
        }

        if !parser.matches(0, Kind::Ident) {
            return false;
        }
        parser.eat_trivia();
        parser.start_node(Kind::ScriptRecordNode);
        script_record_body(parser, recovery);
        parser.finish_node();
        true
    }
}

#[allow(dead_code)]
mod keywords {
    pub const HORIZ_AXIS_BASE_SCRIPT_LIST: &[u8] = b"HorizAxis.BaseScriptList"; // BASE table ✅
    pub const HORIZ_AXIS_BASE_TAG_LIST: &[u8] = b"HorizAxis.BaseTagList"; // BASE table ✅
    pub const HORIZ_AXIS_MIN_MAX: &[u8] = b"HorizAxis.MinMax"; // BASE table ❌
    pub const VERT_AXIS_BASE_SCRIPT_LIST: &[u8] = b"VertAxis.BaseScriptList"; // BASE table ✅
    pub const VERT_AXIS_BASE_TAG_LIST: &[u8] = b"VertAxis.BaseTagList"; // BASE table ✅
    pub const VERT_AXIS_MIN_MAX: &[u8] = b"VertAxis.MinMax"; // BASE table ❌
    pub const ATTACH: &[u8] = b"Attach"; // GDEF table ✅
    pub const GLYPH_CLASS_DEF: &[u8] = b"GlyphClassDef"; // GDEF table ✅
    pub const LIGATURE_CARET_BY_DEV: &[u8] = b"LigatureCaretByDev"; // GDEF table ❌
    pub const LIGATURE_CARET_BY_INDEX: &[u8] = b"LigatureCaretByIndex"; // GDEF table ✅
    pub const LIGATURE_CARET_BY_POS: &[u8] = b"LigatureCaretByPos"; // GDEF table ✅
    pub const MARK_ATTACH_CLASS: &[u8] = b"MarkAttachClass"; // GDEF table ✅
    pub const FONT_REVISION: &[u8] = b"FontRevision"; // head table ✅
    pub const ASCENDER: &[u8] = b"Ascender"; // hhea table ✅
    pub const CARET_OFFSET: &[u8] = b"CaretOffset"; // hhea table ✅
    pub const DESCENDER: &[u8] = b"Descender"; // hhea table ✅
    pub const LINE_GAP: &[u8] = b"LineGap"; // hhea table ✅
    pub const CAP_HEIGHT: &[u8] = b"CapHeight"; // OS/2 table ✅
    pub const CODE_PAGE_RANGE: &[u8] = b"CodePageRange"; // OS/2 table ✅
    pub const PANOSE: &[u8] = b"Panose"; // OS/2 table ✅
    pub const TYPO_ASCENDER: &[u8] = b"TypoAscender"; // OS/2 table ✅
    pub const TYPO_DESCENDER: &[u8] = b"TypoDescender"; // OS/2 table ✅
    pub const TYPO_LINE_GAP: &[u8] = b"TypoLineGap"; // OS/2 table ✅
    pub const UNICODE_RANGE: &[u8] = b"UnicodeRange"; // OS/2 table ✅
    pub const VENDOR: &[u8] = b"Vendor"; // OS/2 table ✅
    pub const WIN_ASCENT: &[u8] = b"winAscent"; // OS/2 table ✅
    pub const WIN_DESCENT: &[u8] = b"winDescent"; // OS/2 table ✅
    pub const XHEIGHT: &[u8] = b"XHeight"; // OS/2 table ✅
    pub const SIZEMENUNAME: &[u8] = b"sizemenuname"; // size feature ✅
    pub const VERT_TYPO_ASCENDER: &[u8] = b"VertTypoAscender"; // vhea table ✅
    pub const VERT_TYPO_DESCENDER: &[u8] = b"VertTypoDescender"; // vhea table ✅
    pub const VERT_TYPO_LINE_GAP: &[u8] = b"VertTypoLineGap"; // vhea table ✅
    pub const VERT_ADVANCE_Y: &[u8] = b"VertAdvanceY"; // vmtx table ✅
    pub const VERT_ORIGIN_Y: &[u8] = b"VertOriginY"; // vmtx table ✅
    pub const ELIDED_FALLBACK_NAME: &[u8] = b"ElidedFallbackName"; // STAT table ✅
    pub const ELIDED_FALLBACK_NAME_ID: &[u8] = b"ElidedFallbackNameID"; // STAT table ✅
    pub const DESIGN_AXIS: &[u8] = b"DesignAxis"; // STAT table ✅
    pub const AXIS_VALUE: &[u8] = b"AxisValue"; // STAT table ✅
    pub const FLAG: &[u8] = b"flag"; // STAT table ✅
    pub const LOCATION: &[u8] = b"location"; // STAT table ✅
    pub const ELIDABLE_AXIS_VALUE_NAME: &[u8] = b"ElidableAxisValueName"; // STAT table ✅
    pub const OLDER_SIBLING_FONT_ATTRIBUTE: &[u8] = b"OlderSiblingFontAttribute";
    // STAT table ✅
}
