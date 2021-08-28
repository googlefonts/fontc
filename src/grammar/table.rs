use std::ops::Range;

use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

pub(crate) fn table(parser: &mut Parser) {
    parser.eat_trivia();
    parser.start_node(Kind::TableKw);
    assert!(parser.eat(Kind::TableKw));
    let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
    if parser.matches(0, Kind::Ident) && parser.matches(1, Kind::LBrace) {
        parser.eat_raw();
        parser.eat_raw();
        unknown_table(parser, raw_label_range.unwrap())
    } else {
        parser.expect_recover(Kind::Ident, TokenSet::TOP_LEVEL.union(Kind::LBrace.into()));
        parser.expect_recover(Kind::LBrace, TokenSet::TOP_LEVEL);
    }
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

#[allow(dead_code)]
mod keywords {
    const HORIZ_AXIS_BASE_SCRIPT_LIST: &str = "HorizAxis.BaseScriptList"; // BASE table ✅
    const HORIZ_AXIS_BASE_TAG_LIST: &str = "HorizAxis.BaseTagList"; // BASE table ✅
    const HORIZ_AXIS_MIN_MAX: &str = "HorizAxis.MinMax"; // BASE table ❌
    const VERT_AXIS_BASE_SCRIPT_LIST: &str = "VertAxis.BaseScriptList"; // BASE table ✅
    const VERT_AXIS_BASE_TAG_LIST: &str = "VertAxis.BaseTagList"; // BASE table ✅
    const VERT_AXIS_MIN_MAX: &str = "VertAxis.MinMax"; // BASE table ❌
    const ATTACH: &str = "Attach"; // GDEF table ✅
    const GLYPH_CLASS_DEF: &str = "GlyphClassDef"; // GDEF table ✅
    const LIGATURE_CARET_BY_DEV: &str = "LigatureCaretByDev"; // GDEF table ❌
    const LIGATURE_CARET_BY_INDEX: &str = "LigatureCaretByIndex"; // GDEF table ✅
    const LIGATURE_CARET_BY_POS: &str = "LigatureCaretByPos"; // GDEF table ✅
    const MARK_ATTACH_CLASS: &str = "MarkAttachClass"; // GDEF table ✅
    const FONT_REVISION: &str = "FontRevision"; // head table ✅
    const ASCENDER: &str = "Ascender"; // hhea table ✅
    const CARET_OFFSET: &str = "CaretOffset"; // hhea table ✅
    const DESCENDER: &str = "Descender"; // hhea table ✅
    const LINE_GAP: &str = "LineGap"; // hhea table ✅
    const CAP_HEIGHT: &str = "CapHeight"; // OS/2 table ✅
    const CODE_PAGE_RANGE: &str = "CodePageRange"; // OS/2 table ✅
    const PANOSE: &str = "Panose"; // OS/2 table ✅
    const TYPO_ASCENDER: &str = "TypoAscender"; // OS/2 table ✅
    const TYPO_DESCENDER: &str = "TypoDescender"; // OS/2 table ✅
    const TYPO_LINE_GAP: &str = "TypoLineGap"; // OS/2 table ✅
    const UNICODE_RANGE: &str = "UnicodeRange"; // OS/2 table ✅
    const VENDOR: &str = "Vendor"; // OS/2 table ✅
    const WIN_ASCENT: &str = "winAscent"; // OS/2 table ✅
    const WIN_DESCENT: &str = "winDescent"; // OS/2 table ✅
    const XHEIGHT: &str = "XHeight"; // OS/2 table ✅
    const SIZEMENUNAME: &str = "sizemenuname"; // size feature ✅
    const VERT_TYPO_ASCENDER: &str = "VertTypoAscender"; // vhea table ✅
    const VERT_TYPO_DESCENDER: &str = "VertTypoDescender"; // vhea table ✅
    const VERT_TYPO_LINE_GAP: &str = "VertTypoLineGap"; // vhea table ✅
    const VERT_ADVANCE_Y: &str = "VertAdvanceY"; // vmtx table ✅
    const VERT_ORIGIN_Y: &str = "VertOriginY"; // vmtx table ✅
    const ELIDED_FALLBACK_NAME: &str = "ElidedFallbackName"; // STAT table ✅
    const ELIDED_FALLBACK_NAME_ID: &str = "ElidedFallbackNameID"; // STAT table ✅
    const DESIGN_AXIS: &str = "DesignAxis"; // STAT table ✅
    const AXIS_VALUE: &str = "AxisValue"; // STAT table ✅
    const FLAG: &str = "flag"; // STAT table ✅
    const LOCATION: &str = "location"; // STAT table ✅
    const ELIDABLE_AXIS_VALUE_NAME: &str = "ElidableAxisValueName"; // STAT table ✅
    const OLDER_SIBLING_FONT_ATTRIBUTE: &str = "OlderSiblingFontAttribute"; // STAT table ✅
}
