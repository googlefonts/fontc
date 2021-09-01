use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

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
                Kind::LBrace,
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
            eat_glyph_class_list(parser, recovery);
        }
    }

    parser.start_node(Kind::NamedGlyphClass);
    glyph_class_body(parser, recovery);
    parser.expect_semi();
    parser.finish_node();
}

// B @class [a b]
pub(crate) fn eat_glyph_or_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    parser.eat_remap(TokenSet::IDENT_LIKE, Kind::GlyphName)
        || eat_named_or_unnamed_glyph_class(parser, recovery)
}

pub(crate) fn eat_named_or_unnamed_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    parser.eat(Kind::NamedGlyphClass) || eat_glyph_class_list(parser, recovery)
}

// [ a b a-z @hi \0-\40 ]
pub(crate) fn eat_glyph_class_list(parser: &mut Parser, recovery: TokenSet) -> bool {
    //FIXME: most of this should come from above?
    const LIST_RECOVERY: TokenSet = TokenSet::TOP_SEMI.union(TokenSet::new(&[Kind::RSquare]));

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
        glyph_range(parser, recovery.add(Kind::RSquare));
        parser.finish_node();
    } else {
        expect_glyph_name_like(parser, recovery.add(Kind::RSquare));
    }
}

fn glyph_range(parser: &mut Parser, recovery: TokenSet) -> bool {
    const HYPHEN: TokenSet = TokenSet::new(&[Kind::Hyphen]);

    let first_recovery = recovery.union(HYPHEN);

    expect_glyph_name_like(parser, first_recovery)
        & parser.expect_recover(Kind::Hyphen, recovery)
        & expect_glyph_name_like(parser, recovery)
}

pub(crate) fn expect_glyph_name_like(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.matches(0, Kind::Backslash) {
        if parser.matches(1, Kind::Ident) {
            parser.eat_remap2(Kind::GlyphName);
            true
        } else if parser.matches(1, Kind::Number) {
            parser.eat_remap2(Kind::Cid);
            true
        } else {
            parser.eat_raw();
            let kind = parser.nth(0).kind;
            parser.err(format!("Expected glyph name or CID, found {}", kind));
            parser.eat_unless(recovery);
            false
        }
    } else {
        parser.expect_remap_recover(TokenSet::IDENT_LIKE, Kind::GlyphName, recovery)
    }
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn glyph_name_smoke_test() {
        // normal name, an escaped glyph name, and a contextual keyword
        let fea = "[A \\mark Ascender]";
        let out = debug_parse_output(fea, |parser| {
            eat_glyph_class_list(parser, TokenSet::EMPTY);
        });
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GlyphClass
  [
  GlyphName(A)
  WS( )
  GlyphName(\\\\mark)
  WS( )
  GlyphName(Ascender)
  ]
END GlyphClass
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn glyph_class_alias() {
        let input = "@name = [a b d - z \\1-\\5 @hi];";
        let out = debug_parse_output(input, |parser| {
            named_glyph_class_decl(parser, TokenSet::EMPTY)
        });
        crate::assert_eq_str!(
            out.to_string(),
            "\
START @GlyphClass
  0..5 @GlyphClass
  5..6 WS
  6..7 =
  7..8 WS
  START GlyphClass
    8..9 [
    9..10 GlyphName
    10..11 WS
    11..12 GlyphName
    12..13 WS
    START GlyphRange
      13..14 GlyphName
      14..15 WS
      15..16 -
      16..17 WS
      17..18 GlyphName
    END GlyphRange
    18..19 WS
    START GlyphRange
      19..21 CID
      21..22 -
      22..24 CID
    END GlyphRange
    24..25 WS
    25..28 @GlyphClass
    28..29 ]
  END GlyphClass
  29..30 ;
END @GlyphClass
"
        );
        assert!(out.errors().is_empty());
    }
}
