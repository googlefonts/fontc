use crate::{
    parse::{Kind, Parser, TokenSet},
    validate::{self, NameType},
};

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

    parser.start_node(Kind::GlyphClassDefNode);
    glyph_class_body(parser, recovery);
    parser.expect_semi();
    parser.finish_node();
}

// B @class [a b]
pub(crate) fn eat_glyph_or_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    eat_glyph_name_like(parser) || eat_named_or_unnamed_glyph_class(parser, recovery)
}

pub(crate) fn eat_named_or_unnamed_glyph_class(parser: &mut Parser, recovery: TokenSet) -> bool {
    parser.eat(Kind::NamedGlyphClass) || eat_glyph_class_list(parser, recovery)
}

// [ a b a-z @hi \0-\40 ]
pub(crate) fn eat_glyph_class_list(parser: &mut Parser, recovery: TokenSet) -> bool {
    let recovery = recovery.add(Kind::RSquare);
    if !parser.matches(0, Kind::LSquare) {
        return false;
    }

    parser.eat_trivia();
    parser.start_node(Kind::GlyphClass);
    let range = parser.nth_range(0);
    assert!(parser.eat(Kind::LSquare));
    while !parser.matches(0, recovery) {
        glyph_class_list_member(parser, recovery);
    }

    if !parser.eat(Kind::RSquare) {
        parser.raw_error(range, "Unclosed glyph class.");
        parser.err_recover("Expected closing ']'.", recovery);
    }
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
    if eat_glyph_name_like(parser) {
        return true;
    }

    parser.err_recover("Expected glyph name or CID", recovery);
    false
}

fn eat_glyph_name_like(parser: &mut Parser) -> bool {
    if parser.matches(0, Kind::Backslash) {
        if parser.matches(1, TokenSet::IDENT_LIKE) {
            parser.eat(Kind::Backslash);
            eat_and_validate_glyph_name(parser);
            true
        } else if parser.matches(1, Kind::Number) {
            parser.eat(Kind::Backslash);
            let raw = parser.nth_raw(0);
            // negative numbers not allowed in CID
            let valid = raw.first() != Some(&b'-');
            if !valid {
                parser.err_and_bump("CID must be positive decimal number.")
            } else {
                parser.eat_remap(Kind::Number, Kind::Cid);
            }
            true
        } else {
            false
        }
    } else if parser.matches(0, TokenSet::IDENT_LIKE) {
        eat_and_validate_glyph_name(parser);
        true
    } else {
        false
    }
}

fn eat_and_validate_glyph_name(parser: &mut Parser) {
    debug_assert!(parser.matches(0, TokenSet::IDENT_LIKE));
    let raw = parser.nth_raw(0);
    match validate::validate_glyph_name(raw) {
        NameType::Valid => {
            parser.eat_remap(TokenSet::IDENT_LIKE, Kind::GlyphName);
        }
        NameType::MaybeRange => {
            parser.eat_remap(TokenSet::IDENT_LIKE, Kind::GlyphName);
        }
        NameType::Invalid(pos) => {
            let err = match std::str::from_utf8(&raw[pos..])
                .ok()
                .and_then(|t| t.chars().next())
            {
                Some(chr) => format!("Invalid char '{}' in glyph name", chr),
                None => "Invalid char in glyph name".to_string(),
            };
            parser.err_and_bump(err);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::DebugSink;

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
  \\
  GlyphName(mark)
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
START GlyphClassDefNode
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
      19..20 \\
      20..21 CID
      21..22 -
      22..23 \\
      23..24 CID
    END GlyphRange
    24..25 WS
    25..28 @GlyphClass
    28..29 ]
  END GlyphClass
  29..30 ;
END GlyphClassDefNode
"
        );
        assert!(out.errors().is_empty());
    }

    #[test]
    fn name_like() {
        let fea = "hi \\hi \\mark \\table \\12";
        let mut sink = DebugSink::default();
        let mut parser = Parser::new(fea, &mut sink);
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(1), b"hi");
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(1), b"mark");
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(1), b"table");
        assert!(eat_glyph_name_like(&mut parser));
        assert_eq!(parser.nth_raw(1), b"12");
        assert!(eat_glyph_name_like(&mut parser));
        assert!(!eat_glyph_name_like(&mut parser));
    }

    #[test]
    fn invalid_things() {
        let bad_glyphs = [".hi", "hi!", "hî", "\\-5"];
        for raw in bad_glyphs {
            let mut sink = DebugSink::default();
            let mut parser = Parser::new(raw, &mut sink);
            eat_glyph_name_like(&mut parser);
            assert_eq!(sink.errors().len(), 1, "'{}'", raw);
        }
    }
}
