use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

mod glyph;
mod gsub;
mod metrics;

pub(crate) fn root(parser: &mut Parser) {
    parser.start_node(Kind::SourceFile);
    while !parser.at_eof() {
        top_level_element(parser);
    }

    parser.eat_trivia();
    parser.finish_node();
}

fn top_level_element(parser: &mut Parser) {
    parser.eat_trivia();

    if parser.at_eof() {
        // noop
    } else if parser.matches(0, Kind::IncludeKw) {
        include(parser)
    } else if parser.matches(0, Kind::TableKw) {
        table(parser)
    } else if parser.matches(0, Kind::LookupKw) {
        lookup(parser)
    } else if parser.matches(0, Kind::LanguagesystemKw) {
        language_system(parser)
    } else if parser.matches(0, Kind::FeatureKw) {
        feature(parser)
    } else if parser.matches(0, Kind::MarkClassKw) {
        mark_class(parser)
    } else if parser.matches(0, Kind::AnchorDefKw) {
        anchor_def(parser)
    } else if parser.matches(0, Kind::AnonKw) {
        anonymous(parser)
    } else if parser.matches(0, Kind::NamedGlyphClass) {
        named_glyph_class_decl(parser)
    } else {
        parser.err_and_bump(format!(
            "Unexpected token '{}', expected global keyword.",
            parser.current_token_text()
        ));
        advance_to_top_level(parser);
    }
}

fn advance_to_top_level(parser: &mut Parser) {
    loop {
        parser.eat_trivia();
        if parser.at_eof() || parser.matches(0, TokenSet::TOP_LEVEL) {
            break;
        }
        parser.eat_raw();
    }
}

fn language_system(parser: &mut Parser) {
    fn language_system_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::LanguagesystemKw));
        if !parser.expect_tag() || !parser.expect_tag() {
            return advance_to_top_level(parser);
        }
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    }

    parser.start_node(Kind::LanguagesystemKw);
    language_system_body(parser);
    parser.finish_node();
}

fn include(parser: &mut Parser) {
    fn include_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::IncludeKw));
        if !parser.expect(Kind::LParen) {
            advance_to_top_level(parser);
        }
        if !parser.eat_remap(Kind::Ident, Kind::Path) {
            parser.err("Include statement missing path");
            return advance_to_top_level(parser);
        }
        if !parser.expect(Kind::RParen) {
            return advance_to_top_level(parser);
        }
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    }

    parser.start_node(Kind::IncludeKw);
    include_body(parser);
    parser.finish_node();
}

// @class = @class;
// @class = [a b c];
// @class = [a-z A - Z];
// @class = [\1-\40 \45 - \50]
fn named_glyph_class_decl(parser: &mut Parser) {
    fn glyph_class_body(parser: &mut Parser) {
        assert!(parser.expect(Kind::NamedGlyphClass));
        if !parser.eat(Kind::Eq) {
            return advance_to_top_level(parser);
        }

        if parser.eat(Kind::NamedGlyphClass) {
            parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
        } else if !parser.matches(0, Kind::LSquare) {
            parser.err_recover("Expected named glyph class or '['.", TokenSet::TOP_LEVEL);
        } else {
            glyph::eat_glyph_class_list(parser, TokenSet::EMPTY);
        }
    }

    parser.start_node(Kind::NamedGlyphClass);
    glyph_class_body(parser);
    parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    parser.finish_node();
}

fn table(_parser: &mut Parser) {
    unimplemented!()
}

fn lookup(parser: &mut Parser) {
    fn lookup_body(parser: &mut Parser) {
        const LABEL_RECOVERY: TokenSet = TokenSet::new(&[Kind::UseExtensionKw, Kind::LBrace]);
        assert!(parser.eat(Kind::LookupKw));
        let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
        parser.expect_remap_recover(Kind::Ident, Kind::Label, LABEL_RECOVERY);
        parser.eat(Kind::UseExtensionKw);
        parser.expect(Kind::LBrace);
    }

    parser.eat_trivia();
    parser.start_node(Kind::LookupKw);
    lookup_body(parser);
    parser.finish_node();
}

fn feature(_parser: &mut Parser) {
    unimplemented!()
}

// markClass <glyph|glyphclass> <anchor> <mark glyph class name>;
// e.g. markClass [acute grave dieresis] <anchor 350 0> @MARK_TOP_ACCENTS;
fn mark_class(parser: &mut Parser) {
    const ANCHOR_START: TokenSet = TokenSet::new(&[Kind::LAngle]);

    // true on progress
    fn glyph_or_class(parser: &mut Parser) -> bool {
        if parser.eat(Kind::NamedGlyphClass) {
            true
        } else if parser.matches(0, Kind::LSquare) {
            glyph::eat_glyph_class_list(parser, ANCHOR_START)
        } else {
            parser.eat_remap(Kind::Ident, Kind::GlyphName)
        }
    }

    fn mark_class_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::MarkClassKw));
        if !glyph_or_class(parser) {
            parser.err("Expected glyph name or class");
        }
        metrics::anchor(parser, TokenSet::new(&[Kind::Semi, Kind::NamedGlyphClass]));
        parser.expect_recover(Kind::NamedGlyphClass, TokenSet::SEMI);
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    }

    parser.start_node(Kind::MarkClassKw);
    mark_class_body(parser);
    parser.finish_node();
}

fn anchor_def(_parser: &mut Parser) {
    unimplemented!()
}

fn anonymous(_parser: &mut Parser) {
    unimplemented!()
}

#[cfg(test)]
fn debug_parse_output(text: &str, f: impl FnOnce(&mut Parser)) -> super::parse::DebugSink {
    let mut sink = super::parse::DebugSink::default();
    let mut parser = Parser::new(text, &mut sink);
    f(&mut parser);
    sink
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn languagesystem() {
        let input = "languagesystem dflt cool;";
        let out = debug_parse_output(input, |parser| language_system(parser));
        assert!(out.errors().is_empty());
        crate::assert_eq_str!(
            out.to_string(),
            "\
START LanguagesystemKw
  0..14 LanguagesystemKw
  14..15 WS
  15..19 Tag
  19..20 WS
  20..24 Tag
  24..25 ;
END LanguagesystemKw
"
        )
    }

    #[test]
    fn languagesystem_trivia() {
        let input = "# hi\nlanguagesystem dflt cool;";
        let out = debug_parse_output(input, |parser| root(parser));
        assert!(out.errors().is_empty());
        crate::assert_eq_str!(
            out.to_string(),
            "\
START FILE
  0..4 #
  4..5 WS
  START LanguagesystemKw
    5..19 LanguagesystemKw
    19..20 WS
    20..24 Tag
    24..25 WS
    25..29 Tag
    29..30 ;
  END LanguagesystemKw
END FILE
"
        )
    }

    #[test]
    fn top_level() {
        let input = "\
languagesystem dflt DFTL;
languagesystem okay cool;
include(fun.fea);
";
        let out = debug_parse_output(input, root);
        let exp = "\
START FILE
  START LanguagesystemKw
    0..14 LanguagesystemKw
    14..15 WS
    15..19 Tag
    19..20 WS
    20..24 Tag
    24..25 ;
  END LanguagesystemKw
  25..26 WS
  START LanguagesystemKw
    26..40 LanguagesystemKw
    40..41 WS
    41..45 Tag
    45..46 WS
    46..50 Tag
    50..51 ;
  END LanguagesystemKw
  51..52 WS
  START IncludeKw
    52..59 IncludeKw
    59..60 (
    60..67 Path
    67..68 )
    68..69 ;
  END IncludeKw
  69..70 WS
END FILE
";

        crate::assert_eq_str!(out.to_string(), exp);
    }

    #[test]
    fn glyph_class_alias() {
        let input = "@name = [a b d - z \\1-\\5 @hi];";
        let out = debug_parse_output(input, |parser| named_glyph_class_decl(parser));
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

    #[test]
    fn mark_class_() {
        let fea = "markClass [acute] <anchor 350 0> @TOP_MARKS;";
        let out = debug_parse_output(fea, |parser| mark_class(parser));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START MarkClassKw
  0..9 MarkClassKw
  9..10 WS
  START GlyphClass
    10..11 [
    11..16 GlyphName
    16..17 ]
  END GlyphClass
  17..18 WS
  START AnchorKw
    18..19 <
    19..25 AnchorKw
    25..26 WS
    26..29 METRIC
    29..30 WS
    30..31 METRIC
    31..32 >
  END AnchorKw
  32..33 WS
  33..43 @GlyphClass
  43..44 ;
END MarkClassKw
",
            out.to_string(),
        );
    }
}
