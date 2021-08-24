use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

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
        assert!(
            parser.matches(0, Kind::LanguagesystemKw),
            "{}",
            parser.current_token_text()
        );
        parser.eat_remap(Kind::LanguagesystemKw);
        if !parser.expect_tag() || !parser.expect_tag() {
            return advance_to_top_level(parser);
        }
        expect_semi_or_ws_semi(parser);
    }

    parser.start_node(Kind::LanguagesystemKw);
    language_system_body(parser);
    parser.finish_node();
}

fn include(parser: &mut Parser) {
    fn include_body(parser: &mut Parser) {
        assert!(parser.matches(0, Kind::IncludeKw));
        parser.eat_remap(Kind::IncludeKw);
        if !parser.expect(Kind::LParen) {
            advance_to_top_level(parser);
        }
        if parser.matches(0, Kind::Ident) {
            parser.eat_remap(Kind::Path);
        } else {
            parser.err("Include statement missing path");
            return advance_to_top_level(parser);
        }
        if !parser.expect(Kind::RParen) {
            return advance_to_top_level(parser);
        }
        expect_semi_or_ws_semi(parser);
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
            return expect_semi_or_ws_semi(parser);
        } else if !parser.matches(0, Kind::LSquare) {
            parser.err_recover("Expected named glyph class or '['.", TokenSet::TOP_LEVEL);
            return;
        }
        glyph_class_list(parser);
    }

    parser.start_node(Kind::NamedGlyphClass);
    glyph_class_body(parser);
    parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    parser.finish_node();
}

// [ a b a-z @hi \0-\40 ]
fn glyph_class_list(parser: &mut Parser) {
    const LIST_RECOVERY: TokenSet = TokenSet::TOP_LEVEL
        .union(TokenSet::RSQUARE)
        .union(TokenSet::SEMI);

    parser.eat_trivia();
    parser.start_node(Kind::GlyphClass);
    parser.expect(Kind::LSquare);
    while !parser.matches(0, LIST_RECOVERY) {
        glyph_class_list_member(parser);
    }

    parser.expect(Kind::RSquare);
    parser.finish_node();
}

fn glyph_class_list_member(parser: &mut Parser) {
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
        glyph_range(parser, true);
        parser.finish_node();
    } else {
        glyph_name_like(parser, TokenSet::RSQUARE);
    }
}

fn glyph_range(parser: &mut Parser, in_list: bool) -> bool {
    const HYPHEN: TokenSet = TokenSet::new(&[Kind::Hyphen]);
    const HYPHEN_RSQUARE: TokenSet = HYPHEN.union(TokenSet::RSQUARE);

    let first_recovery = if in_list { HYPHEN_RSQUARE } else { HYPHEN };
    let last_recovery = if in_list {
        TokenSet::RSQUARE
    } else {
        TokenSet::EMPTY
    };

    glyph_name_like(parser, first_recovery)
        & parser.expect_recover(Kind::Hyphen, last_recovery)
        & glyph_name_like(parser, last_recovery)
}

fn glyph_name_like(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.matches(0, Kind::Backslash) {
        if parser.matches(1, Kind::Ident) {
            parser.eat_remap2(Kind::GlyphName);
            return true;
        } else if parser.matches(1, Kind::NumberDec) {
            parser.eat_remap2(Kind::Cid);
            return true;
        } else {
            parser.eat_raw();
            let kind = parser.nth(0).kind;
            parser.err(format!("Expected glyph name or CID, found {}", kind));
            if !parser.matches(0, recovery) {
                parser.eat_raw();
            }
        }
    } else if parser.matches(0, Kind::Ident) {
        parser.eat_remap(Kind::GlyphName);
        return true;
    } else {
        parser.expect_recover(Kind::Ident, recovery);
    }
    false
}

fn expect_semi_or_ws_semi(parser: &mut Parser) {
    if parser.matches(0, Kind::Whitespace) && parser.matches(1, Kind::Semi) {
        parser.eat_raw();
    }
    if !parser.eat(Kind::Semi) {
        parser.err_recover("Missing closing ';'.", TokenSet::TOP_LEVEL);
    }
}

fn table(_parser: &mut Parser) {
    unimplemented!()
}

fn lookup(_parser: &mut Parser) {
    unimplemented!()
}

fn feature(_parser: &mut Parser) {
    unimplemented!()
}

fn mark_class(_parser: &mut Parser) {
    unimplemented!()
}

fn anchor_def(_parser: &mut Parser) {
    unimplemented!()
}

fn anonymous(_parser: &mut Parser) {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::DebugSink;

    fn debug_parse_output(text: &str, f: impl FnOnce(&mut Parser)) -> String {
        let mut sink = DebugSink::default();
        let mut parser = Parser::new(text, &mut sink);
        f(&mut parser);
        sink.to_string()
    }

    #[test]
    fn languagesystem() {
        let input = "languagesystem dflt cool;";
        let out = debug_parse_output(input, |parser| language_system(parser));
        crate::assert_eq_str!(
            out,
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

        crate::assert_eq_str!(out, exp);
    }

    #[test]
    fn glyph_class_alias() {
        let input = "@name = [a b d - z \\1-\\5];";
        let out = debug_parse_output(input, |parser| named_glyph_class_decl(parser));
        crate::assert_eq_str!(
            out,
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
    24..25 ]
  END GlyphClass
  25..26 ;
END @GlyphClass
"
        )
    }
}
