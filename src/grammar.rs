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
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
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
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
        //expect_semi_or_ws_semi(parser);
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
            glyph_class_list(parser, TokenSet::EMPTY);
        }
    }

    parser.start_node(Kind::NamedGlyphClass);
    glyph_class_body(parser);
    parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    parser.finish_node();
}

// [ a b a-z @hi \0-\40 ]
/// returns `true` if the list has a closing ']'
fn glyph_class_list(parser: &mut Parser, recovery: TokenSet) -> bool {
    const LIST_RECOVERY: TokenSet = TokenSet::TOP_LEVEL
        .union(TokenSet::RSQUARE)
        .union(TokenSet::SEMI);

    parser.eat_trivia();
    parser.start_node(Kind::GlyphClass);
    parser.expect(Kind::LSquare);
    while !parser.matches(0, LIST_RECOVERY.union(recovery)) {
        glyph_class_list_member(parser, recovery);
    }

    let r = parser.expect(Kind::RSquare);
    parser.finish_node();
    r
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
        } else if parser.matches(1, Kind::DecimalLike) {
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

fn table(_parser: &mut Parser) {
    unimplemented!()
}

fn lookup(_parser: &mut Parser) {
    unimplemented!()
}

fn feature(_parser: &mut Parser) {
    unimplemented!()
}

// markClass <glyph|glyphclass> <anchor> <mark glyph class name>;
// e.g. markClass [acute grave dieresis] <anchor 350 0> @MARK_TOP_ACCENTS;
fn mark_class(parser: &mut Parser) {
    const ANCHOR_START: TokenSet = TokenSet::new(&[Kind::LAngle]);
    // true on success
    fn glyph_or_class(parser: &mut Parser) -> bool {
        if parser.eat(Kind::NamedGlyphClass) {
            true
        } else if parser.matches(0, Kind::LSquare) {
            glyph_class_list(parser, ANCHOR_START)
        } else if parser.matches(0, Kind::Ident) {
            parser.eat_remap(Kind::GlyphName);
            true
        } else {
            false
        }
    }

    fn mark_class_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::MarkClassKw));
        if !glyph_or_class(parser) {
            parser.err("Expected glyph name or class");
        }
        anchor(parser, TokenSet::new(&[Kind::Semi, Kind::NamedGlyphClass]));
        parser.expect_recover(Kind::NamedGlyphClass, TokenSet::SEMI);
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

// A: <anchor <metric> <metric>> (<anchor 120 -20>)
// B: <anchor <metric> <metric>  # x coordinate, y coordinate
//    <contour point>> (<anchor 120 -20 contourpoint 5>)
// C: <anchor <metric> <metric>   # x coordinate, y coordinate
//    <device> <device>>  # x coordinate device, y coordinate device
//    (<anchor 120 -20 <device 11 1> <device NULL>>)
// D: <anchor NULL>
// E: <anchor <name>> (<anchor TOP_ANCHOR_1>)
fn anchor(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn anchor_body(parser: &mut Parser, recovery: TokenSet) -> bool {
        parser.expect(Kind::LAngle);
        parser.expect(Kind::AnchorKw);
        if parser.eat(Kind::NullKw) || parser.eat(Kind::Ident) {
            return parser.expect_recover(Kind::RAngle, recovery);
        }

        const RANGLE: TokenSet = TokenSet::new(&[Kind::RAngle]);
        let recovery = recovery.union(RANGLE);
        // now either:
        // <metric> metric>
        // <metric> <metric> <contour point>
        // <metric> <metric> <device> <device>
        if expect_number(parser, Kind::Metric, recovery) {
            expect_number(parser, Kind::Metric, recovery);
        }
        // either done or contour | device
        if parser.eat(Kind::RAngle) {
            return true;
        }
        if parser.eat(Kind::ContourpointKw) {
            if parser.matches(0, Kind::DecimalLike) {
                parser.eat_remap(Kind::Number);
            } else {
                parser.err("countourpoint should be a positive decimal number");
                if !parser.matches(0, recovery) {
                    parser.eat_raw();
                }
            }
        } else if parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw) {
            if expect_device(parser, recovery) {
                expect_device(parser, recovery);
            }
        } else {
            // something else unexpected in our anchor?
            assert!(!parser.matches(0, Kind::RAngle));
            parser.err("Invalid anchor element.");
            if !parser.matches(0, recovery) {
                parser.eat_raw();
            }
        }
        parser.expect_recover(Kind::RAngle, recovery)
    }

    parser.start_node(Kind::AnchorKw);
    let r = anchor_body(parser, recovery);
    parser.finish_node();
    r
}

// A <number> is a signed decimal integer (without leading zeros)
// returns true if we advance.
fn expect_number(parser: &mut Parser, kind: Kind, recovery: TokenSet) -> bool {
    fn leading_zeros(bytes: &[u8]) -> bool {
        match bytes {
            [_b] => false, // one leading zero is just a 0
            [b'0', ..] => true,
            _ => false,
        }
    }

    let has_neg = parser.matches(0, Kind::Hyphen);
    let num_pos = if has_neg { 1 } else { 0 };
    if parser.matches(num_pos, Kind::DecimalLike) {
        if leading_zeros(parser.nth_raw(num_pos)) {
            if has_neg {
                assert!(parser.eat(Kind::Hyphen));
            }
            parser.err_and_bump("Number cannot have leading zeros.");
        } else if has_neg {
            parser.eat_remap2(kind);
        } else {
            parser.eat_remap(kind);
        }
        true
    } else {
        if has_neg {
            parser.eat_raw();
        }
        // always fails, but generates our error message
        parser.expect_recover(kind, recovery);
        // if we ate a '-' we can still advance
        has_neg
    }
}

fn expect_device(parser: &mut Parser, recovery: TokenSet) -> bool {
    debug_assert!(parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw));
    parser.eat_trivia();
    parser.start_node(Kind::DeviceKw);
    parser.expect(Kind::LAngle);
    parser.expect(Kind::DeviceKw);
    if expect_number(parser, Kind::Number, recovery)
        && expect_number(parser, Kind::Number, recovery) &&
        parser.eat(Kind::Comma) && expect_number(parser, Kind::Number, recovery) {
            expect_number(parser, Kind::Number, recovery);
        }
    //}
    // FIXME: this should handle an arbitary number of pairs? but also isn't
    // supported yet?
    // I don't know what's going on tbh
    parser.expect(Kind::RAngle)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{DebugSink, SyntaxError};

    fn debug_parse_output(text: &str, f: impl FnOnce(&mut Parser)) -> DebugSink {
        let mut sink = DebugSink::default();
        let mut parser = Parser::new(text, &mut sink);
        f(&mut parser);
        sink
    }

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
        let fea = "markClass [acute] <anchor 350 0> @TOP_MARKS";
        let out = debug_parse_output(fea, |parser| mark_class(parser));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        //crate::assert_eq_str!(
    }
}
