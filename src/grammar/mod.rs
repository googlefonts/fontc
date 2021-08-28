use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

mod glyph;
mod gpos;
mod gsub;
mod metrics;
mod table;

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
        lookup_block(parser, TokenSet::TOP_LEVEL)
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
        if !parser.expect_tag(Kind::Semi) || !parser.expect_tag(Kind::Semi) {
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

fn table(parser: &mut Parser) {
    table::table(parser)
}

fn lookupflag(parser: &mut Parser, recovery: TokenSet) {
    fn eat_named_lookup_value(parser: &mut Parser, recovery: TokenSet) -> bool {
        match parser.nth(0).kind {
            Kind::RightToLeftKw
            | Kind::IgnoreBaseGlyphsKw
            | Kind::IgnoreMarksKw
            | Kind::IgnoreLigaturesKw => {
                parser.eat_raw();
                true
            }
            Kind::MarkAttachmentTypeKw | Kind::UseMarkFilteringSetKw => {
                parser.eat_raw();
                if !parser.eat(Kind::NamedGlyphClass)
                    && !glyph::eat_glyph_class_list(parser, recovery)
                {
                    parser.err("lookupflag '{}' must be followed by a glyph class.");
                }
                true
            }
            _ => false,
        }
    }

    fn lookupflag_body(parser: &mut Parser, recovery: TokenSet) {
        assert!(parser.eat(Kind::LookupflagKw));
        if !parser.eat(Kind::Number) {
            while eat_named_lookup_value(parser, recovery) {
                continue;
            }
        }
        parser.expect_recover(Kind::Semi, recovery);
    }

    parser.eat_trivia();
    parser.start_node(Kind::LookupflagKw);
    lookupflag_body(parser, recovery);
    parser.finish_node();
}

fn lookup_block(parser: &mut Parser, recovery: TokenSet) {
    fn eat_lookup_item(parser: &mut Parser, recovery: TokenSet) -> bool {
        let start_pos = parser.nth_range(0).start;
        match parser.nth(0).kind {
            Kind::LookupflagKw => lookupflag(parser, recovery),
            Kind::MarkClassKw => mark_class(parser),
            Kind::ScriptKw => {
                eat_script(parser, recovery);
            }
            Kind::SubtableKw => {
                parser.eat_raw();
                parser.expect_recover(Kind::Semi, recovery);
            }
            Kind::PosKw | Kind::SubKw | Kind::RsubKw | Kind::IgnoreKw | Kind::EnumKw => {
                pos_or_sub_rule(parser, recovery)
            }
            _ => (),
        }
        parser.nth_range(0).start != start_pos
    }

    fn lookup_body(parser: &mut Parser, recovery: TokenSet) {
        const LABEL_RECOVERY: TokenSet = TokenSet::new(&[Kind::UseExtensionKw, Kind::LBrace]);
        assert!(parser.eat(Kind::LookupKw));
        //let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
        parser.expect_remap_recover(Kind::Ident, Kind::Label, LABEL_RECOVERY);
        parser.eat(Kind::UseExtensionKw);
        parser.expect(Kind::LBrace);
        while eat_lookup_item(parser, recovery) {
            continue;
        }
        parser.expect_recover(
            Kind::RBrace,
            recovery.union(TokenSet::new(&[Kind::Ident, Kind::Semi])),
        );
        parser.expect_remap_recover(Kind::Ident, Kind::Label, recovery.union(Kind::Semi.into()));
        parser.expect_recover(Kind::Semi, recovery);
    }

    parser.eat_trivia();
    parser.start_node(Kind::LookupBlockNode);
    lookup_body(parser, recovery);
    parser.finish_node();
}

//either lookup <label> { ... } <label>;
//or     lookup <label>;
fn lookup_block_or_reference(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.matches(0, Kind::LookupKw));
    let kind1 = parser.nth(1).kind;
    let kind2 = parser.nth(2).kind;
    let (ref_like, block_like) = match (kind1, kind2) {
        (Kind::Ident, Kind::Semi) => (true, false),
        (Kind::Ident, Kind::LBrace) => (false, true),
        (Kind::Semi, _) => (true, false),   // ref without ident
        (Kind::LBrace, _) => (false, true), // block without ident
        _ => (false, false),
    };
    if block_like {
        lookup_block(parser, recovery);
    } else if ref_like {
        parser.eat_trivia();
        parser.start_node(Kind::LookupRefNode);
        parser.eat(Kind::LookupKw);
        parser.expect_recover(Kind::Ident, recovery.union(Kind::Semi.into()));
        parser.expect_recover(Kind::Semi, recovery);
        parser.finish_node();
    } else {
        parser.eat(Kind::LookupKw);
        if parser.eat(Kind::Ident) {
            parser.err_recover("Expected ';' or '{', found '{}'", parser.nth(0).kind);
        } else {
            parser.expect_recover(Kind::Ident, recovery);
        }
    }
}

fn feature(parser: &mut Parser) {
    fn feature_body_item(parser: &mut Parser) -> bool {
        let start_pos = parser.nth_range(0).start;
        match parser.nth(0).kind {
            Kind::PosKw | Kind::SubKw | Kind::RsubKw | Kind::IgnoreKw | Kind::EnumKw => {
                pos_or_sub_rule(parser, TokenSet::FEATURE_BODY_ITEM)
            }
            Kind::NamedGlyphClass => named_glyph_class_decl(parser),
            Kind::MarkClassKw => mark_class(parser),
            Kind::ParametersKw => metrics::parameters(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::SubtableKw => {
                parser.eat_raw();
                parser.expect_recover(Kind::Semi, TokenSet::FEATURE_BODY_ITEM);
            }
            Kind::LookupKw => lookup_block_or_reference(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::LookupflagKw => lookupflag(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::ScriptKw => {
                eat_script(parser, TokenSet::FEATURE_BODY_ITEM);
            }
            Kind::LanguageKw => {
                parser.eat_trivia();
                parser.start_node(Kind::LanguageKw);
                parser.eat_raw();
                parser.expect_tag(TokenSet::FEATURE_BODY_ITEM.union(Kind::Semi.into()));
                parser.eat(Kind::ExcludeDfltKw);
                parser.eat(Kind::IncludeDfltKw);
                parser.eat(Kind::RequiredKw);
                parser.expect_recover(Kind::Semi, TokenSet::FEATURE_BODY_ITEM);
            }
            Kind::FeatureKw => {
                // aalt only
                if parser.matches(1, Kind::Ident) && parser.matches(2, Kind::Semi) {
                    assert!(parser.eat(Kind::FeatureKw));
                    parser.expect_tag(TokenSet::EMPTY);
                    assert!(parser.eat(Kind::Semi));
                }
            }
            _ => (),
        }
        parser.nth_range(0).start != start_pos
    }

    fn feature_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::FeatureKw));
        // if there's a tag, stash the range
        // keywords that could be valid tags
        const KEYWORD_TAGS: TokenSet = TokenSet::new(&[
            Kind::MarkKw,
            Kind::AnonKw,
            Kind::ByKw,
            Kind::FromKw,
            Kind::PosKw,
            Kind::RsubKw,
        ]);
        let tag_kind = if parser.matches(0, KEYWORD_TAGS) && parser.nth_raw(0).len() <= 4 {
            parser.nth(0).kind
        } else {
            Kind::Ident
        };

        parser.expect_remap_recover(
            tag_kind,
            Kind::Ident,
            TokenSet::new(&[Kind::UseExtensionKw, Kind::LBrace]),
        );
        parser.eat(Kind::UseExtensionKw);
        parser.expect(Kind::LBrace);
        while feature_body_item(parser) {
            continue;
        }
        parser.expect_recover(Kind::RBrace, TokenSet::TOP_SEMI);
        parser.expect_remap_recover(tag_kind, Kind::Ident, TokenSet::TOP_LEVEL);
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    }

    parser.eat_trivia();
    parser.start_node(Kind::FeatureKw);
    feature_body(parser);
    parser.finish_node();
}

fn eat_script(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !parser.matches(0, Kind::ScriptKw) {
        return false;
    }
    parser.eat_trivia();
    parser.start_node(Kind::ScriptKw);
    parser.eat_raw();
    parser.expect_tag(recovery.union(Kind::Semi.into()));
    parser.expect_recover(Kind::Semi, recovery);
    parser.finish_node();
    true
}

fn pos_or_sub_rule(parser: &mut Parser, recovery: TokenSet) {
    match parser.nth(0).kind {
        Kind::PosKw => gpos::gpos(parser, recovery),
        Kind::EnumKw if parser.nth(1).kind == Kind::PosKw => gpos::gpos(parser, recovery),
        Kind::EnumKw => parser.err_and_bump("'enum' keyword must be followed by position rule"),
        Kind::IgnoreKw => match parser.nth(1).kind {
            Kind::PosKw => gpos::gpos(parser, recovery),
            Kind::SubKw => gsub::gsub(parser, recovery),
            _ => parser
                .err_and_bump("'ignore' keyword must be followed by position or substitution rule"),
        },
        Kind::SubKw | Kind::RsubKw => gsub::gsub(parser, recovery),
        other => panic!("'{}' is not a valid gpos or gsub token", other),
    }
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
        parser.expect_recover(Kind::NamedGlyphClass, Kind::Semi);
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    }

    parser.start_node(Kind::MarkClassKw);
    mark_class_body(parser);
    parser.finish_node();
}

fn anchor_def(parser: &mut Parser) {
    fn anchor_def_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::AnchorDefKw));
        let recovery = TokenSet::TOP_LEVEL.union(TokenSet::new(&[
            Kind::ContourpointKw,
            Kind::Ident,
            Kind::Semi,
        ]));
        parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery);
        parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery);
        if parser.eat(Kind::ContourpointKw) {
            parser.expect_recover(Kind::Number, TokenSet::TOP_LEVEL.union(Kind::Semi.into()));
        }
        parser.expect_recover(Kind::Ident, TokenSet::TOP_SEMI);
        parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
    }

    parser.eat_trivia();
    parser.start_node(Kind::AnchorDefKw);
    anchor_def_body(parser);
    parser.finish_node();
}

fn anonymous(parser: &mut Parser) {
    fn anon_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::AnonKw));
        let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
        if !(parser.expect_recover(Kind::Ident, TokenSet::new(&[Kind::LBrace]))
            & parser.expect(Kind::LBrace))
        {
            return;
        }
        loop {
            match parser.nth(0).kind {
                Kind::RBrace
                    if parser.nth_raw(1) == parser.raw_range(raw_label_range.clone().unwrap())
                        && parser.matches(2, Kind::Semi) =>
                {
                    assert!(
                        parser.eat(Kind::RBrace)
                            && parser.eat(Kind::Ident)
                            && parser.eat(Kind::Semi)
                    );
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

    parser.eat_trivia();
    parser.start_node(Kind::AnonKw);
    anon_body(parser);
    parser.finish_node();
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
