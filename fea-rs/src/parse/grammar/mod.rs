use super::{Kind, Parser, TokenSet};

mod feature;
mod glyph;
mod gpos;
//FIXME: figure out what to expose here, and how.
pub mod gsub;
mod metrics;
mod table;

/// Entry point for parsing a FEA file.
pub fn root(parser: &mut Parser) {
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
        lookup_block_or_reference(parser, TokenSet::TOP_LEVEL)
    } else if parser.matches(0, Kind::LanguagesystemKw) {
        language_system(parser)
    } else if parser.matches(0, Kind::FeatureKw) {
        feature::feature(parser)
    } else if parser.matches(0, Kind::MarkClassKw) {
        mark_class(parser)
    } else if parser.matches(0, Kind::AnchorDefKw) {
        anchor_def(parser)
    } else if parser.matches(0, Kind::AnonKw) {
        anonymous(parser)
    } else if parser.matches(0, Kind::NamedGlyphClass) {
        glyph::named_glyph_class_decl(parser, TokenSet::TOP_LEVEL)
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

pub fn language_system(parser: &mut Parser) {
    fn language_system_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::LanguagesystemKw));
        if !parser.expect_tag(Kind::Semi) || !parser.expect_tag(Kind::Semi) {
            return advance_to_top_level(parser);
        }
        parser.expect_semi();
    }

    parser.eat_trivia();
    parser.start_node(Kind::LanguageSystemNode);
    language_system_body(parser);
    parser.finish_node();
}

fn include(parser: &mut Parser) {
    fn include_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::IncludeKw));
        if !parser.expect(Kind::LParen) {
            advance_to_top_level(parser);
        }
        //FIXME: really anything can be a path?
        if !parser.eat_remap(TokenSet::IDENT_LIKE, Kind::Path) {
            parser.err("Include statement missing path");
            return advance_to_top_level(parser);
        }
        if !parser.expect(Kind::RParen) {
            return advance_to_top_level(parser);
        }
        parser.expect_semi();
    }

    parser.start_node(Kind::IncludeNode);
    include_body(parser);
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
        parser.expect_semi();
    }

    parser.eat_trivia();
    parser.start_node(Kind::LookupFlagNode);
    lookupflag_body(parser, recovery);
    parser.finish_node();
}

fn lookup_block(parser: &mut Parser, recovery: TokenSet) {
    // returns true if we advanced the parser
    fn lookup_item(parser: &mut Parser, recovery: TokenSet) -> bool {
        let start_pos = parser.nth_range(0).start;
        match parser.nth(0).kind {
            Kind::LookupflagKw => lookupflag(parser, recovery),
            Kind::MarkClassKw => mark_class(parser),
            Kind::ScriptKw => {
                eat_script(parser, recovery);
            }
            Kind::NamedGlyphClass => glyph::named_glyph_class_decl(parser, recovery),
            Kind::SubtableKw => {
                parser.eat_raw();
                parser.expect_semi();
            }
            Kind::PosKw | Kind::SubKw | Kind::RsubKw | Kind::IgnoreKw | Kind::EnumKw => {
                feature::pos_or_sub_rule(parser, recovery)
            }
            _ => {
                parser.err(format!(
                    "'{}' Not valid in a lookup block",
                    parser.current_token_text()
                ));
                parser.eat_until(TokenSet::TOP_AND_FEATURE.add(Kind::RBrace));
            }
        }
        parser.nth_range(0).start != start_pos
    }

    fn lookup_body(parser: &mut Parser, recovery: TokenSet) {
        const LABEL_RECOVERY: TokenSet = TokenSet::new(&[Kind::UseExtensionKw, Kind::LBrace]);
        assert!(parser.eat(Kind::LookupKw));
        let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
        parser.expect_remap_recover(
            TokenSet::IDENT_LIKE,
            Kind::Label,
            LABEL_RECOVERY.union(recovery),
        );

        parser.eat(Kind::UseExtensionKw);
        parser.expect(Kind::LBrace);
        while !parser.at_eof() && !parser.matches(0, Kind::RBrace) {
            if !lookup_item(parser, recovery) {
                if let Some(range) = raw_label_range {
                    parser.raw_error(range, "Table is never closed");
                }
                break;
            }
        }
        parser.expect_recover(
            Kind::RBrace,
            recovery.union(TokenSet::IDENT_LIKE.union(Kind::Semi.into())),
        );
        parser.expect_remap_recover(
            TokenSet::IDENT_LIKE,
            Kind::Label,
            recovery.union(Kind::Semi.into()),
        );
        parser.expect_semi();
    }

    parser.eat_trivia();
    parser.start_node(Kind::LookupBlockNode);
    lookup_body(parser, recovery);
    parser.finish_node();
}

//either lookup <label> { ... } <label>;
//or     lookup <label>;
//FIXME: doesn't handle IDENT_LIKE
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
        parser.expect_semi();
        parser.finish_node();
    } else {
        parser.eat(Kind::LookupKw);
        if parser.eat(Kind::Ident) {
            parser.err_before_ws("Expected ';' or '{'");
            parser.eat_unless(recovery);
        } else {
            parser.expect_recover(Kind::Ident, recovery);
        }
    }
}

fn eat_script(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !parser.matches(0, Kind::ScriptKw) {
        return false;
    }
    parser.eat_trivia();
    parser.start_node(Kind::ScriptNode);
    parser.eat_raw();
    parser.expect_tag(recovery.union(Kind::Semi.into()));
    parser.expect_semi();
    parser.finish_node();
    true
}

fn eat_language(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !parser.matches(0, Kind::LanguageKw) {
        return false;
    }
    parser.eat_trivia();
    parser.start_node(Kind::LanguageNode);
    parser.eat_raw();
    parser.expect_tag(recovery.union(Kind::Semi.into()));
    parser.eat(Kind::ExcludeDfltKw);
    parser.eat(Kind::IncludeDfltKw);
    parser.eat(Kind::RequiredKw);
    parser.expect_semi();
    parser.finish_node();
    true
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
            parser.eat_remap(TokenSet::IDENT_LIKE, Kind::GlyphName)
        }
    }

    fn mark_class_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::MarkClassKw));
        if !glyph_or_class(parser) {
            parser.err("Expected glyph name or class");
        }
        metrics::anchor(parser, TokenSet::new(&[Kind::Semi, Kind::NamedGlyphClass]));
        parser.expect_recover(Kind::NamedGlyphClass, Kind::Semi);
        parser.expect_semi();
    }

    parser.start_node(Kind::MarkClassNode);
    mark_class_body(parser);
    parser.finish_node();
}

fn anchor_def(parser: &mut Parser) {
    fn anchor_def_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::AnchorDefKw));
        parser.eat_trivia();
        parser.start_node(Kind::AnchorNode);
        let recovery = TokenSet::TOP_LEVEL
            .union(TokenSet::IDENT_LIKE)
            .union(TokenSet::new(&[Kind::ContourpointKw, Kind::Semi]));
        parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery);
        parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery);
        if parser.eat(Kind::ContourpointKw) {
            parser.expect_recover(Kind::Number, TokenSet::TOP_LEVEL.union(Kind::Semi.into()));
        }
        parser.finish_node();
        parser.expect_remap_recover(TokenSet::IDENT_LIKE, Kind::Ident, TokenSet::TOP_SEMI);
        parser.expect_semi();
    }

    parser.eat_trivia();
    parser.start_node(Kind::AnchorDefNode);
    anchor_def_body(parser);
    parser.finish_node();
}

fn anonymous(parser: &mut Parser) {
    fn anon_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::AnonKw));
        let raw_label_range = parser
            .matches(0, TokenSet::IDENT_LIKE)
            .then(|| parser.nth_range(0));
        if !(parser.expect_recover(TokenSet::IDENT_LIKE, TokenSet::new(&[Kind::LBrace]))
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
                            && parser.eat(TokenSet::IDENT_LIKE)
                            && parser.eat(Kind::Semi)
                    );
                    break;
                }
                _ => {
                    if parser.nth(1).kind == Kind::Eof {
                        parser.raw_error(raw_label_range.unwrap(), "unterminated anonymous block");
                        parser.eat_raw();
                        break;
                    }
                    parser.eat_raw();
                }
            }
        }
    }

    parser.eat_trivia();
    parser.start_node(Kind::AnonBlockNode);
    anon_body(parser);
    parser.finish_node();
}

#[cfg(test)]
fn debug_parse_output(
    text: &str,
    f: impl FnOnce(&mut Parser),
) -> (crate::Node, Vec<crate::SyntaxError>, String) {
    let mut sink = crate::AstSink::new(text, None);
    let mut parser = Parser::new(text, &mut sink);
    f(&mut parser);
    sink.finish_stringified()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn languagesystem() {
        let input = "languagesystem dflt cool;";
        let (out, errors, _err_str) = debug_parse_output(input, |parser| language_system(parser));
        assert!(errors.is_empty());
        crate::assert_eq_str!(
            out.simple_parse_tree(),
            "\
START LanguageSystemNode
  LanguagesystemKw
  WS( )
  Tag
  WS( )
  Tag
  ;
END LanguageSystemNode
"
        )
    }

    #[test]
    fn languagesystem_trivia() {
        let input = "# hi\nlanguagesystem dflt cool;";
        let (out, errors, _str) = debug_parse_output(input, |parser| root(parser));
        assert!(errors.is_empty());
        crate::assert_eq_str!(
            out.simple_parse_tree(),
            "\
START FILE
  #(# hi)
  WS(\\n)
  START LanguageSystemNode
    LanguagesystemKw
    WS( )
    Tag
    WS( )
    Tag
    ;
  END LanguageSystemNode
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
        let (out, _errors, _str) = debug_parse_output(input, root);
        let exp = "\
START FILE
  START LanguageSystemNode
    LanguagesystemKw
    WS( )
    Tag
    WS( )
    Tag
    ;
  END LanguageSystemNode
  WS(\\n)
  START LanguageSystemNode
    LanguagesystemKw
    WS( )
    Tag
    WS( )
    Tag
    ;
  END LanguageSystemNode
  WS(\\n)
  START IncludeNode
    IncludeKw
    (
    Path
    )
    ;
  END IncludeNode
  WS(\\n)
END FILE
";

        crate::assert_eq_str!(out.simple_parse_tree(), exp);
    }

    #[test]
    fn mark_class_() {
        let fea = "markClass [acute] <anchor 350 0> @TOP_MARKS;";
        let (out, errors, _str) = debug_parse_output(fea, |parser| mark_class(parser));
        assert!(errors.is_empty(), "{}", _str);
        crate::assert_eq_str!(
            "\
START MarkClassNode
  MarkClassKw
  WS( )
  START GlyphClass
    [
    GlyphName(acute)
    ]
  END GlyphClass
  WS( )
  START AnchorNode
    <
    AnchorKw
    WS( )
    METRIC(350)
    WS( )
    METRIC(0)
    >
  END AnchorNode
  WS( )
  @GlyphClass(@TOP_MARKS)
  ;
END MarkClassNode
",
            out.simple_parse_tree(),
        );
    }
}
