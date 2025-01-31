use super::lexer::{Kind, TokenSet};
use super::Parser;
use crate::token_tree::Kind as AstKind;

mod feature;
mod glyph;
mod gpos;
mod gsub;
mod metrics;
mod table;
mod variations;

// we use these in tests in various places
#[cfg(test)]
pub(crate) use self::{gsub::gsub_rule, metrics::expect_glyphs_number_value};

/// Entry point for parsing a FEA file.
pub fn root(parser: &mut Parser) {
    parser.start_node(AstKind::SourceFile);
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
    } else if parser.matches(0, Kind::ConditionSetKw) {
        variations::condition_set(parser)
    } else if parser.matches(0, Kind::VariationKw) {
        variations::variation(parser)
    } else if parser.matches(0, Kind::ValueRecordDefKw) {
        value_record_def(parser, TokenSet::TOP_LEVEL)
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
    parser.in_node(AstKind::LanguageSystemNode, |parser| {
        assert!(parser.eat(Kind::LanguagesystemKw));
        if parser.expect_tag(Kind::Semi).is_none() || parser.expect_tag(Kind::Semi).is_none() {
            return advance_to_top_level(parser);
        }
        parser.expect_semi();
    })
}

fn include(parser: &mut Parser) {
    fn include_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::IncludeKw));
        if !parser.expect(Kind::LParen) {
            advance_to_top_level(parser);
        }

        // during lexing we include leading and trailing whitespace as
        // part of the path, but now we want to trim that if the path
        // contains any non-whitespace.
        // (we don't do this at lex time because it requires arbitrary lookahead)
        if !parser.split_remap_current(Kind::Path, |path, buf| {
            let leading = path.bytes().take_while(|b| b.is_ascii_whitespace()).count();
            let trailing = path
                .bytes()
                .rev()
                .take_while(|b| b.is_ascii_whitespace())
                .count();
            // if no whitespace or all whitespace, call it all a path.
            // in the latter case, we'll error when we try to read the path.
            if leading + trailing == 0 || leading == path.len() {
                buf.push((0..path.len(), AstKind::Path));
                return;
            }

            if leading > 0 {
                buf.push((0..leading, AstKind::Whitespace));
            }
            buf.push((leading..path.len() - trailing, AstKind::Path));
            if trailing > 0 {
                buf.push((path.len() - trailing..path.len(), AstKind::Whitespace));
            }
        }) {
            parser.err("Include statement missing path");
            return advance_to_top_level(parser);
        }
        if !parser.expect(Kind::RParen) {
            return advance_to_top_level(parser);
        }
        if !parser.eat(Kind::Semi) {
            parser.warn_before_ws("include statement is missing ';'");
        }
    }

    parser.in_node(AstKind::IncludeNode, include_body)
}

fn table(parser: &mut Parser) {
    table::table(parser)
}

//either lookup <label> { ... } <label>;
//or     lookup <label>;
fn lookup_block_or_reference(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.matches(0, Kind::LookupKw));
    if parser.matches(2, Kind::LBrace)
        || (parser.matches(2, Kind::UseExtensionKw) && parser.matches(3, Kind::LBrace))
    {
        feature::lookup_block(parser, recovery.union(TokenSet::STATEMENT));
    } else if parser.matches(2, Kind::Semi) {
        parser.in_node(AstKind::LookupRefNode, |parser| {
            assert!(parser.eat(Kind::LookupKw));
            parser.eat_remap(TokenSet::IDENT_LIKE, AstKind::Ident);
            parser.expect_semi();
        })
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
    parser.in_node(AstKind::ScriptNode, |parser| {
        assert!(parser.eat(Kind::ScriptKw));
        parser.expect_tag(recovery.union(TokenSet::SEMI));
        parser.expect_semi();
    });
    true
}

fn eat_language(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !parser.matches(0, Kind::LanguageKw) {
        return false;
    }
    parser.in_node(AstKind::LanguageNode, |parser| {
        assert!(parser.eat(Kind::LanguageKw));
        parser.expect_tag(recovery.union(TokenSet::SEMI));
        parser.eat(Kind::ExcludeDfltKw);
        parser.eat(Kind::IncludeDfltKw);
        parser.eat(Kind::RequiredKw);
        parser.expect_semi();
    });
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
            parser.eat_remap(TokenSet::IDENT_LIKE, AstKind::GlyphName)
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

    parser.in_node(AstKind::MarkClassNode, mark_class_body);
}

fn anchor_def(parser: &mut Parser) {
    fn anchor_def_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::AnchorDefKw));
        parser.in_node(AstKind::AnchorNode, |parser| {
            let recovery = TokenSet::TOP_LEVEL
                .union(TokenSet::IDENT_LIKE)
                .union(TokenSet::new(&[Kind::ContourpointKw, Kind::Semi]));
            parser.repeat(|parser| metrics::expect_metric(parser, recovery), 2);
            if parser.eat(Kind::ContourpointKw) {
                parser.expect_recover(Kind::Number, TokenSet::TOP_SEMI);
            }
        });
        parser.expect_remap_recover(TokenSet::IDENT_LIKE, AstKind::Ident, TokenSet::TOP_SEMI);
        parser.expect_semi();
    }

    parser.in_node(AstKind::AnchorDefNode, anchor_def_body);
}

fn value_record_def(parser: &mut Parser, recovery: TokenSet) {
    fn value_record_def_body(parser: &mut Parser, recovery: TokenSet) {
        assert!(parser.eat(Kind::ValueRecordDefKw));
        metrics::expect_value_record(parser, recovery);
        parser.expect_remap_recover(TokenSet::IDENT_LIKE, AstKind::Ident, recovery);
        parser.expect_semi();
    }

    parser.in_node(AstKind::ValueRecordDefNode, |parser| {
        value_record_def_body(parser, recovery)
    })
}

fn anonymous(parser: &mut Parser) {
    fn anon_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::AnonKw));
        let raw_label_range = parser
            .matches(0, TokenSet::IDENT_LIKE)
            .then(|| parser.nth_range(0));
        if !(parser.expect_recover(TokenSet::IDENT_LIKE, Kind::LBrace)
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

    parser.in_node(AstKind::AnonBlockNode, anon_body);
}

/// Common between gpos/gsub
fn expect_ignore_pattern_body(parser: &mut Parser, recovery: TokenSet) -> bool {
    let recovery = recovery.add(Kind::Semi);
    if !eat_ignore_statement_item(parser, recovery) {
        parser.err_recover("Expected ignore pattern", recovery);
        parser.eat_until(recovery);
        return false;
    }

    while parser.eat(Kind::Comma) {
        eat_ignore_statement_item(parser, recovery);
    }
    parser.expect_semi();
    true
}

fn eat_ignore_statement_item(parser: &mut Parser, recovery: TokenSet) -> bool {
    let recovery = recovery.union(Kind::Comma.into());
    // eat backtrack + first mark glyph
    if !glyph::eat_glyph_or_glyph_class(parser, recovery) {
        return false;
    }
    while glyph::eat_glyph_or_glyph_class(parser, recovery) {
        continue;
    }

    // expect a marked glyph.
    // if this is omitted we will continue parsing, and warn when we rewrite.
    if parser.eat(Kind::SingleQuote) {
        loop {
            glyph::eat_glyph_or_glyph_class(parser, recovery);
            if !parser.eat(Kind::SingleQuote) {
                break;
            }
        }
    }

    // eat any suffix sequence
    while glyph::eat_glyph_or_glyph_class(parser, recovery) {
        continue;
    }
    true
}

/// take an eat_ method and call it until it returns false.
fn greedy<F: FnMut(&mut Parser, TokenSet) -> bool>(
    mut f: F,
) -> impl FnMut(&mut Parser, TokenSet) -> bool {
    move |parser, recovery| {
        if !f(parser, recovery) {
            return false;
        }
        while f(parser, recovery) {
            continue;
        }
        true
    }
}

#[cfg(test)]
fn debug_parse_output(
    text: &str,
    f: impl FnOnce(&mut Parser),
) -> (crate::NodeOrToken, Vec<crate::Diagnostic>, String) {
    use super::Source;

    let source = Source::new("debug_parse_output".into(), text.into());
    let mut sink = crate::token_tree::AstSink::new(text, source.id(), None);
    let mut parser = Parser::new(text, &mut sink);
    f(&mut parser);
    let (node, errs, _) = sink.finish();
    let mut err_str = String::new();
    for err in &errs {
        if !err_str.is_empty() {
            err_str.push('\n');
        }
        crate::util::highlighting::write_diagnostic(&mut err_str, err, &source, Some(80), true);
    }
    (node.into(), errs, err_str)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_tree::typed::{self, AstNode};

    #[test]
    fn no_cv_param_in_lookup() {
        let fea = "lookup hi {cvParameters {}; } hi ;";
        let (_out, errors, _errstr) = debug_parse_output(fea, root);
        assert!(!errors.is_empty(), "{}", fea);
        assert!(errors.first().unwrap().text().contains("cvParameters"));
    }

    #[test]
    fn simple_value_record_def() {
        let fea = "valueRecordDef 123 foo;";
        let (out, errors, _errstr) = debug_parse_output(fea, root);
        assert!(errors.is_empty());

        let out = typed::Root::cast(&out).unwrap();
        let value_def = out.iter().find_map(typed::ValueRecordDef::cast).unwrap();
        assert_eq!(
            value_def
                .value_record()
                .advance()
                .and_then(|x| x.parse_simple()),
            Some(123)
        );
        assert_eq!(value_def.name().as_str(), "foo");
    }

    fn assert_include_path_matches(fea: &str, path: &str) {
        let (out, errors, _errstr) = debug_parse_output(fea, include);
        assert!(errors.is_empty(), "{errors:?}");

        let out = typed::Include::cast(&out).unwrap();
        assert_eq!(out.path().as_str(), path);
    }

    #[test]
    fn parse_include_with_space() {
        assert_include_path_matches("include (myfile.fea);", "myfile.fea");
    }

    #[test]
    fn parse_include_leading_ws() {
        assert_include_path_matches("include( myfile.fea);", "myfile.fea");
    }

    #[test]
    fn parse_include_trailing_ws() {
        assert_include_path_matches("include(myfile.fea );", "myfile.fea");
    }

    #[test]
    fn parse_include_leading_and_trailing_ws() {
        assert_include_path_matches("include( myfile.fea );", "myfile.fea");
    }

    #[test]
    fn parse_include_all_whitespace() {
        assert_include_path_matches("include(  );", "  ");
    }
}
