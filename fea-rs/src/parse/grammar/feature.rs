use super::{glyph, gpos, gsub, metrics};
use crate::parse::{Kind, Parser, TokenSet};

pub(crate) fn feature(parser: &mut Parser) {
    // returns true if we advanced the parser.
    fn feature_body_item(parser: &mut Parser) -> bool {
        let start_pos = parser.nth_range(0).start;
        match parser.nth(0).kind {
            Kind::PosKw | Kind::SubKw | Kind::RsubKw | Kind::IgnoreKw | Kind::EnumKw => {
                pos_or_sub_rule(parser, TokenSet::FEATURE_BODY_ITEM)
            }
            Kind::NamedGlyphClass => glyph::named_glyph_class_decl(
                parser,
                TokenSet::TOP_LEVEL.union(TokenSet::FEATURE_BODY_ITEM),
            ),
            Kind::MarkClassKw => super::mark_class(parser),
            Kind::ParametersKw => metrics::parameters(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::SubtableKw => {
                parser.eat_raw();
                parser.expect_recover(Kind::Semi, TokenSet::FEATURE_BODY_ITEM);
            }
            Kind::LookupKw => super::lookup_block_or_reference(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::LookupflagKw => super::lookupflag(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::ScriptKw => {
                super::eat_script(parser, TokenSet::FEATURE_BODY_ITEM);
            }
            Kind::LanguageKw => {
                super::eat_language(parser, TokenSet::FEATURE_BODY_ITEM);
            }
            Kind::FeatureKw => {
                // aalt only
                if parser.matches(1, TokenSet::IDENT_LIKE) && parser.matches(2, Kind::Semi) {
                    assert!(parser.eat(Kind::FeatureKw));
                    parser.expect_tag(TokenSet::EMPTY);
                    assert!(parser.eat(Kind::Semi));
                }
            }
            Kind::SizemenunameKw => {
                parser.start_node(Kind::SizemenunameKw);
                assert!(parser.eat(Kind::SizemenunameKw));
                metrics::expect_name_record(parser, TokenSet::FEATURE_BODY_ITEM);
                parser.expect_recover(Kind::Semi, TokenSet::FEATURE_BODY_ITEM);
                parser.finish_node();
            }
            Kind::CvParametersKw => cv_parameters(parser, TokenSet::FEATURE_BODY_ITEM),
            Kind::FeatureNamesKw => feature_names(parser, TokenSet::FEATURE_BODY_ITEM),

            _ => {
                parser.err(format!(
                    "'{}' Not valid in a feature block",
                    parser.current_token_text()
                ));
                parser.eat_until(TokenSet::TOP_AND_FEATURE.add(Kind::RBrace));
            }
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
            Kind::Ident,
        ]);

        let tag_kind = (parser.matches(0, KEYWORD_TAGS) && parser.nth_raw(0).len() <= 4)
            .then(|| (parser.nth(0).kind));
        let tag_range = tag_kind.as_ref().map(|_| parser.nth_range(0));
        let tag_kind = tag_kind.unwrap_or(Kind::Ident);

        if !parser.eat_remap(tag_kind, Kind::Tag) {
            parser.err_recover(
                "Expected feature tag",
                TokenSet::new(&[Kind::UseExtensionKw, Kind::LBrace]),
            );
        }

        parser.eat(Kind::UseExtensionKw);
        parser.expect(Kind::LBrace);
        while !parser.at_eof() && !parser.matches(0, Kind::RBrace) {
            if !feature_body_item(parser) {
                if let Some(tag_range) = tag_range {
                    parser.raw_error(tag_range, "Feature block is unclosed.");
                }
                break;
            }
        }
        parser.expect_recover(Kind::RBrace, TokenSet::TOP_SEMI);
        parser.expect_remap_recover(tag_kind, Kind::Ident, TokenSet::TOP_LEVEL);
        parser.expect_semi();
    }

    parser.eat_trivia();
    parser.start_node(Kind::FeatureNode);
    feature_body(parser);
    parser.finish_node();
}

pub(crate) fn pos_or_sub_rule(parser: &mut Parser, recovery: TokenSet) {
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
fn name_entry(parser: &mut Parser, recovery: TokenSet) {
    if parser.expect(Kind::NameKw) {
        metrics::expect_name_record(parser, recovery);
    } else {
        parser.eat_until(recovery);
    }
    parser.expect_semi();
}

fn feature_names(parser: &mut Parser, recovery: TokenSet) {
    let name_recovery = recovery.union(TokenSet::new(&[Kind::NameKw, Kind::RBrace, Kind::Semi]));

    parser.start_node(Kind::FeatureNamesKw);
    assert!(parser.eat(Kind::FeatureNamesKw));
    parser.expect_recover(Kind::LBrace, name_recovery);
    while !parser.at_eof() && !parser.matches(0, recovery.add(Kind::RBrace)) {
        name_entry(parser, name_recovery);
    }
    parser.expect_recover(Kind::RBrace, name_recovery);
    parser.expect_semi();
    parser.finish_node();
}

fn cv_parameters(parser: &mut Parser, recovery: TokenSet) {
    const UNICODE_VALUE: TokenSet = TokenSet::new(&[Kind::Number, Kind::Hex]);
    const PARAM_KEYWORDS: TokenSet = TokenSet::new(&[
        Kind::FeatUiLabelNameIdKw,
        Kind::FeatUiTooltipTextNameIdKw,
        Kind::SampleTextNameIdKw,
        Kind::ParamUiLabelNameIdKw,
        Kind::CharacterKw,
    ]);

    fn entry(parser: &mut Parser, recovery: TokenSet) {
        if parser.eat(Kind::CharacterKw) {
            parser.expect_recover(UNICODE_VALUE, recovery);
            parser.expect_semi();
        } else if parser.matches(0, PARAM_KEYWORDS) {
            parser.start_node(parser.nth(0).kind);
            assert!(parser.eat(PARAM_KEYWORDS));
            parser.expect_recover(Kind::LBrace, recovery.add(Kind::NameKw));
            while !parser.at_eof() && !parser.matches(0, recovery) {
                name_entry(parser, recovery.add(Kind::NameKw));
            }
            parser.expect_recover(Kind::RBrace, recovery);
            parser.expect_semi();
            parser.finish_node();
        }
    }

    let entry_recovery = recovery
        .union(PARAM_KEYWORDS)
        .union(TokenSet::new(&[Kind::RBrace, Kind::Semi]));

    parser.start_node(Kind::CvParametersKw);
    assert!(parser.eat(Kind::CvParametersKw));
    parser.expect_recover(Kind::LBrace, entry_recovery);
    while !parser.at_eof() && !parser.matches(0, recovery.add(Kind::RBrace)) {
        entry(parser, entry_recovery);
    }
    parser.expect_recover(Kind::RBrace, entry_recovery);
    parser.expect_semi();
    parser.finish_node();
}
