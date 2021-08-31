use super::{glyph, gpos, gsub, metrics};
use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

pub(crate) fn feature(parser: &mut Parser) {
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
