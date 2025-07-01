use super::super::lexer::{Kind as LexemeKind, TokenSet};
use super::{glyph, gpos, gsub, metrics};

use crate::parse::Parser;
use crate::token_tree::Kind;

const LABEL_RECOVERY: TokenSet = TokenSet::new(&[LexemeKind::UseExtensionKw, LexemeKind::LBrace]);

pub(crate) fn feature(parser: &mut Parser) {
    fn feature_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::FeatureKw));
        let open_tag = parser.expect_tag(LABEL_RECOVERY);

        parser.eat(Kind::UseExtensionKw);
        parser.expect(Kind::LBrace);
        while !parser.at_eof() && !parser.matches(0, Kind::RBrace) {
            if !statement(parser, TokenSet::FEATURE_STATEMENT, false) {
                if let Some(tag) = open_tag.as_ref() {
                    parser.raw_error(tag.range.clone(), "Feature block is unclosed");
                }
                break;
            }
        }
        parser.expect_recover(Kind::RBrace, TokenSet::TOP_SEMI);
        let close_tag = parser.expect_tag(TokenSet::TOP_LEVEL);
        if let (Some(open), Some(close)) = (open_tag, close_tag) {
            if open.tag != close.tag {
                parser.raw_error(close.range, format!("expected tag '{}'", open.tag));
            }
        }
        parser.expect_semi();
    }

    parser.in_node(Kind::FeatureNode, feature_body);
}

pub(crate) fn lookup_block(parser: &mut Parser, recovery: TokenSet) {
    fn lookup_body(parser: &mut Parser, recovery: TokenSet) {
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
            if !statement(parser, recovery, true) {
                if let Some(range) = raw_label_range {
                    parser.raw_error(range, "Table is unclosed");
                }
                break;
            }
        }
        parser.expect_recover(
            Kind::RBrace,
            recovery.union(TokenSet::IDENT_LIKE.union(TokenSet::SEMI)),
        );
        parser.expect_remap_recover(
            TokenSet::IDENT_LIKE,
            Kind::Label,
            recovery.union(TokenSet::SEMI),
        );
        parser.expect_semi();
    }

    parser.in_node(Kind::LookupBlockNode, |parser| {
        lookup_body(parser, recovery)
    });
}

/// returns true if we advanced the parser.
pub(crate) fn statement(parser: &mut Parser, recovery: TokenSet, in_lookup: bool) -> bool {
    let start_pos = parser.nth_range(0).start;
    match parser.nth(0).kind.to_token_kind() {
        Kind::PosKw | Kind::SubKw | Kind::RsubKw | Kind::IgnoreKw | Kind::EnumKw => {
            pos_or_sub_rule(parser, recovery)
        }
        Kind::NamedGlyphClass => {
            glyph::named_glyph_class_decl(parser, TokenSet::TOP_LEVEL.union(recovery))
        }
        Kind::MarkClassKw => super::mark_class(parser),
        Kind::SubtableKw => parser.in_node(Kind::SubtableNode, |parser| {
            parser.eat_raw();
            parser.expect_recover(Kind::Semi, recovery);
        }),
        Kind::LookupKw if in_lookup => {
            parser.err_and_bump("lookups cannot be nested.");
            parser.eat_until(recovery);
        }
        Kind::IncludeKw => super::include(parser),
        Kind::LookupKw => super::lookup_block_or_reference(parser, recovery),
        Kind::LookupflagKw => lookupflag(parser, recovery),
        Kind::ScriptKw => {
            super::eat_script(parser, recovery);
        }
        Kind::LanguageKw => {
            super::eat_language(parser, recovery);
        }
        Kind::FeatureKw => {
            // aalt only
            if parser.matches(1, TokenSet::TAG_LIKE) && parser.matches(2, Kind::Semi) {
                parser.in_node(Kind::AaltFeatureNode, |parser| {
                    assert!(parser.eat(Kind::FeatureKw));
                    parser.expect_tag(TokenSet::EMPTY);
                    parser.expect_recover(Kind::Semi, recovery);
                });
            }
        }
        Kind::ParametersKw => metrics::parameters(parser, recovery),
        Kind::SizemenunameKw => parser.in_node(Kind::SizeMenuNameNode, |parser| {
            assert!(parser.eat(Kind::SizemenunameKw));
            metrics::expect_name_record(parser, recovery);
            parser.expect_recover(Kind::Semi, recovery);
        }),
        Kind::CvParametersKw if in_lookup => {
            parser.err_and_bump("'cvParameters' invalid in lookup block");
            parser.eat_until(recovery);
        }
        Kind::CvParametersKw => cv_parameters(parser, recovery),
        Kind::FeatureNamesKw => feature_names(parser, recovery),
        Kind::Semi => {
            parser.warn("';' should only follow a statement");
            parser.eat_raw();
        }

        _ => {
            let token = parser.current_token_text();
            let scope = if in_lookup { "lookup" } else { "feature" };
            parser.err(format!("'{token}' Not valid in a {scope} block"));
            parser.eat_until(TokenSet::TOP_AND_FEATURE.add(LexemeKind::RBrace));
        }
    }
    parser.nth_range(0).start != start_pos
}

pub(crate) fn pos_or_sub_rule(parser: &mut Parser, recovery: TokenSet) {
    match parser.nth(0).kind.to_token_kind() {
        Kind::PosKw => gpos::gpos_rule(parser, recovery),
        Kind::EnumKw if parser.nth(1).kind.to_token_kind() == Kind::PosKw => {
            gpos::gpos_rule(parser, recovery)
        }
        Kind::EnumKw => parser.err_and_bump("'enum' keyword must be followed by position rule"),
        Kind::IgnoreKw => match parser.nth(1).kind.to_token_kind() {
            Kind::PosKw => gpos::gpos_rule(parser, recovery),
            Kind::SubKw => gsub::gsub_rule(parser, recovery),
            _ => parser
                .err_and_bump("'ignore' keyword must be followed by position or substitution rule"),
        },
        Kind::SubKw | Kind::RsubKw => gsub::gsub_rule(parser, recovery),
        other => panic!("'{other}' is not a valid gpos or gsub token"),
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
    let name_recovery = recovery.union(TokenSet::new(&[
        LexemeKind::NameKw,
        LexemeKind::RBrace,
        LexemeKind::Semi,
    ]));

    parser.in_node(Kind::FeatureNamesKw, |parser| {
        assert!(parser.eat(Kind::FeatureNamesKw));
        parser.expect_recover(Kind::LBrace, name_recovery);
        while !parser.at_eof() && !parser.matches(0, recovery.add(LexemeKind::RBrace)) {
            name_entry(parser, name_recovery);
        }
        parser.expect_recover(Kind::RBrace, name_recovery);
        parser.expect_semi();
    })
}

fn cv_parameters(parser: &mut Parser, recovery: TokenSet) {
    const UNICODE_VALUE: TokenSet = TokenSet::new(&[LexemeKind::Number, LexemeKind::Hex]);
    const PARAM_KEYWORDS: TokenSet = TokenSet::new(&[
        LexemeKind::FeatUiLabelNameIdKw,
        LexemeKind::FeatUiTooltipTextNameIdKw,
        LexemeKind::SampleTextNameIdKw,
        LexemeKind::ParamUiLabelNameIdKw,
    ]);

    fn entry(parser: &mut Parser, recovery: TokenSet) {
        if parser.matches(0, Kind::CharacterKw) {
            parser.in_node(Kind::CharacterKw, |parser| {
                assert!(parser.eat(Kind::CharacterKw));
                parser.expect_recover(UNICODE_VALUE, recovery);
                parser.expect_semi();
            })
        } else if parser.matches(0, PARAM_KEYWORDS) {
            parser.in_node(Kind::CvParamsNameNode, |parser| {
                assert!(parser.eat(PARAM_KEYWORDS));
                parser.expect_recover(Kind::LBrace, recovery.add(LexemeKind::NameKw));
                while !parser.at_eof() && !parser.matches(0, recovery) {
                    name_entry(parser, recovery.add(LexemeKind::NameKw));
                }
                parser.expect_recover(Kind::RBrace, recovery);
                parser.expect_semi();
            });
        } else {
            parser.err_and_bump("token not valid in cvParameters block");
            parser.eat_until(recovery);
            parser.eat(Kind::Semi);
        }
    }

    let entry_recovery = recovery.union(PARAM_KEYWORDS).union(TokenSet::new(&[
        LexemeKind::RBrace,
        LexemeKind::CharacterKw,
        LexemeKind::Semi,
    ]));

    parser.in_node(Kind::CvParametersKw, |parser| {
        assert!(parser.eat(Kind::CvParametersKw));
        parser.expect_recover(Kind::LBrace, entry_recovery);
        while !parser.at_eof() && !parser.matches(0, recovery.add(LexemeKind::RBrace)) {
            entry(parser, entry_recovery);
        }
        parser.expect_recover(Kind::RBrace, entry_recovery);
        parser.expect_semi();
    });
}

fn lookupflag(parser: &mut Parser, recovery: TokenSet) {
    fn eat_named_lookup_value(parser: &mut Parser, recovery: TokenSet) -> bool {
        match parser.nth(0).kind.to_token_kind() {
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

    parser.in_node(Kind::LookupFlagNode, |parser| {
        lookupflag_body(parser, recovery);
    });
}
