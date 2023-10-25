//! syntax for variation syntax.
//!
//! See <https://github.com/adobe-type-tools/afdko/pull/1350>

use crate::parse::{
    lexer::{Kind, TokenSet},
    Parser,
};
use crate::token_tree::Kind as AstKind;

pub(crate) fn condition_set(parser: &mut Parser) {
    // parse a single condition. returns true if we advance the cursor
    // a condition looks like:
    // <tag> <min> <max>;, e.g.
    // wght 100 300;
    fn condition(parser: &mut Parser) -> bool {
        if !parser.matches(0, TokenSet::TAG_LIKE) {
            return false;
        }
        parser.in_node(AstKind::ConditionNode, |parser| {
            parser.expect_tag(TokenSet::TOP_SEMI.add(Kind::Number));
            parser.repeat(|p| p.expect_recover(Kind::Number, TokenSet::TOP_SEMI), 2);
            parser.expect_semi();
        });

        true
    }

    fn condition_set_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::ConditionSetKw));
        // saved for error reporting
        let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
        parser.expect_remap_recover(
            TokenSet::IDENT_LIKE,
            AstKind::Label,
            TokenSet::TOP_LEVEL.add(Kind::LBrace),
        );
        parser.expect(Kind::LBrace);
        while !parser.at_eof() && !parser.matches(0, Kind::RBrace) {
            if !condition(parser) {
                if let Some(range) = raw_label_range {
                    parser.raw_error(range, "Table is unclosed");
                }
                break;
            }
        }
        parser.expect_recover(
            Kind::RBrace,
            TokenSet::TOP_LEVEL.union(TokenSet::IDENT_LIKE.union(TokenSet::SEMI)),
        );
        parser.expect_remap_recover(
            TokenSet::IDENT_LIKE,
            AstKind::Label,
            TokenSet::TOP_LEVEL.union(TokenSet::SEMI),
        );

        parser.expect_semi();
    }

    parser.in_node(AstKind::ConditionSetNode, condition_set_body);
}

pub(crate) fn variation(parser: &mut Parser) {
    fn variation_body(parser: &mut Parser) {
        assert!(parser.eat(Kind::VariationKw));
        let open_tag = parser.expect_tag(TokenSet::TOP_LEVEL.add(Kind::Ident).add(Kind::LBrace));
        if !parser.eat(Kind::NullKw) && !parser.eat_remap(TokenSet::IDENT_LIKE, AstKind::Label) {
            parser.err_recover(
                "expected label or NULL",
                TokenSet::TOP_LEVEL.add(Kind::LBrace),
            );
        }

        parser.expect(Kind::LBrace);

        while !parser.at_eof() && !parser.matches(0, Kind::RBrace) {
            if !super::feature::statement(parser, TokenSet::FEATURE_STATEMENT, false) {
                if let Some(tag) = open_tag.as_ref() {
                    parser.raw_error(tag.range.clone(), "Variation block is unclosed");
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

    parser.in_node(AstKind::VariationNode, variation_body);
}
