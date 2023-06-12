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
            parser.eat_raw();
            parser.expect_recover(Kind::Number, TokenSet::TOP_SEMI);
            parser.expect_recover(Kind::Number, TokenSet::TOP_SEMI);
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
