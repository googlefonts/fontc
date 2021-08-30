use std::ops::Range;

use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

pub(crate) fn table(parser: &mut Parser) {
    parser.eat_trivia();
    parser.start_node(Kind::TableKw);
    assert!(parser.eat(Kind::TableKw));
    let raw_label_range = parser.matches(0, Kind::Ident).then(|| parser.nth_range(0));
    match parser.nth_raw(0) {
        b"BASE" => table_impl(parser, b"BASE", base::table_entry),
        _ => {
            parser.expect_recover(Kind::Ident, TokenSet::TOP_LEVEL.union(Kind::LBrace.into()));
            if parser.expect_recover(Kind::LBrace, TokenSet::TOP_LEVEL) {
                if let Some(range) = raw_label_range {
                    unknown_table(parser, range);
                }
            }
        }
    }
}

fn table_impl(parser: &mut Parser, tag: &[u8], table_fn: impl Fn(&mut Parser, TokenSet)) {
    assert_eq!(parser.nth_raw(0), tag);
    parser.eat_raw();
    parser.expect_recover(Kind::LBrace, TokenSet::TOP_SEMI);
    while !parser.at_eof() && !parser.matches(0, TokenSet::TOP_LEVEL.union(Kind::RBrace.into())) {
        table_fn(parser, TokenSet::TOP_LEVEL);
    }
    parser.expect_recover(Kind::RBrace, TokenSet::TOP_SEMI);
    if parser.nth_raw(0) != tag {
        parser.err_and_bump(format!("Expected '{}' tag", String::from_utf8_lossy(tag)));
    } else {
        parser.eat_raw();
    }
    parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
}

// if we don't recognize a table tag we still want to try parsing.
fn unknown_table(parser: &mut Parser, open_tag: Range<usize>) {
    loop {
        match parser.nth(0).kind {
            Kind::RBrace if parser.nth_raw(1) == parser.raw_range(open_tag.clone()) => {
                assert!(parser.eat(Kind::RBrace) && parser.eat(Kind::Ident));
                parser.expect_recover(Kind::Semi, TokenSet::TOP_LEVEL);
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

mod base {
    use super::*;
    const MINMAX: TokenSet = TokenSet::new(&[Kind::HorizAxisMinMaxKw, Kind::VertAxisMinMaxKw]);
    const TAG_LIST: TokenSet =
        TokenSet::new(&[Kind::HorizAxisBaseTagListKw, Kind::VertAxisBaseTagListKw]);
    const SCRIPT_LIST: TokenSet = TokenSet::new(&[
        Kind::HorizAxisBaseScriptListKw,
        Kind::VertAxisBaseScriptListKw,
    ]);
    const BASE_KEYWORDS: TokenSet = MINMAX.union(TAG_LIST).union(SCRIPT_LIST);

    // when we are aborting
    const EAT_UNTIL: TokenSet = TokenSet::TOP_LEVEL
        .union(BASE_KEYWORDS)
        .union(TokenSet::new(&[Kind::RBrace]));

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(BASE_KEYWORDS);

        fn table_node(parser: &mut Parser, f: impl FnOnce(&mut Parser)) {
            parser.eat_trivia();
            parser.start_node(Kind::TableEntryNode);
            f(parser);
            parser.finish_node();
        }

        if parser.matches(0, TAG_LIST) {
            table_node(parser, |parser| {
                assert!(parser.eat(TAG_LIST));
                parser.expect_recover(Kind::Ident, recovery.union(Kind::Semi.into()));
                while parser.eat(Kind::Ident) {
                    continue;
                }
                parser.expect_recover(Kind::Semi, recovery);
            })
        } else if parser.matches(0, SCRIPT_LIST) {
            table_node(parser, |parser| {
                assert!(parser.eat(SCRIPT_LIST));
                expect_script_record(parser, recovery);
                while parser.eat(Kind::Comma) {
                    expect_script_record(parser, recovery);
                }
                parser.expect_recover(Kind::Semi, recovery);
            })
        } else if parser.matches(0, MINMAX) {
            table_node(parser, |parser| {
                // not implemented yet, just eat everything?
                parser.eat_until(EAT_UNTIL);
            })
        } else {
            // any unrecognized token
            parser.expect_recover(BASE_KEYWORDS, EAT_UNTIL);
            parser.eat_until(EAT_UNTIL);
        }
    }

    fn expect_script_record(parser: &mut Parser, recovery: TokenSet) -> bool {
        const RECORD_RECOVERY: TokenSet = TokenSet::new(&[Kind::Tag, Kind::Number, Kind::Comma]);

        fn script_record_body(parser: &mut Parser, recovery: TokenSet) {
            parser.eat_remap(Kind::Ident, Kind::Tag);
            parser.expect_remap_recover(Kind::Ident, Kind::Tag, recovery.union(RECORD_RECOVERY));
            parser.expect_recover(Kind::Number, recovery.union(RECORD_RECOVERY));
            while parser.eat(Kind::Number) {
                continue;
            }
        }

        if !parser.matches(0, Kind::Ident) {
            return false;
        }
        parser.eat_trivia();
        parser.start_node(Kind::ScriptRecordNode);
        script_record_body(parser, recovery);
        parser.finish_node();
        true
    }
}

mod gdef {}
