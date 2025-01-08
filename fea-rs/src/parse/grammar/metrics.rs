use std::ops::Range;

use crate::parse::{
    lexer::{Kind, TokenSet},
    Parser,
};
use crate::token_tree::Kind as AstKind;

// A: <anchor <metric> <metric>> (<anchor 120 -20>)
// B: <anchor <metric> <metric>  # x coordinate, y coordinate
//    <contour point>> (<anchor 120 -20 contourpoint 5>)
// C: <anchor <metric> <metric>   # x coordinate, y coordinate
//    <device> <device>>  # x coordinate device, y coordinate device
//    (<anchor 120 -20 <device 11 1> <device NULL>>)
// D: <anchor NULL>
// E: <anchor <name>> (<anchor TOP_ANCHOR_1>)
pub(crate) fn anchor(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn anchor_body(parser: &mut Parser, recovery: TokenSet) -> bool {
        parser.expect(Kind::LAngle);
        parser.expect(Kind::AnchorKw);
        if parser.eat(Kind::NullKw) || parser.eat(Kind::Ident) {
            return parser.expect_recover(Kind::RAngle, recovery);
        }

        let recovery = recovery.add(Kind::RAngle).add(Kind::ContourpointKw);
        // now either:
        // <metric> metric>
        // <metric> <metric> <contour point>
        // <metric> <metric> <device> <device>
        parser.repeat(|parser| expect_metric(parser, recovery), 2);
        if parser.eat(Kind::ContourpointKw) {
            parser.expect_recover(Kind::Number, recovery);
        } else if eat_device(parser, recovery) {
            expect_device(parser, recovery);
        }
        parser.expect_recover(Kind::RAngle, recovery)
    }

    parser.in_node(AstKind::AnchorNode, |parser| anchor_body(parser, recovery))
}

pub(crate) fn expect_value_record(parser: &mut Parser, recovery: TokenSet) -> bool {
    if eat_value_record(parser, recovery) {
        true
    } else {
        parser.err_recover("expected valuerecord", recovery);
        false
    }
}

// A: <metric> (-5)
// B: <<metric> <metric> <metric> <metric>> (<1 2 -5 242>)
// C: <<metric> <metric> <metric> <metric> <device> <device> <device> <device>>
// (<1 2 -5 242 <device 1 2, 3 4> <device NULL> <device 1 1, 2 2> <device NULL>>)
// return 'true' if we make any progress (this looks like a value record)
pub(crate) fn eat_value_record(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn value_record_body(parser: &mut Parser, recovery: TokenSet) {
        if eat_metric(parser, recovery) {
            return;
        } else if !parser.matches(0, Kind::LAngle) {
            // if we didn't eat a metric but we aren't at a '<' we've errored,
            // so skip remaining tokens
            parser.eat_until(recovery);
            return;
        }

        let recovery = recovery.union(TokenSet::new(&[Kind::RAngle]));
        parser.expect_recover(Kind::LAngle, recovery);
        if parser.eat(Kind::NullKw) || parser.eat_remap(TokenSet::IDENT_LIKE, AstKind::Ident) {
            parser.expect_recover(Kind::RAngle, recovery);
            return;
        }

        parser.repeat(|parser| expect_metric(parser, recovery), 4);
        if parser.eat(Kind::RAngle) {
            return;
        }
        // type C:
        parser.repeat(|parser| expect_device(parser, recovery), 4);
        parser.expect_recover(Kind::RAngle, recovery);
    }

    let looks_like_record = parser.matches(
        0,
        TokenSet::new(&[Kind::Number, Kind::LParen, Kind::Dollar]),
    ) || (parser.matches(0, Kind::LAngle)
        && parser.matches(
            1,
            TokenSet::new(&[Kind::Number, Kind::NullKw, Kind::Dollar]).union(TokenSet::IDENT_LIKE),
        ));

    if !looks_like_record {
        return false;
    }

    parser.eat_trivia();
    parser.start_node(AstKind::ValueRecordNode);
    value_record_body(parser, recovery);
    parser.finish_node();
    true
}

pub(crate) fn expect_metric(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !eat_metric(parser, recovery) {
        parser.err_recover("expected metric (scalar or variable)", recovery);
        return false;
    }
    true
}

/// Eat a metric, which may either be a number, a variable metric, or a glyphsapp 'numbervalue'
///
/// (<https://glyphsapp.com/learn/tokens#g-number-values>)
///
/// A variable metric has the syntax `(<location_value>+)`
/// where `<location_value>` is `<location_spec>:<number>`
/// and a `<location_spec>` is `<axis_tag>=<number>,+`
///
/// a number value has the syntax $ident or ${expr}
fn eat_metric(parser: &mut Parser, recovery: TokenSet) -> bool {
    // a simple numerical metric
    if parser.eat(Kind::Number) {
        return true;
    // else we expect a variable metric; return if we dont' find a paren
    } else if parser.matches(0, Kind::Dollar) {
        return expect_glyphs_number_value(parser, recovery);
    } else if !parser.matches(0, Kind::LParen) {
        return false;
    }

    parser.in_node(AstKind::VariableMetricNode, |parser| {
        parser.eat_raw();
        if parser.in_node(AstKind::LocationValueNode, |parser| {
            expect_variation_location_and_value(parser, recovery.add(Kind::RParen))
        }) {
            while !parser.at_eof() && !parser.matches(0, Kind::RParen) {
                if !parser.in_node(AstKind::LocationValueNode, |parser| {
                    eat_variation_location_and_value(parser, recovery.add(Kind::RParen))
                }) {
                    break;
                }
            }
        }
        parser.expect_recover(Kind::RParen, recovery)
    });
    true
}

// $pad or ${pad * 2 + 5}
pub(crate) fn expect_glyphs_number_value(parser: &mut Parser, recovery: TokenSet) -> bool {
    parser.in_node(AstKind::GlyphsNumberValueNode, |parser| {
        assert!(parser.eat(Kind::Dollar));
        if parser.eat_remap(Kind::Ident, AstKind::GlyphsNumberIdent) {
            return true;
        }
        if parser.matches(0, Kind::LBrace) {
            let recovery = recovery.add(Kind::RBrace).add(Kind::Dollar);
            parser.in_node(AstKind::GlyphsNumberValueExprNode, |parser| {
                parser.eat_raw();
                let looks_okay = eat_expr_body(parser);
                if !looks_okay {
                    parser.eat_until(recovery);
                }
                parser.expect_recover(Kind::RBrace, recovery) && looks_okay
            })
        } else {
            parser.err_recover("expected '$ident' or '${predicate}'", recovery);
            false
        }
    })
}

// generally just eat values and operators; we will validate later
// the one special case is '2-2' which we need to ensure gets parsed as (NUM)(-)(NUM)
// and not (NUM)(NUM)
fn eat_expr_body(parser: &mut Parser) -> bool {
    let mut after_ident_or_number = false;
    loop {
        if parser.matches(0, Kind::RBrace) {
            if !after_ident_or_number {
                parser.err("expected ident or number");
                return false;
            }
            return true;
        }
        // make sure '2-2' becomes 2 - 2
        if after_ident_or_number
            && parser.matches(0, TokenSet::FLOAT_LIKE)
            && parser.current_token_text().starts_with('-')
        {
            let kind = parser.nth(0).kind;
            parser.split_remap_current(TokenSet::FLOAT_LIKE, |text, buf| {
                buf.push((0..1, AstKind::Hyphen));
                buf.push((1..text.len(), kind.to_token_kind()));
            });
            continue;
        }
        if parser.eat(TokenSet::FLOAT_LIKE) {
            if after_ident_or_number {
                parser.err("expected '}' or operator");
                return false;
            }
            after_ident_or_number = true;
            continue;
        }
        if parser.eat(TokenSet::OPERATORS) {
            if !after_ident_or_number {
                parser.err("expected ident or number");
                return false;
            }
            after_ident_or_number = false;
            continue;
        }
        if parser.matches(0, Kind::Ident) {
            if !parser.current_token_text().contains(['-', '/']) {
                if after_ident_or_number {
                    parser.err("expected '}' or operator");
                    return false;
                }
                parser.eat_remap(Kind::Ident, AstKind::GlyphsNumberIdent);
                after_ident_or_number = true;
                continue;
            }
            after_ident_or_number = !parser.current_token_text().ends_with(['-', '/']);
            // if this is an ident and contains '-' or '/' we need to split it up:
            if !parser.split_remap_current(Kind::Ident, split_ident_with_hyphen) {
                return false;
            }
        } else {
            // any other token here is an error
            parser.err("expected value, operator, or '}'");
            return false;
        }
    }
}

fn split_ident_with_hyphen(mut token: &str, buf: &mut Vec<(Range<usize>, AstKind)>) {
    let mut pos = 0;
    while !token.is_empty() {
        if let Some((len, kind)) = take_next_token(token) {
            buf.push((pos..pos + len, kind));
            pos += len;
            token = &token[len..];
        } else {
            // parsing failed; clear the buffer and return
            buf.clear();
            return;
        }
    }
}

fn take_next_token(text: &str) -> Option<(usize, AstKind)> {
    match text.as_bytes()[0] {
        b'-' => Some((1, AstKind::Hyphen)),
        b'/' => Some((1, AstKind::Slash)),
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
            let end = text
                .as_bytes()
                .iter()
                .position(|x| [b'-', b'/'].contains(x))
                .unwrap_or(text.len());
            Some((end, AstKind::GlyphsNumberIdent))
        }
        b'0'..=b'9' => {
            let len = text
                .as_bytes()
                .iter()
                .take_while(|b| b.is_ascii_digit())
                .count();
            if len < text.len() && text.as_bytes()[len] == b'.' {
                let decimal_len = text
                    .as_bytes()
                    .iter()
                    .take_while(|b| b.is_ascii_digit())
                    .count();
                return Some((len + decimal_len + 1, AstKind::Float));
            }
            Some((len, AstKind::Number))
        }
        // we're confused
        _ => None,
    }
}

fn expect_variation_location_and_value(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !eat_variation_location_and_value(parser, recovery) {
        parser.err_recover("expected variation location spec", recovery);
        return false;
    }
    true
}

fn eat_variation_location_and_value(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !parser.matches(0, TokenSet::TAG_LIKE) {
        return false;
    }

    let _ = parser.in_node(AstKind::LocationSpecNode, |parser| {
        eat_location_spec(parser, recovery)
    }) && parser.expect_recover(Kind::Colon, recovery)
        && parser.expect_recover(Kind::Number, recovery);
    true
}

fn eat_location_spec(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !expect_location_item(parser, recovery) {
        return false;
    }
    while parser.eat(Kind::Comma) {
        expect_location_item(parser, recovery);
    }
    true
}

fn expect_location_item(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !eat_location_item(parser, recovery) {
        parser.err_recover("expected <tag>=<number>", recovery);
        return false;
    }
    true
}

fn eat_location_item(parser: &mut Parser, recovery: TokenSet) -> bool {
    if !parser.matches(0, TokenSet::TAG_LIKE) {
        return false;
    }

    parser.in_node(AstKind::LocationSpecItemNode, |parser| {
        parser.eat_tag();
        parser.expect_recover(Kind::Eq, recovery.add(Kind::Comma));
        if !expect_axis_location(parser) {
            parser.err_recover(
                "expected axis location (number or float)",
                recovery.add(Kind::Comma),
            );
        }
    });
    true
}

/// A float or integer, optionally followed by one of the suffixes 'u', 'd' or 'n'
/// NOTE: this is very experimental syntax, based on the discussion at
/// <https://github.com/harfbuzz/boring-expansion-spec/issues/94#issuecomment-1608007111>
// e.g:
// -50
// 10001
// -5u
// 1.0n
// -0.9d
fn expect_axis_location(parser: &mut Parser) -> bool {
    if !parser.matches(0, TokenSet::new(&[Kind::Number, Kind::Float])) {
        return false;
    }
    parser.in_node(AstKind::AxisLocationNode, |parser| {
        assert!(parser.eat(Kind::Number) || parser.eat(Kind::Float));
        parser.eat(Kind::NumberSuffix);
    });
    true
}

fn expect_device(parser: &mut Parser, recovery: TokenSet) -> bool {
    let result = eat_device(parser, recovery);
    if !result {
        parser.err_recover("expected device record", recovery);
    }
    result
}

fn eat_device(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn device_body(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(TokenSet::new(&[Kind::LAngle, Kind::RAngle, Kind::Comma]));
        parser.expect_recover(Kind::LAngle, recovery);
        parser.expect_recover(Kind::DeviceKw, recovery);
        if parser.eat(Kind::NullKw) {
            parser.expect_recover(Kind::RAngle, recovery);
            return;
        }

        parser.repeat(|parser| parser.expect_recover(Kind::Number, recovery), 2);

        while parser.eat(Kind::Comma) {
            // according to the spec <1 2,> should be legal?
            if parser.eat(Kind::RAngle) {
                return;
            }
            parser.repeat(|parser| parser.expect_recover(Kind::Number, recovery), 2);
        }

        parser.expect(Kind::RAngle);
    }

    if parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw) {
        parser.in_node(AstKind::DeviceNode, |parser| device_body(parser, recovery));
        true
    } else {
        false
    }
}

pub(crate) fn parameters(parser: &mut Parser, recovery: TokenSet) {
    let recovery = recovery.union(Kind::Semi.into());

    parser.in_node(AstKind::ParametersNode, |parser| {
        assert!(parser.eat(Kind::ParametersKw));
        parser.expect_recover(TokenSet::FLOAT_LIKE, recovery);
        parser.expect_recover(Kind::Number, recovery);
        if !parser.matches(0, Kind::Semi) {
            parser.repeat(
                |parser| parser.expect_recover(TokenSet::FLOAT_LIKE, recovery),
                2,
            );
        }
        parser.expect_semi();
    })
}

/// used in size feature and name table.
///
/// This doesn't include the leading keyword or ID, which vary.
pub(crate) fn expect_name_record(parser: &mut Parser, recovery: TokenSet) {
    const NUM_TYPES: TokenSet = TokenSet::new(&[Kind::Number, Kind::Octal, Kind::Hex]);
    parser.in_node(AstKind::NameSpecNode, |parser| {
        if parser.eat(NUM_TYPES) && parser.eat(NUM_TYPES) && !parser.eat(NUM_TYPES) {
            parser.err_recover("name record must contain 1 or 3 numbers", recovery);
        }
        parser.expect_recover(Kind::String, recovery.union(Kind::Semi.into()));
    })
}

#[cfg(test)]
mod tests {
    use crate::typed::AstNode;

    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn anchor_a_octal() {
        let fea = "<anchor 070 -30>";
        let (_out, errors, _errstr) = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert_eq!(errors.len(), 1, "{_errstr}");
        assert!(
            errors[0].text().contains("expected metric"),
            "{}",
            errors[0].text()
        );
    }

    #[test]
    fn variable_anchor() {
        let fea = "<anchor 0 (wght=200:-100 wght=900:-150 wght=900,wdth=150:-120)>";
        let (_out, errors, _errstr) = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert!(errors.is_empty());
    }

    #[test]
    fn device_record_smoke_test() {
        let fea = "\
<device NULL>
<device 1 2>
<device 1 2,>
<device 1 2, 3 4, 5 6, 7 8>
";
        let (_out, _, errstr) = debug_parse_output(fea, |parser| {
            expect_device(parser, TokenSet::EMPTY);
        });
        assert!(errstr.is_empty(), "{}", errstr);
    }

    #[test]
    fn wrong_metric_count_in_value_record() {
        let fea = "<1 2>";
        let (_out, _err, errstr) = debug_parse_output(fea, |parser| {
            expect_value_record(parser, TokenSet::EMPTY);
        });
        assert!(errstr.contains("expected metric"));

        let fea = "<1 2 3>";
        let (_out, _err, errstr) = debug_parse_output(fea, |parser| {
            expect_value_record(parser, TokenSet::EMPTY);
        });
        assert!(errstr.contains("expected metric"));
    }

    #[test]
    fn empty_metric() {
        let empty_metric = "()";
        let (_out, _err, errstr) = debug_parse_output(empty_metric, |parser| {
            expect_metric(parser, TokenSet::EMPTY);
        });
        assert!(errstr.contains("expected variation location"), "{errstr}");
    }

    #[test]
    fn empty_location_spec() {
        let empty_metric = "(:10)";
        let (_out, _err, errstr) = debug_parse_output(empty_metric, |parser| {
            expect_metric(parser, TokenSet::EMPTY);
        });
        assert!(errstr.contains("expected variation location"), "{errstr}");
    }

    // https://github.com/googlefonts/fontc/issues/948
    #[test]
    fn glyphs_value_syntax() {
        let fea = "<0 $hi 0 0>";
        let (token, _err, _) = debug_parse_output(fea, |parser| {
            expect_value_record(parser, TokenSet::EMPTY);
        });

        let valrecord = crate::typed::ValueRecord::cast(&token).unwrap();
        assert!(valrecord.advance().is_none());
        assert!(valrecord.placement().is_some());
    }

    #[test]
    fn name_string_omits_quotes() {
        let parse_name = |fea| {
            let (token, _err, _) = debug_parse_output(fea, |parser| {
                expect_name_record(parser, TokenSet::EMPTY);
            });

            crate::typed::NameSpec::cast(&token).unwrap()
        };
        assert_eq!(
            ["", "duck"],
            [
                parse_name("3 1 0x409 \"\"").string(),
                parse_name("3 1 0x409 \"duck\"").string(),
            ]
        );
    }
}
