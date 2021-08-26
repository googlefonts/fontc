use crate::parse::Parser;
use crate::token::Kind;
use crate::token_set::TokenSet;

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

        const RANGLE: TokenSet = TokenSet::new(&[Kind::RAngle]);
        let recovery = recovery.union(RANGLE);
        // now either:
        // <metric> metric>
        // <metric> <metric> <contour point>
        // <metric> <metric> <device> <device>
        if expect_number(parser, Kind::Metric, recovery) {
            expect_number(parser, Kind::Metric, recovery);
        }
        // either done or contour | device
        if parser.eat(Kind::RAngle) {
            return true;
        }
        if parser.eat(Kind::ContourpointKw) {
            parser.expect_remap_recover(Kind::DecimalLike, Kind::Number, recovery);
        } else if parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw) {
            if expect_device(parser, recovery) {
                expect_device(parser, recovery);
            }
        } else {
            // something else unexpected in our anchor?
            assert!(!parser.matches(0, Kind::RAngle));
            parser.err("Invalid anchor element.");
            if !parser.matches(0, recovery) {
                parser.eat_raw();
            }
        }
        parser.expect_recover(Kind::RAngle, recovery)
    }

    parser.eat_trivia();
    parser.start_node(Kind::AnchorKw);
    let r = anchor_body(parser, recovery);
    parser.finish_node();
    r
}

// A <number> is a signed decimal integer (without leading zeros)
// returns true if we advance.
fn expect_number(parser: &mut Parser, kind: Kind, recovery: TokenSet) -> bool {
    fn leading_zeros(bytes: &[u8]) -> bool {
        match bytes {
            [_b] => false, // one leading zero is just a 0
            [b'0', ..] => true,
            _ => false,
        }
    }

    let has_neg = parser.matches(0, Kind::Hyphen);
    let num_pos = if has_neg { 1 } else { 0 };
    if parser.matches(num_pos, Kind::DecimalLike) {
        if leading_zeros(parser.nth_raw(num_pos)) {
            if has_neg {
                assert!(parser.eat(Kind::Hyphen));
            }
            parser.err_and_bump("Number cannot have leading zeros.");
        } else if has_neg {
            parser.eat_remap2(kind);
        } else {
            parser.eat_remap(Kind::DecimalLike, kind);
        }
        true
    } else {
        if has_neg {
            parser.eat_raw();
        }
        // always fails, but generates our error message
        parser.expect_recover(kind, recovery);
        // if we ate a '-' we can still advance
        has_neg
    }
}

fn expect_device(parser: &mut Parser, recovery: TokenSet) -> bool {
    debug_assert!(parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw));
    parser.eat_trivia();
    parser.start_node(Kind::DeviceKw);
    parser.expect(Kind::LAngle);
    parser.expect(Kind::DeviceKw);
    if expect_number(parser, Kind::Number, recovery)
        && expect_number(parser, Kind::Number, recovery)
        && parser.eat(Kind::Comma)
        && expect_number(parser, Kind::Number, recovery)
    {
        expect_number(parser, Kind::Number, recovery);
    }
    //}
    // FIXME: this should handle an arbitary number of pairs? but also isn't
    // supported yet?
    // I don't know what's going on tbh
    parser.expect(Kind::RAngle)
}
