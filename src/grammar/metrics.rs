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
        expect_number(parser, Kind::Metric, recovery);
        expect_number(parser, Kind::Metric, recovery);
        if parser.eat(Kind::ContourpointKw) {
            parser.expect_recover(Kind::Number, recovery);
        } else if parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw) {
            if expect_device(parser, recovery) {
                expect_device(parser, recovery);
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

// A: <metric> (-5)
// B: <<metric> <metric> <metric> <metric>> (<1 2 -5 242>)
// return 'true' if we make any progress (this looks like a value record)
pub(crate) fn eat_value_record(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn value_record_body(parser: &mut Parser, recovery: TokenSet) {
        if parser.eat(Kind::Number) {
            return;
        }

        let recovery = recovery.union(TokenSet::new(&[Kind::RAngle]));
        parser.expect_recover(Kind::LAngle, recovery);
        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::RAngle, recovery);
    }

    let looks_like_record = parser.matches(0, Kind::Number)
        || (parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::Number));

    if !looks_like_record {
        return false;
    }

    parser.eat_trivia();
    parser.start_node(Kind::ValueRecordNode);
    value_record_body(parser, recovery);
    parser.finish_node();
    true
}

fn expect_number(parser: &mut Parser, kind: Kind, recovery: TokenSet) {
    parser.expect_remap_recover(Kind::Number, kind, recovery);
}

fn expect_device(parser: &mut Parser, recovery: TokenSet) -> bool {
    debug_assert!(parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw));
    parser.eat_trivia();
    parser.start_node(Kind::DeviceKw);
    parser.expect(Kind::LAngle);
    parser.expect(Kind::DeviceKw);
    expect_number(parser, Kind::Number, recovery);
    expect_number(parser, Kind::Number, recovery);
    if parser.eat(Kind::Comma) {
        expect_number(parser, Kind::Number, recovery);
        expect_number(parser, Kind::Number, recovery);
    }
    // FIXME: this should handle an arbitary number of pairs? but also isn't
    // supported yet?
    // I don't know what's going on tbh
    parser.expect(Kind::RAngle)
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn anchor_a() {
        let fea = "<anchor 120 -30>";
        let out = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START AnchorKw
  <
  AnchorKw
  WS( )
  METRIC(120)
  WS( )
  METRIC(-30)
  >
END AnchorKw
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn anchor_a_octal() {
        let fea = "<anchor 070 -30>";
        let out = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        let errors = out.errors();
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0].message.contains("Expected METRIC"),
            "{}",
            errors[0].message
        );
        crate::assert_eq_str!(
            "\
START AnchorKw
  <
  AnchorKw
  WS( )
  OCT(070)
  WS( )
  METRIC(-30)
  >
END AnchorKw
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn anchor_b() {
        let fea = "<anchor 5 -5 contourpoint 14>";
        let out = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START AnchorKw
  <
  AnchorKw
  WS( )
  METRIC(5)
  WS( )
  METRIC(-5)
  WS( )
  ContourpointKw
  WS( )
  NUM(14)
  >
END AnchorKw
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn value_record_b() {
        let fea = "<-80 0 -160 0>";
        let out = debug_parse_output(fea, |parser| {
            eat_value_record(parser, TokenSet::EMPTY);
        });
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START ValueRecordNode
  <
  NUM(-80)
  WS( )
  NUM(0)
  WS( )
  NUM(-160)
  WS( )
  NUM(0)
  >
END ValueRecordNode
",
            out.simple_parse_tree(fea),
        );
    }
}
