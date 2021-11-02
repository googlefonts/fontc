use std::ops::Range;

use fonttools::types::Tag;

use crate::parse::{Kind, Parser, TokenSet};

#[allow(non_upper_case_globals)]
mod tags {
    use fonttools::{tag, types::Tag};
    pub const BASE: Tag = tag!("BASE");
    pub const GDEF: Tag = tag!("GDEF");
    pub const STAT: Tag = tag!("STAT");
    pub const head: Tag = tag!("head");
    pub const hhea: Tag = tag!("hhea");
    pub const name: Tag = tag!("name");
    pub const OS2: Tag = tag!("OS/2");
    pub const vhea: Tag = tag!("vhea");
    pub const vmtx: Tag = tag!("vmtx");
}

pub(crate) fn table(parser: &mut Parser) {
    parser.eat_trivia();
    parser.start_node(Kind::TableNode);
    assert!(parser.eat(Kind::TableKw));

    let tag = match parser.eat_tag() {
        Some(tag) => tag,
        None => {
            parser.err("expected tag");
            parser.eat_until(TokenSet::TOP_LEVEL);
            parser.finish_node();
            return;
        }
    };

    match tag.tag {
        tags::BASE => table_impl(parser, tags::BASE, base::table_entry),
        tags::GDEF => table_impl(parser, tags::GDEF, gdef::table_entry),
        tags::head => table_impl(parser, tags::head, head::table_entry),
        tags::hhea => table_impl(parser, tags::hhea, hhea::table_entry),
        tags::name => table_impl(parser, tags::name, name::table_entry),
        tags::OS2 => table_impl(parser, tags::OS2, os2::table_entry),
        tags::vhea => table_impl(parser, tags::vhea, vhea::table_entry),
        tags::vmtx => table_impl(parser, tags::vmtx, vmtx::table_entry),
        tags::STAT => table_impl(parser, tags::STAT, stat::table_entry),
        _ => unknown_table(parser, tag.range),
    }

    let table_kind = table_kind_for_tag(tag.tag);
    parser.finish_and_remap_node(table_kind);
}

// build any table, given a function that parses items from that table.
fn table_impl(parser: &mut Parser, tag: Tag, table_fn: impl Fn(&mut Parser, TokenSet)) {
    parser.expect_recover(Kind::LBrace, TokenSet::TOP_SEMI);
    while !parser.at_eof() && !parser.matches(0, TokenSet::TOP_LEVEL.union(Kind::RBrace.into())) {
        table_fn(parser, TokenSet::TOP_LEVEL);
    }

    parser.expect_recover(Kind::RBrace, TokenSet::TOP_SEMI);
    if let Some(close) = parser.expect_tag(TokenSet::TOP_SEMI) {
        if close.tag != tag {
            parser.raw_error(close.range, format!("expected tag '{}'", tag));
        }
    }

    parser.expect_semi();
}

// if we don't recognize a table tag we still want to try parsing.
fn unknown_table(parser: &mut Parser, open_tag: Range<usize>) {
    loop {
        match parser.nth(0).kind {
            Kind::RBrace if parser.nth_raw(1) == parser.raw_range(open_tag.clone()) => {
                assert!(parser.eat(Kind::RBrace) && parser.eat(Kind::Ident));
                parser.expect_semi();
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

fn table_node(parser: &mut Parser, f: impl FnOnce(&mut Parser)) {
    parser.eat_trivia();
    parser.start_node(Kind::TableEntryNode);
    f(parser);
    parser.finish_node();
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

        if parser.matches(0, TAG_LIST) {
            parser.in_node(Kind::BaseTagListNode, |parser| {
                assert!(parser.eat(TAG_LIST));
                parser.expect_tag(recovery.union(Kind::Semi.into()));
                while parser.eat_tag().is_some() {
                    continue;
                }
                parser.expect_semi();
            });
        } else if parser.matches(0, SCRIPT_LIST) {
            parser.in_node(Kind::BaseScriptListNode, |parser| {
                assert!(parser.eat(SCRIPT_LIST));
                expect_script_record(parser, recovery);
                while parser.eat(Kind::Comma) {
                    expect_script_record(parser, recovery);
                }
                parser.expect_semi();
            })
        } else if parser.matches(0, MINMAX) {
            parser.in_node(Kind::BaseMinMaxNode, |parser| {
                parser.eat_until(EAT_UNTIL);
            });
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

mod gdef {
    use super::super::glyph;
    use super::*;

    const GDEF_KEYWORDS: TokenSet = TokenSet::new(&[
        Kind::GlyphClassDefKw,
        Kind::AttachKw,
        Kind::LigatureCaretByDevKw,
        Kind::LigatureCaretByIndexKw,
        Kind::LigatureCaretByPosKw,
    ]);

    const CARET_POS_OR_IDX: TokenSet =
        TokenSet::new(&[Kind::LigatureCaretByPosKw, Kind::LigatureCaretByIndexKw]);

    const CLASS_RECOVERY: TokenSet =
        TokenSet::new(&[Kind::Comma, Kind::NamedGlyphClass, Kind::LSquare]);

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let eat_until = recovery.union(GDEF_KEYWORDS).add(Kind::RBrace);
        let recovery = recovery.union(GDEF_KEYWORDS).add(Kind::Semi);

        if parser.matches(0, Kind::GlyphClassDefKw) {
            parser.in_node(Kind::GdefClassDefNode, |parser| {
                assert!(parser.eat(Kind::GlyphClassDefKw));
                let recovery = recovery.union(CLASS_RECOVERY);
                for i in 0..=3 {
                    parser.in_node(Kind::GdefClassDefEntryNode, |parser| {
                        glyph::eat_named_or_unnamed_glyph_class(parser, recovery);
                        if i != 3 {
                            parser.expect_recover(Kind::Comma, recovery);
                        }
                    })
                }
                parser.expect_semi();
            })
        } else if parser.matches(0, Kind::AttachKw) {
            parser.in_node(Kind::GdefAttachNode, |parser| {
                assert!(parser.eat(Kind::AttachKw));
                glyph::expect_glyph_or_glyph_class(parser, recovery);
                if parser.expect_recover(Kind::Number, recovery) {
                    parser.eat_while(Kind::Number);
                }
                parser.expect_semi();
            })
            // unimplemented (in spec)
        } else if parser.matches(0, Kind::LigatureCaretByDevKw) {
            table_node(parser, |parser| parser.eat_until(eat_until))
        } else if parser.matches(0, CARET_POS_OR_IDX) {
            parser.in_node(Kind::GdefLigatureCaretNode, |parser| {
                assert!(parser.eat(CARET_POS_OR_IDX));
                glyph::expect_glyph_or_glyph_class(parser, recovery);
                if parser.expect_recover(Kind::Number, recovery) {
                    parser.eat_while(Kind::Number);
                }
                parser.expect_semi();
            })
        } else {
            // any unrecognized token
            parser.expect_recover(GDEF_KEYWORDS, eat_until);
            parser.eat_until(eat_until);
        }
    }
}

mod head {
    use super::*;

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        if parser.matches(0, Kind::FontRevisionKw) {
            parser.in_node(Kind::HeadFontRevisionNode, |parser| {
                assert!(parser.eat(Kind::FontRevisionKw));
                parser.expect_recover(
                    Kind::Float,
                    recovery.union(TokenSet::new(&[Kind::Semi, Kind::RBrace])),
                );
                parser.expect_recover(Kind::Semi, recovery.union(Kind::RBrace.into()));
            })
        } else {
            parser.expect_recover(
                Kind::FontRevisionKw,
                recovery.union(TokenSet::new(&[Kind::Semi, Kind::RBrace])),
            );
            parser.eat_until(recovery.union(TokenSet::new(&[Kind::FontRevisionKw, Kind::RBrace])));
        }
    }
}

mod hhea {
    use super::*;

    const HHEA_KEYWORDS: TokenSet = TokenSet::new(&[
        Kind::CaretOffsetKw,
        Kind::AscenderKw,
        Kind::DescenderKw,
        Kind::LineGapKw,
    ]);

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(HHEA_KEYWORDS);
        if parser.matches(0, HHEA_KEYWORDS) {
            parser.in_node(Kind::MetricValueNode, |parser| {
                assert!(parser.eat(HHEA_KEYWORDS));
                parser.expect_remap_recover(
                    Kind::Number,
                    Kind::Metric,
                    recovery.union(TokenSet::new(&[Kind::Semi, Kind::RBrace])),
                );
                parser.expect_recover(Kind::Semi, recovery.union(Kind::RBrace.into()));
            })
        } else {
            parser.expect_recover(
                HHEA_KEYWORDS,
                recovery.union(TokenSet::new(&[Kind::Semi, Kind::RBrace])),
            );
            parser.eat_until(recovery.union(Kind::RBrace.into()));
        }
    }
}

mod name {
    use super::super::metrics;
    use super::*;

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(TokenSet::new(&[Kind::NameIdKw, Kind::RBrace]));
        if parser.matches(0, Kind::NameIdKw) {
            parser.in_node(Kind::NameRecordNode, |parser| {
                assert!(parser.eat(Kind::NameIdKw));
                parser.expect_recover(TokenSet::NUM_TYPES, recovery.union(Kind::Semi.into()));
                metrics::expect_name_record(parser, recovery);
                parser.expect_semi();
            })
        } else {
            parser.expect_recover(Kind::NameIdKw, recovery.union(Kind::Semi.into()));
            parser.eat_until(recovery);
        }
    }
}

mod os2 {
    use super::*;

    const METRICS: TokenSet = TokenSet::new(&[
        Kind::TypoAscenderKw,
        Kind::TypoDescenderKw,
        Kind::TypoLineGapKw,
        Kind::WinAscentKw,
        Kind::WinDescentKw,
        Kind::XHeightKw,
        Kind::CapHeightKw,
    ]);

    const NUM_LISTS: TokenSet =
        TokenSet::new(&[Kind::PanoseKw, Kind::UnicodeRangeKw, Kind::CodePageRangeKw]);

    const OS2_KEYWORDS: TokenSet = METRICS
        .union(NUM_LISTS)
        .union(TokenSet::new(&[Kind::VendorKw]));

    // these are not keywords per the spec, so we have to handle them specially??
    static RAW_KEYWORDS: &[&[u8]] = &[
        //b"FamilyClass",
        b"FSType",
        b"LowerOpSize",
        b"WeightClass",
        b"WidthClass",
        b"UpperOpSize",
    ];

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(OS2_KEYWORDS);
        let recovery_semi = recovery.union(Kind::Semi.into());

        if parser.matches(0, METRICS) {
            parser.in_node(Kind::MetricValueNode, |parser| {
                assert!(parser.eat(METRICS));
                parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery_semi);
                parser.expect_semi();
            })
        } else if parser.matches(0, NUM_LISTS) {
            parser.in_node(Kind::Os2NumberListNode, |parser| {
                assert!(parser.eat(NUM_LISTS));
                parser.eat_while(Kind::Number);
                parser.expect_semi();
            })
        } else if parser.matches(0, Kind::VendorKw) {
            parser.in_node(Kind::Os2VendorNode, |parser| {
                assert!(parser.eat(Kind::VendorKw));
                parser.expect_recover(Kind::String, recovery_semi);
                parser.expect_semi();
            })
        } else if RAW_KEYWORDS.contains(&parser.nth_raw(0)) {
            parser.in_node(Kind::NumberValueNode, |parser| {
                parser.eat_raw();
                parser.expect_recover(Kind::Number, recovery_semi);
                parser.expect_semi();
            })
        } else if parser.nth_raw(0) == b"FamilyClass" {
            parser.in_node(Kind::Os2FamilyClassNode, |parser| {
                parser.eat_raw();
                parser.expect_recover(TokenSet::NUM_TYPES, recovery.union(Kind::Semi.into()));
                parser.expect_semi();
            })
        } else {
            parser.err_recover("Expected OS/2 table keyword", recovery_semi);
            parser.eat_until(recovery);
        }
    }
}

mod vhea {
    use super::*;

    const VHEA_KEYWORDS: TokenSet = TokenSet::new(&[
        Kind::VertTypoAscenderKw,
        Kind::VertTypoDescenderKw,
        Kind::VertTypoLineGapKw,
    ]);

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(VHEA_KEYWORDS).union(Kind::RBrace.into());
        let recovery_semi = recovery.union(Kind::Semi.into());
        if parser.matches(0, VHEA_KEYWORDS) {
            parser.in_node(Kind::MetricValueNode, |parser| {
                assert!(parser.eat(VHEA_KEYWORDS));
                parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery_semi);
                parser.expect_semi();
            })
        } else {
            parser.expect_recover(VHEA_KEYWORDS, recovery_semi);
            parser.eat_until(recovery);
        }
    }
}

mod vmtx {
    use super::super::glyph;
    use super::*;

    const VMTX_KEYWORDS: TokenSet = TokenSet::new(&[Kind::VertOriginYKw, Kind::VertAdvanceYKw]);

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(VMTX_KEYWORDS).union(Kind::RBrace.into());
        let recovery_semi = recovery.union(Kind::Semi.into());
        if parser.matches(0, Kind::NameIdKw) {
            table_node(parser, |parser| {
                assert!(parser.eat(VMTX_KEYWORDS));
                glyph::expect_glyph_name_like(parser, recovery_semi.union(Kind::Number.into()));
                parser.expect_recover(Kind::Number, recovery_semi);
                parser.expect_semi();
            })
        } else {
            parser.expect_recover(VMTX_KEYWORDS, recovery_semi);
            parser.eat_until(recovery);
        }
    }
}

mod stat {
    use super::*;
    const STAT_TOPLEVEL: TokenSet = TokenSet::new(&[
        Kind::ElidedFallbackNameKw,
        Kind::ElidedFallbackNameIDKw,
        Kind::DesignAxisKw,
        Kind::AxisValueKw,
    ]);

    pub(crate) fn table_entry(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(STAT_TOPLEVEL);
        if parser.matches(0, Kind::ElidedFallbackNameKw) {
            parser.in_node(Kind::StatElidedFallbackNameNode, |parser| {
                assert!(parser.eat(Kind::ElidedFallbackNameKw));
                parser.expect_recover(Kind::LBrace, recovery);
                super::super::greedy(eat_name_entry)(parser, recovery);
                parser.expect_recover(Kind::RBrace, recovery);
                parser.expect_semi();
            })
        } else if parser.matches(0, Kind::ElidedFallbackNameIDKw) {
            parser.in_node(Kind::StatElidedFallbackNameNode, |parser| {
                assert!(parser.eat(Kind::ElidedFallbackNameIDKw));
                parser.expect_recover(Kind::Number, recovery.union(Kind::Semi.into()));
                parser.expect_semi();
            })
        } else if parser.matches(0, Kind::DesignAxisKw) {
            parser.in_node(Kind::StatDesignAxisNode, |parser| {
                assert!(parser.eat(Kind::DesignAxisKw));
                parser.expect_tag(recovery);
                parser.expect_recover(Kind::Number, recovery);
                parser.expect_recover(Kind::LBrace, recovery);
                super::super::greedy(eat_name_entry)(parser, recovery);
                parser.expect_recover(Kind::RBrace, recovery);
                parser.expect_semi();
            })
        } else if parser.matches(0, Kind::AxisValueKw) {
            parser.in_node(Kind::StatAxisValueNode, |parser| {
                assert!(parser.eat(Kind::AxisValueKw));
                parser.expect_recover(Kind::LBrace, recovery);
                super::super::greedy(axis_value_field)(parser, recovery);
                parser.expect_recover(Kind::RBrace, recovery);
                parser.expect_semi();
            })
        } else {
            parser.err_recover("not valid in STAT table", recovery);
            parser.eat_until(recovery);
        }
    }

    fn axis_value_field(parser: &mut Parser, recovery: TokenSet) -> bool {
        eat_name_entry(parser, recovery)
            || eat_location(parser, recovery)
            || eat_flags(parser, recovery)
    }

    fn eat_flags(parser: &mut Parser, _recovery: TokenSet) -> bool {
        const FLAGS: TokenSet = TokenSet::new(&[
            Kind::ElidableAxisValueNameKw,
            Kind::OlderSiblingFontAttributeKw,
        ]);
        if parser.matches(0, Kind::FlagKw) {
            parser.in_node(Kind::StatAxisValueFlagNode, |parser| {
                assert!(parser.eat(Kind::FlagKw));
                parser.eat(FLAGS);
                parser.eat(FLAGS);
                parser.expect_semi();
            });
            return true;
        }
        false
    }

    fn eat_location(parser: &mut Parser, recovery: TokenSet) -> bool {
        const NUM_OR_FLOAT: TokenSet = TokenSet::new(&[Kind::Number, Kind::Float]);
        if parser.matches(0, Kind::LocationKw) {
            parser.in_node(Kind::StatAxisValueLocationNode, |parser| {
                assert!(parser.eat(Kind::LocationKw));
                parser.expect_tag(recovery);
                parser.expect_recover(NUM_OR_FLOAT, recovery);
                parser.eat(NUM_OR_FLOAT);
                parser.eat(NUM_OR_FLOAT);
                parser.expect_semi();
            });
            return true;
        }
        false
    }

    fn eat_name_entry(parser: &mut Parser, recovery: TokenSet) -> bool {
        if parser.eat(Kind::NameKw) {
            super::super::metrics::expect_name_record(parser, recovery);
            parser.expect_semi();
            return true;
        }
        false
    }
}

fn table_kind_for_tag(tag: Tag) -> Kind {
    match tag {
        tags::head => Kind::HeadTableNode,
        tags::hhea => Kind::HheaTableNode,
        tags::BASE => Kind::BaseTableNode,
        tags::GDEF => Kind::GdefTableNode,
        tags::name => Kind::NameTableNode,
        tags::OS2 => Kind::Os2TableNode,
        tags::vhea => Kind::VheaTableNode,
        tags::vmtx => Kind::VmtxTableNode,
        tags::STAT => Kind::StatTableNode,
        _ => Kind::TableNode,
    }
}
