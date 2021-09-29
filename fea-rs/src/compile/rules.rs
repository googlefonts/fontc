use crate::{
    token_tree::typed,
    types::{gpos, gsub, ValueRecord},
};

use super::ValidationCtx;

pub(crate) fn resolve_gsub_statement(
    ctx: &mut ValidationCtx,
    statement: typed::GsubStatement,
) -> Option<gsub::Rule> {
    match statement {
        typed::GsubStatement::Type1(stmt) => {
            let target = ctx.resolve_glyph_or_class(&stmt.target())?;
            let replacement = ctx.resolve_glyph_or_class(&stmt.replacement())?;
            Some(gsub::Rule::Type1(gsub::Single {
                target,
                replacement,
            }))
        }
        typed::GsubStatement::Type2(stmt) => {
            let target = ctx.resolve_glyph(&stmt.target())?;
            let replacement = stmt
                .replacement()
                .filter_map(|glyph| ctx.resolve_glyph(&glyph))
                .collect();
            Some(gsub::Rule::Type2(gsub::Multiple {
                target,
                replacement,
            }))
        }
        typed::GsubStatement::Type3(stmt) => {
            let target = ctx.resolve_glyph(&stmt.target())?;
            let alternates = ctx.resolve_glyph_class(&stmt.alternates())?;
            Some(gsub::Rule::Type3(gsub::Alternate { target, alternates }))
        }
        typed::GsubStatement::Type4(stmt) => {
            let target = stmt
                .target()
                .flat_map(|item| ctx.resolve_glyph_or_class(&item))
                .collect();
            let replacement = ctx.resolve_glyph(&stmt.replacement())?;
            Some(gsub::Rule::Type4(gsub::Ligature {
                target,
                replacement,
            }))
        }
        _ => None,
    }
}

pub(crate) fn resolve_gpos_statement(
    ctx: &mut ValidationCtx,
    statement: typed::GposStatement,
) -> Option<gpos::Rule> {
    match statement {
        typed::GposStatement::Type1(stmt) => {
            let target = ctx.resolve_glyph_or_class(&stmt.target())?;
            let value = resolve_value_record(&stmt.value());
            Some(gpos::Rule::Type1(gpos::Single { target, value }))
        }
        typed::GposStatement::Type2(stmt) => {
            let first_item = ctx.resolve_glyph_or_class(&stmt.first_item())?;
            let first_value = resolve_value_record(&stmt.first_value());
            let second_item = ctx.resolve_glyph_or_class(&stmt.second_item())?;
            let second_value = stmt
                .second_value()
                .as_ref()
                .map(resolve_value_record)
                .unwrap_or(ValueRecord::Null);

            Some(gpos::Rule::Type2(gpos::Pair {
                first: gpos::Single {
                    target: first_item,
                    value: first_value,
                },
                second: gpos::Single {
                    target: second_item,
                    value: second_value,
                },
            }))
        }
        _ => None,
    }
}

fn resolve_value_record(raw: &typed::ValueRecord) -> ValueRecord {
    if let Some(_) = raw.null() {
        ValueRecord::Null
    } else if let Some(advance) = raw.advance() {
        ValueRecord::Advance(advance.parse())
    } else if let Some(named) = raw.named() {
        ValueRecord::Named(named.text.clone())
    } else if let Some([x, y, x_adv, y_adv]) = raw.placement() {
        ValueRecord::Placement {
            x_placement: x.parse(),
            y_placement: y.parse(),
            x_advance: x_adv.parse(),
            y_advance: y_adv.parse(),
        }
    } else {
        unreachable!("already validated")
    }
}
