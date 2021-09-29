use crate::{token_tree::typed, types::gsub, Kind};

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
            let alternates = match stmt.alternates() {
                typed::GlyphClass::Literal(lit) => {
                    let mut bad = false;
                    for item in lit.iter() {
                        if item.kind() == Kind::NamedGlyphClass {
                            bad = true;
                            ctx.error(
                                item.range(),
                                "glyph classes are not allowed in alternates".into(),
                            );
                        }
                    }
                    if bad {
                        return None;
                    }
                    ctx.resolve_glyph_class_literal(&lit)
                }
                typed::GlyphClass::Named(name) => ctx.resolve_named_glyph_class(&name)?,
            };
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
