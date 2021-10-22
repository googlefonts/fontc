//! Utilities for debugging compilation results

use std::fmt::{self, Write};

use fonttools::{
    font::Font,
    layout::{
        common::{LanguageSystem, GPOSGSUB},
        valuerecord::ValueRecord,
    },
    tables::{self, GPOS::Positioning, GSUB::Substitution},
    tag, types,
};

struct IndentWriter<'a> {
    writer: &'a mut dyn Write,
    needs_indent: bool,
    indent: usize,
}

impl IndentWriter<'_> {
    fn write_indent(&mut self) -> fmt::Result {
        let indent = if self.needs_indent {
            let indent_n = super::SPACES.len().min(self.indent * 2);
            &super::SPACES[..indent_n]
        } else {
            ""
        };
        self.writer.write_str(indent)
    }
}

impl<'a> Write for IndentWriter<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.write_indent()?;
        self.needs_indent = s.ends_with('\n');
        self.writer.write_str(s)
    }
}

fn print_with_indent(f: impl FnOnce(&mut IndentWriter) -> fmt::Result) {
    let mut buffer = String::new();
    let mut writer = IndentWriter {
        writer: &mut buffer,
        needs_indent: true,
        indent: 0,
    };

    f(&mut writer).expect("fmt shouldn't fail");
    println!("{}", buffer);
}

/// Print a debug representation of a font to stdout.
pub fn explode_font(font: &Font, verbose: bool) {
    print_with_indent(|f| explode_font_impl(f, font, verbose))
}

/// Print a debug representation of a gpos table to stdout.
pub fn explode_gpos(table: &tables::GPOS::GPOS, verbose: bool) {
    print_with_indent(|f| explode_gpos_impl(f, table, verbose))
}

/// Print a debug representation of a gsub table to stdout.
pub fn explode_gsub(table: &tables::GSUB::GSUB, verbose: bool) {
    print_with_indent(|f| explode_gsub_impl(f, table, verbose))
}

fn explode_font_impl(f: &mut IndentWriter, font: &Font, verbose: bool) -> fmt::Result {
    print_font_name(f, font, verbose)?;

    let gsub = font.tables.GSUB().expect("failed to load GSUB");
    if let Some(gsub) = gsub {
        explode_gsub_impl(f, &gsub, verbose)?;
    }

    let gpos = font.tables.GPOS().expect("failed to load GPOS");
    if let Some(gpos) = gpos {
        explode_gpos_impl(f, &gpos, verbose)?;
    }
    Ok(())
}

fn print_font_name(f: &mut IndentWriter, font: &Font, _verbose: bool) -> fmt::Result {
    let name = font.tables.name().expect("failed to load name");
    let font_name = name.as_ref().and_then(|name| {
        name.records
            .iter()
            .find(|rec| rec.nameID == u16::from(tables::name::NameRecordID::PostscriptName))
            .map(|rec| rec.string.as_str())
    });
    writeln!(f, "exploding '{}'", font_name.unwrap_or("unknown font"))
}

fn explode_gsub_impl(
    f: &mut IndentWriter,
    table: &tables::GSUB::GSUB,
    verbose: bool,
) -> fmt::Result {
    writeln!(f, "<GSUB>")?;
    f.indent += 1;
    explode_script_list(f, table)?;
    explode_feature_list(f, table)?;
    explode_gsub_lookup_list(f, table, verbose)?;
    f.indent -= 1;
    Ok(())
}

fn explode_gpos_impl(
    f: &mut IndentWriter,
    table: &tables::GPOS::GPOS,
    verbose: bool,
) -> fmt::Result {
    writeln!(f, "<GPOS>")?;
    f.indent += 1;
    explode_script_list(f, table)?;
    explode_feature_list(f, table)?;
    explode_gpos_lookup_list(f, table, verbose)?;
    f.indent -= 1;
    Ok(())
}

fn explode_script_list<T>(f: &mut IndentWriter, table: &GPOSGSUB<T>) -> fmt::Result {
    for (tag, script) in &table.scripts.scripts {
        writeln!(f, "script {}", tag)?;
        if let Some(dflt) = &script.default_language_system {
            print_language_system(f, tag!("dflt"), dflt, table)?;
        }
        for (tag, sys) in &script.language_systems {
            print_language_system(f, *tag, sys, table)?;
        }
    }
    Ok(())
}

fn print_language_system<T>(
    f: &mut IndentWriter,
    tag: types::Tag,
    sys: &LanguageSystem,
    table: &GPOSGSUB<T>,
) -> fmt::Result {
    let features = sys
        .feature_indices
        .iter()
        .map(|idx| format!("{}({})", idx, table.features[*idx].0))
        .collect::<Vec<_>>();
    let features_str = features.join(" ");
    f.indent += 1;
    if let Some(required) = sys.required_feature {
        writeln!(
            f,
            "{}: required({}), features [{}]",
            tag, table.features[required].0, features_str
        )?
    } else {
        writeln!(f, "{}: features [{}]", tag, features_str)?;
    }
    f.indent -= 1;
    Ok(())
}

fn explode_feature_list<T>(f: &mut IndentWriter, table: &GPOSGSUB<T>) -> fmt::Result {
    writeln!(f, "features ({}):", table.features.len())?;
    f.indent += 1;
    for (tag, lookups, _) in &table.features {
        writeln!(f, "{} {:?}", tag, lookups.as_slice())?;
    }
    f.indent -= 1;
    Ok(())
}

fn explode_gsub_lookup_list(
    f: &mut IndentWriter,
    table: &tables::GSUB::GSUB,
    verbose: bool,
) -> fmt::Result {
    writeln!(f, "lookups ({}):", table.lookups.len())?;
    f.indent += 1;
    for (idx, lookup) in table.lookups.iter().enumerate() {
        writeln!(
            f,
            "#{} {} flags {:X}",
            idx,
            sub_rule_name(&lookup.rule),
            lookup.flags
        )?;
        f.indent += 1;
        match &lookup.rule {
            Substitution::Single(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} subs", i, item.mapping.len())?;
                    if !verbose {
                        continue;
                    }
                    f.indent += 1;
                    for (replace, by) in item.mapping.iter() {
                        writeln!(f, "{} -> {}", replace, by)?;
                    }
                    f.indent -= 1;
                }
            }
            Substitution::Multiple(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} subs", i, item.mapping.len())?;
                    if !verbose {
                        continue;
                    }
                    f.indent += 1;
                    for (replace, by) in item.mapping.iter() {
                        write!(f, "{} ->", replace)?;
                        for item in by {
                            write!(f, " {}", item)?;
                        }
                        writeln!(f)?;
                    }
                    f.indent -= 1;
                }
            }
            Substitution::Alternate(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} subs", i, item.mapping.len())?;
                    if !verbose {
                        continue;
                    }
                    f.indent += 1;
                    for (replace, from) in item.mapping.iter() {
                        write!(f, "{} from [", replace)?;
                        for item in from {
                            write!(f, " {}", item)?;
                        }
                        writeln!(f, " ]")?;
                    }
                    f.indent -= 1;
                }
            }
            Substitution::Ligature(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} subs", i, item.mapping.len())?;
                    if !verbose {
                        continue;
                    }
                    f.indent += 1;
                    for (replace, by) in item.mapping.iter() {
                        write!(f, "   ")?;
                        for item in replace {
                            write!(f, " {}", item)?;
                        }
                        writeln!(f, " by {}", by)?;
                    }
                    f.indent -= 1;
                }
            }
            Substitution::Contextual(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} subs", i, item.rules.len())?;
                }
            }
            Substitution::ChainedContextual(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} subs", i, item.rules.len())?;
                }
            }
            _ => writeln!(f, "unknown")?,
        }
        f.indent -= 1;
    }
    f.indent -= 1;
    Ok(())
}

fn explode_gpos_lookup_list(
    f: &mut IndentWriter,
    table: &tables::GPOS::GPOS,
    verbose: bool,
) -> fmt::Result {
    writeln!(f, "lookups ({}):", table.lookups.len())?;
    f.indent += 1;
    for (idx, lookup) in table.lookups.iter().enumerate() {
        writeln!(
            f,
            "lookup #{} {} flags {:X}",
            idx,
            pos_rule_name(&lookup.rule),
            lookup.flags
        )?;
        match &lookup.rule {
            Positioning::Single(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} rules", i, item.mapping.len())?;
                    if !verbose {
                        continue;
                    }
                    f.indent += 1;
                    for (item, pos) in item.mapping.iter() {
                        write!(f, "{} -> ", item)?;
                        write_value_record(f, pos)?;
                        writeln!(f)?;
                    }
                    f.indent -= 1;
                }
            }
            Positioning::Pair(items) => {
                for (i, item) in items.iter().enumerate() {
                    writeln!(f, "subtable {}: {} rules", i, item.mapping.len())?;
                    if !verbose {
                        continue;
                    }
                    f.indent += 1;
                    for (pair, pos) in item.mapping.iter() {
                        write!(f, "{} {} -> ", pair.0, pair.1)?;
                        write_value_record(f, &pos.0)?;
                        write!(f, " ")?;
                        write_value_record(f, &pos.1)?;
                        writeln!(f)?;
                    }
                    f.indent -= 1;
                }
            }
            _ => writeln!(f, "unimplemented")?,
        }
    }
    f.indent -= 1;
    Ok(())
}

fn write_value_record(f: &mut IndentWriter, record: &ValueRecord) -> fmt::Result {
    let x0 = record.xPlacement.unwrap_or_default();
    let y0 = record.yPlacement.unwrap_or_default();
    let x1 = record.xAdvance.unwrap_or_default();
    let y1 = record.yAdvance.unwrap_or_default();
    if x0.abs() + x1.abs() + y0.abs() + y1.abs() == 0 {
        write!(f, "NULL")
    } else if x0.abs() + y0.abs() + y1.abs() == 0 {
        write!(f, "{:+}", x1)
    } else {
        write!(f, "<{} {} {} {}>", x0, y0, x1, y1)
    }
}

fn sub_rule_name(rule: &Substitution) -> &str {
    match rule {
        Substitution::Single(_) => "Single Substitution",
        Substitution::Multiple(_) => "Multiple Substitution",
        Substitution::Alternate(_) => "Alternate Substitution",
        Substitution::Ligature(_) => "Ligature Substitution",
        Substitution::Contextual(_) => "Contextual Substitution",
        Substitution::ChainedContextual(_) => "Chained Contextual Substitution",
        _ => "Unknown",
    }
}

fn pos_rule_name(rule: &Positioning) -> &str {
    match rule {
        Positioning::Single(_) => "Single",
        Positioning::Pair(_) => "Pair",
        Positioning::Cursive(_) => "Cursive",
        Positioning::MarkToBase(_) => "MarkToBase",
        Positioning::MarkToLig => "MarkToLig",
        Positioning::MarkToMark => "MarkToMark",
        Positioning::Contextual(_) => "Contextual",
        Positioning::ChainedContextual(_) => "ChainedContextual",
        Positioning::Extension => "Extension",
    }
}
