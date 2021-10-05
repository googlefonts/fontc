use fonttools::{
    font::Font,
    layout::{
        common::{LanguageSystem, GPOSGSUB},
        valuerecord::ValueRecord,
    },
    tables::{self, GPOS::Positioning, GSUB::Substitution},
    tag, types,
};

fn main() {
    let args = flags::ValidateNames::from_env().unwrap();

    let font = Font::load(&args.path).expect("failed to load font");
    let gsub = font.tables.GSUB().expect("failed to load gsub");
    if let Some(gsub) = gsub {
        explode_gsub(&gsub, args.verbose);
    }

    let gpos = font.tables.GPOS().expect("failed to load gpos");
    if let Some(gpos) = gpos {
        explode_gpos(&gpos, args.verbose);
    }
}

fn explode_gsub(table: &tables::GSUB::GSUB, verbose: bool) {
    println!("<GSUB>");
    explode_script_list(&table);
    explode_feature_list(&table);
    explode_gsub_lookup_list(&table, verbose);
}

fn explode_gpos(table: &tables::GPOS::GPOS, verbose: bool) {
    println!("<GPOS>");
    explode_script_list(&table);
    explode_feature_list(&table);
    explode_gpos_lookup_list(&table, verbose);
}

fn explode_script_list<T>(table: &GPOSGSUB<T>) {
    for (tag, script) in &table.scripts.scripts {
        println!("script {}", tag);
        if let Some(dflt) = &script.default_language_system {
            print_language_system(tag!("dflt"), &dflt, table);
        }
        for (tag, sys) in &script.language_systems {
            print_language_system(*tag, sys, table);
        }
    }
}

fn print_language_system<T>(tag: types::Tag, sys: &LanguageSystem, table: &GPOSGSUB<T>) {
    let features = sys
        .feature_indices
        .iter()
        .map(|idx| format!("{}({})", idx, table.features[*idx].0))
        .collect::<Vec<_>>();
    let features_str = features.join(" ");
    if let Some(required) = sys.required_feature {
        println!(
            "  {}: required({}), features [{}]",
            tag, table.features[required].0, features_str
        )
    } else {
        println!("  {}: features [{}]", tag, features_str);
    }
}

fn explode_feature_list<T>(table: &GPOSGSUB<T>) {
    println!("features ({}):", table.features.len());
    for (tag, lookups, _) in &table.features {
        println!("  {} {:?}", tag, lookups.as_slice());
    }
}

fn explode_gsub_lookup_list(table: &tables::GSUB::GSUB, verbose: bool) {
    println!("lookups ({}):", table.lookups.len());
    for (idx, lookup) in table.lookups.iter().enumerate() {
        println!(
            "  #{} {} flags {:X}",
            idx,
            sub_rule_name(&lookup.rule),
            lookup.flags
        );
        match &lookup.rule {
            Substitution::Single(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} subs", i, item.mapping.len());
                    if !verbose {
                        continue;
                    }
                    for (replace, by) in item.mapping.iter() {
                        println!("    {} -> {}", replace, by);
                    }
                }
            }
            Substitution::Multiple(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} subs", i, item.mapping.len());
                    if !verbose {
                        continue;
                    }
                    for (replace, by) in item.mapping.iter() {
                        print!("    {} ->", replace);
                        for item in by {
                            print!(" {}", item);
                        }
                        print!("\n")
                    }
                }
            }
            Substitution::Alternate(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} subs", i, item.mapping.len());
                    if !verbose {
                        continue;
                    }
                    for (replace, from) in item.mapping.iter() {
                        print!("    {} from [", replace);
                        for item in from {
                            print!(" {}", item);
                        }
                        print!(" ]\n")
                    }
                }
            }
            Substitution::Ligature(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} subs", i, item.mapping.len());
                    if !verbose {
                        continue;
                    }
                    for (replace, by) in item.mapping.iter() {
                        print!("   ");
                        for item in replace {
                            print!(" {}", item);
                        }
                        println!(" by {}", by)
                    }
                }
            }
            Substitution::Contextual(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} subs", i, item.rules.len());
                }
            }
            Substitution::ChainedContextual(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} subs", i, item.rules.len());
                }
            }
            _ => println!(" unknown"),
        }
    }
}
fn explode_gpos_lookup_list(table: &tables::GPOS::GPOS, verbose: bool) {
    println!("lookups ({}):", table.lookups.len());
    for (idx, lookup) in table.lookups.iter().enumerate() {
        println!(
            "  #{} {} flags {:X}",
            idx,
            pos_rule_name(&lookup.rule),
            lookup.flags
        );
        match &lookup.rule {
            Positioning::Single(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} rules", i, item.mapping.len());
                    if !verbose {
                        continue;
                    }
                    for (item, pos) in item.mapping.iter() {
                        print!("    {} -> ", item);
                        print_value_record(pos);
                        println!("");
                    }
                }
            }
            Positioning::Pair(items) => {
                for (i, item) in items.iter().enumerate() {
                    println!("    #{}: {} rules", i, item.mapping.len());
                    if !verbose {
                        continue;
                    }
                    for (pair, pos) in item.mapping.iter() {
                        print!("    {} {} -> ", pair.0, pair.1);
                        print_value_record(&pos.0);
                        print!(" ");
                        print_value_record(&pos.1);
                        println!("");
                    }
                }
            }
            _ => println!("    unimplemented"),
        }
    }
}

fn print_value_record(record: &ValueRecord) {
    let x0 = record.xPlacement.unwrap_or_default();
    let y0 = record.yPlacement.unwrap_or_default();
    let x1 = record.xAdvance.unwrap_or_default();
    let y1 = record.yAdvance.unwrap_or_default();
    if x0.abs() + x1.abs() + y0.abs() + y1.abs() == 0 {
        print!("NULL")
    } else if x0.abs() + y0.abs() + y1.abs() == 0 {
        print!("{:+}", x1);
    } else {
        print!("<{} {} {} {}>", x0, y0, x1, y1);
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

mod flags {
    use std::path::PathBuf;
    xflags::xflags! {

        cmd validate-names
            required path: PathBuf
            {
                optional -v, --verbose
            }
    }
}
