//! Generate static arrays with data from the Adobe Glyph Lists

use std::{env, fs::File, io::BufWriter, path::Path, str::FromStr};

const OUT_FILE: &str = "agl_codegen.rs";

fn main() {
    println!("cargo::rerun-if-changed=data");

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join(OUT_FILE);
    let mut file = BufWriter::new(File::create(path).unwrap());
    write_legacy_agl_array(&mut file);
    write_agl_arrays(&mut file);
}

fn write_legacy_agl_array(out: &mut impl std::io::Write) {
    let entries = parse_entries("data/glyphlist.txt");
    writeln!(out, "static LEGACY_AGL: &[(&str, &[char])] = &[").unwrap();
    for entry in entries {
        let stringify = entry
            .chr
            .iter()
            .map(|c| format!("'{}'", char::from_u32(*c).unwrap().escape_default()))
            .collect::<Vec<_>>()
            .join(",");
        writeln!(out, "(\"{}\", &[{stringify}]),", entry.name).unwrap();
    }
    writeln!(out, "];").unwrap()
}

fn write_agl_arrays(out: &mut impl std::io::Write) {
    let entries = parse_entries("data/aglfn.txt");
    let formatted = entries
        .iter()
        .map(|NameEntry { chr, name }| format!("(\"{name}\", {})", chr.first().unwrap()))
        .collect::<Vec<_>>()
        .join(",\n");
    writeln!(out, "static AGLFN: &[(&str, u32)] = &[\n{formatted}];\n",).unwrap();
}

fn parse_entries(path: &str) -> Vec<NameEntry> {
    let raw_source = std::fs::read_to_string(path).unwrap();
    raw_source
        .lines()
        .filter(|l| !l.starts_with('#'))
        .map(NameEntry::from_str)
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}

struct NameEntry {
    chr: Vec<u32>,
    name: String,
}

impl FromStr for NameEntry {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(';');
        match (split.next(), split.next(), split.next(), split.next()) {
            (Some(cpoint), Some(postscript_name), Some(_unic_name), None) => {
                let chr = u32::from_str_radix(cpoint, 16).unwrap();
                let postscript_name = postscript_name.to_string();
                Ok(NameEntry {
                    chr: vec![chr],
                    name: postscript_name,
                })
            }
            (Some(name), Some(cpoints), None, None) => {
                let chrs = cpoints
                    .split_whitespace()
                    .map(|s| u32::from_str_radix(s, 16).unwrap())
                    .collect();
                Ok(NameEntry {
                    chr: chrs,
                    name: name.into(),
                })
            }
            _ => Err(s.to_string()),
        }
    }
}
