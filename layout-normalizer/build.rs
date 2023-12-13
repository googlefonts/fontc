//! Build script to generate our glyph name lookup table.

use std::{
    env,
    fs::File,
    io::{BufWriter, Write},
    path::Path,
    str::FromStr,
};

const GLYPH_NAMES_FILE: &str = "glyph_names_codegen.rs";
static AGLFN: &str = include_str!("aglfn.txt");

fn main() {
    println!("cargo:rerun-if-changed=aglfn.txt");
    generate_glyph_names();
}

fn generate_glyph_names() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join(GLYPH_NAMES_FILE);
    let mut file = BufWriter::new(File::create(path).unwrap());

    let mut entries = AGLFN
        .lines()
        .filter(|l| !l.starts_with('#'))
        .map(NameEntry::from_str)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    entries.sort_by(|a, b| a.chr.cmp(&b.chr));
    let formatted = entries
        .iter()
        .map(|NameEntry { chr, name }| format!("({chr}, SmolStr::new_inline(\"{name}\"))"))
        .collect::<Vec<_>>();
    writeln!(
        &mut file,
        "static GLYPH_NAMES: &[(u32, SmolStr)] = &[\n{}];\n",
        formatted.join(",\n")
    )
    .unwrap();
}

struct NameEntry {
    chr: u32,
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
                    chr,
                    name: postscript_name,
                })
            }
            _ => Err(s.to_string()),
        }
    }
}
