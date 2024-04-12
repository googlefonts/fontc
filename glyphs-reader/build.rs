use std::env;
use std::path::Path;

include!("src/glyphdata/glyphdata_impl.rs");

fn parse_xml_files() -> Result<Vec<GlyphInfo>, GlyphDataError> {
    let mut one = parse_xml_file("data/GlyphData.xml")?;
    let two = parse_xml_file("data/GlyphData_Ideographs.xml")?;
    one.extend(two);
    Ok(one)
}

fn parse_xml_file(path: &str) -> Result<Vec<GlyphInfo>, GlyphDataError> {
    let Ok(bytes) = std::fs::read(path) else {
        panic!("failed to read path '{path}'");
    };
    parse_entries(&bytes)
}

// tell cargo when to rerun this script
fn register_dependencies() {
    println!("cargo::rerun-if-changed=data");
    println!("cargo::rerun-if-changed=src/glyphdata/glyphdata_impl.rs");
}

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("glyphdata.bin");
    let parsed = parse_xml_files().expect("failed to parse GlyphData xml files");
    let bytes = bincode::serialize(&parsed).expect("bincode failed");
    std::fs::write(dest_path, bytes).unwrap();

    register_dependencies()
}
