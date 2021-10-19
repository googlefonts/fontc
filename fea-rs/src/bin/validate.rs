//! Attempt to validate input, reporting errors.

use std::{collections::HashMap, path::Path};

use fea_rs::{typed::AstNode as _, AstSink, Diagnostic, GlyphMap, GlyphName, Level, Node, Parser};

/// Attempt to parse fea files.
///
/// usage: PATH [-v]
///
/// PATH may be a feature file or a ufo.
fn main() {
    let args = flags::ValidateNames::from_env().unwrap();
    let (names, features) = load_font(&args.path);
    let (root, mut errors) = try_parse_fea(&features, &names);
    print_statement_info(&root);

    let val_errors = fea_rs::validate(&root, &names);
    errors.extend(val_errors);

    if !errors.is_empty() {
        let tokens = root
            .iter_tokens()
            .map(|t| (t.kind, t.range()))
            .collect::<Vec<_>>();
        errors.sort_unstable_by_key(|err| (err.span().start, err.span().end));
        let mut prev_range = 0..usize::MAX;

        errors.retain(|x| {
            let retain = x.span() != prev_range;
            prev_range = x.span();
            retain
        });

        println!(
            "{}",
            fea_rs::util::stringify_errors(&features, &tokens, &errors)
        );

        let err_count = errors
            .iter()
            .filter(|err| err.level == Level::Error)
            .count();
        let warning_count = errors
            .iter()
            .filter(|err| err.level == Level::Warning)
            .count();
        println!("{} errors, {} warnings", err_count, warning_count);
    } else {
        println!("exited successfully!")
    }
}

fn load_font(filename: &Path) -> (GlyphMap, String) {
    if filename.extension().and_then(|ext| ext.to_str()) == Some("ufo") {
        let font = norad::Font::load(filename).expect("failed to load font");
        let features = match font.features.as_ref() {
            Some(features) => features.to_owned(),
            None => panic!("font contains no features"),
        };
        let glyphs: GlyphMap = font
            .default_layer()
            .iter()
            .map(|g| GlyphName::from(g.name.as_ref()))
            .collect();

        (glyphs, features)
    } else if filename.extension().and_then(|ext| ext.to_str()) == Some("fea") {
        let features = std::fs::read_to_string(filename).unwrap();
        let glyphs = DEFAULT_GLYPH_LIST
            .iter()
            .map(|s| GlyphName::from(*s))
            .collect();
        (glyphs, features)
    } else {
        panic!("unknown file type {}", filename.display());
    }
}

/// returns the tree and any errors
fn try_parse_fea(contents: &str, names: &GlyphMap) -> (Node, Vec<Diagnostic>) {
    let mut sink = AstSink::new(contents, Some(names));
    let mut parser = Parser::new(contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

fn print_statement_info(root: &Node) {
    let mut counter = HashMap::new();
    let mut statements = HashMap::new();
    for child in root.iter_children().filter(|t| !t.kind().is_trivia()) {
        *counter.entry(child.kind()).or_insert(0_u32) += 1;
        if let Some(lookup) = fea_rs::typed::LookupBlock::cast(child) {
            statements.insert(lookup.tag().text.clone(), lookup.statements().count());
        } else if let Some(feature) = fea_rs::typed::Feature::cast(child) {
            statements.insert(feature.tag().text().clone(), feature.statements().count());
        }
    }

    println!("## total statement count ##");
    for (kind, count) in counter {
        println!("{}: {}", kind, count);
    }

    println!("\n## feature/lookup statement counts ##");
    let mut statements: Vec<_> = statements.into_iter().collect();
    statements.sort_unstable_by_key(|(_, count)| *count);
    statements.reverse();
    for (kind, count) in statements {
        if count > 10 {
            println!("{:5}: {}", count, kind);
        }
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

static DEFAULT_GLYPH_LIST: &[&str] = &[
    ".notdef",
    "space",
    "slash",
    "fraction",
    "semicolon",
    "period",
    "comma",
    "ampersand",
    "quotedblleft",
    "quotedblright",
    "quoteleft",
    "quoteright",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "zero.oldstyle",
    "one.oldstyle",
    "two.oldstyle",
    "three.oldstyle",
    "four.oldstyle",
    "five.oldstyle",
    "six.oldstyle",
    "seven.oldstyle",
    "eight.oldstyle",
    "nine.oldstyle",
    "onequarter",
    "onehalf",
    "threequarters",
    "onesuperior",
    "twosuperior",
    "threesuperior",
    "ordfeminine",
    "ordmasculine",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A.sc",
    "B.sc",
    "C.sc",
    "D.sc",
    "E.sc",
    "F.sc",
    "G.sc",
    "H.sc",
    "I.sc",
    "J.sc",
    "K.sc",
    "L.sc",
    "M.sc",
    "N.sc",
    "O.sc",
    "P.sc",
    "Q.sc",
    "R.sc",
    "S.sc",
    "T.sc",
    "U.sc",
    "V.sc",
    "W.sc",
    "X.sc",
    "Y.sc",
    "Z.sc",
    "A.alt1",
    "A.alt2",
    "A.alt3",
    "B.alt1",
    "B.alt2",
    "B.alt3",
    "C.alt1",
    "C.alt2",
    "C.alt3",
    "a.alt1",
    "a.alt2",
    "a.alt3",
    "a.end",
    "b.alt",
    "c.mid",
    "d.alt",
    "d.mid",
    "e.begin",
    "e.mid",
    "e.end",
    "m.begin",
    "n.end",
    "s.end",
    "z.end",
    "Eng",
    "Eng.alt1",
    "Eng.alt2",
    "Eng.alt3",
    "A.swash",
    "B.swash",
    "C.swash",
    "D.swash",
    "E.swash",
    "F.swash",
    "G.swash",
    "H.swash",
    "I.swash",
    "J.swash",
    "K.swash",
    "L.swash",
    "M.swash",
    "N.swash",
    "O.swash",
    "P.swash",
    "Q.swash",
    "R.swash",
    "S.swash",
    "T.swash",
    "U.swash",
    "V.swash",
    "W.swash",
    "X.swash",
    "Y.swash",
    "Z.swash",
    "f_l",
    "c_h",
    "c_k",
    "c_s",
    "c_t",
    "f_f",
    "f_f_i",
    "f_f_l",
    "f_i",
    "o_f_f_i",
    "s_t",
    "f_i.begin",
    "a_n_d",
    "T_h",
    "T_h.swash",
    "germandbls",
    "ydieresis",
    "yacute",
    "breve",
    "grave",
    "acute",
    "dieresis",
    "macron",
    "circumflex",
    "cedilla",
    "umlaut",
    "ogonek",
    "caron",
    "damma",
    "hamza",
    "sukun",
    "kasratan",
    "lam_meem_jeem",
    "noon.final",
    "noon.initial",
    "by",
    "feature",
    "lookup",
    "sub",
    "table",
    "uni0327",
    "uni0328",
    "e.fina",
];
