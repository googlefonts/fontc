//! Run the compiler against a bunch of inputs, comparing them with
//! the results of fonttools.
//!
//! set FEA_TEST_VERBOSE=1 in the environment for verbose output.

use std::{
    collections::HashMap,
    convert::TryInto,
    env::temp_dir,
    ffi::OsStr,
    fmt::Debug,
    fs,
    path::{Path, PathBuf},
    process::Command,
    time::SystemTime,
};

use crate::{AstSink, Compilation, Diagnostic, GlyphIdent, GlyphMap, GlyphName, Node, Parser};
use fonttools::{font::Font, tables};

static TEST_DATA1: &str = "./test-data/fonttools-tests";
//static TEST_DATA2: &str = "./test-data/other-parse-tests";

static ALLOWED_PARSE_FAILURES: &[&str] = &[
    "GSUB_error.fea",     // expected failure
    "bug509.fea",         // see https://github.com/adobe-type-tools/afdko/issues/1415
    "GSUB_5_formats.fea", // ditto
    //FIXME these should be working!
    "GSUB_8.fea",                  // we just don't handle rsub yet?
    "MultipleLookupsPerGlyph.fea", // multiple lookups per glyph, eh?
];

/// A way to customize output when our test fails
#[derive(Default)]
struct Results {
    seen: usize,
    failures: Vec<Failure>,
}

struct Failure {
    path: PathBuf,
    reason: Reason,
}

enum Reason {
    Panic,
    ParseFail(Vec<Diagnostic>),
    CompileFail(Vec<Diagnostic>),
    TtxFail { code: Option<i32>, std_err: String },
    CompareFail { expected: String, result: String },
}

#[test]
fn all_compile_tests() -> Result<(), Results> {
    let glyph_map = make_glyph_map();
    let reverse_map = glyph_map.reverse_map();
    let reverse_map = reverse_map
        .into_iter()
        .map(|(id, glyph)| {
            (
                format!("glyph{:05}", id.to_raw()),
                match glyph {
                    GlyphIdent::Cid(num) => format!("cid{}", num),
                    GlyphIdent::Name(name) => name.to_string(),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    let mut results = Vec::new();

    for path in iter_compile_tests(TEST_DATA1) {
        let err = match std::panic::catch_unwind(|| match try_parse_file(&path, &glyph_map) {
            Err(errs) => Err(Failure {
                path: path.clone(),
                reason: Reason::ParseFail(errs),
            }),
            Ok(node) => match crate::compile(&node, &glyph_map) {
                Err(errs) => Err(Failure {
                    path: path.clone(),
                    reason: Reason::CompileFail(errs),
                }),
                Ok(result) => Ok((make_font(result, &glyph_map), path.clone())),
            },
        }) {
            Err(_) => Err(Failure {
                path,
                reason: Reason::Panic,
            }),
            Ok(thing) => thing,
        };
        results.push(err);
    }

    let result = results
        .into_iter()
        .map(|result| match result {
            Err(e) => Err(e),
            Ok((font, path)) => compare_ttx(font, path, &reverse_map),
        })
        .fold(Results::default(), |mut results, current| {
            results.seen += 1;
            if let Err(err) = current {
                results.failures.push(err);
            }
            results
        });

    if result.failures.is_empty() {
        Ok(())
    } else {
        Err(result)
    }
}

fn compare_ttx(
    mut font: Font,
    fea_path: PathBuf,
    reverse_map: &HashMap<String, String>,
) -> Result<(), Failure> {
    let ttx_path = fea_path.with_extension("ttx");
    assert!(ttx_path.exists());
    let temp_path = temp_dir()
        .join(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
                .to_string(),
        )
        .with_extension("ttf");
    font.save(&temp_path).unwrap();

    const TO_WRITE: &[&str] = &[
        "head", "name", "BASE", "GDEF", "GSUB", "GPOS", "OS/2", "STAT", "hhea", "vhea",
    ];

    let mut cmd = Command::new("ttx");
    for table in TO_WRITE.into_iter() {
        cmd.arg("-t").arg(table);
    }
    let status = cmd.arg(&temp_path).output().expect(&format!(
        "failed to execute for path {}",
        fea_path.display()
    ));
    if !status.status.success() {
        let std_err = String::from_utf8_lossy(&status.stderr).into_owned();
        return Err(Failure {
            path: fea_path.into(),
            reason: Reason::TtxFail {
                code: status.status.code(),
                std_err,
            },
        });
    }

    let ttx_out_path = temp_path.with_extension("ttx");
    assert!(ttx_out_path.exists());

    let expected = std::fs::read_to_string(ttx_path).unwrap();
    let result = std::fs::read_to_string(ttx_out_path).unwrap();
    let result = rewrite_ttx(&result, reverse_map);
    if expected != result {
        Err(Failure {
            path: fea_path.into(),
            reason: Reason::CompareFail { expected, result },
        })
    } else {
        Ok(())
    }
}

// hacky way to make our ttx output match fonttools'
fn rewrite_ttx(input: &str, reverse_map: &HashMap<String, String>) -> String {
    let mut out = String::with_capacity(input.len());
    let mut scan = input;

    let header = b"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<ttFont sfntVersion=\"\\x00\\x01\\x00\\x00\" ttLibVersion=\"4.26\">";
    if input.as_bytes().starts_with(header) {
        out.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<ttFont>");
        scan = &scan[header.len()..];
    }
    loop {
        let next = scan.find("glyph").unwrap_or_else(|| scan.len());
        out.push_str(&scan[..next]);
        scan = &scan[next..];
        if scan.is_empty() {
            break;
        }
        if scan.len() >= 10 {
            if let Some(replacement) = reverse_map.get(&scan[..10]) {
                out.push_str(&replacement);
                scan = &scan[10..];
                continue;
            }
        }
        out.push_str(&scan[..5]);
        scan = &scan[5..];
    }
    out
}

fn iter_compile_tests(path: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    let mut dir = path.as_ref().read_dir().unwrap();
    std::iter::from_fn(move || loop {
        let entry = dir.next()?.unwrap();
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) && path.with_extension("ttx").exists() {
            return Some(path);
        }
    })
}

/// returns the tree and any errors
fn try_parse_file(path: &Path, map: &GlyphMap) -> Result<Node, Vec<Diagnostic>> {
    let contents = fs::read_to_string(path).expect("file read failed");
    let mut sink = AstSink::new(&contents, Some(map));
    let mut parser = Parser::new(&contents, &mut sink);
    crate::root(&mut parser);
    let (root, errors) = sink.finish();
    if errors.iter().any(Diagnostic::is_error) {
        Err(errors)
    } else {
        Ok(root)
    }
}

impl std::fmt::Debug for Results {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "failed test cases")?;
        let widest = self
            .failures
            .iter()
            .map(|item| item.path.as_os_str().len())
            .max()
            .unwrap_or(0);
        for failure in &self.failures {
            writeln!(
                f,
                "{:width$} {:?}",
                failure.path.display(),
                failure.reason,
                width = widest
            )?;
        }
        writeln!(f, "failed {}/{} test cases", self.failures.len(), self.seen)?;
        Ok(())
    }
}

impl Debug for Reason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Panic => write!(f, "panic"),
            Self::ParseFail(_arg0) => write!(f, "parse failure"),
            Self::CompileFail(_arg0) => write!(f, "compile failure"),
            Self::TtxFail { code, std_err } => {
                write!(f, "ttx failure ({:?}) stderr:\n{}", code, std_err)
            }
            Self::CompareFail { expected, result } => {
                if std::env::var("FEA_TEST_VERBOSE").is_ok() {
                    writeln!(f, "compare failure")?;
                    crate::util::write_line_diff(f, &expected, &result)
                } else {
                    write!(f, "compare failure")
                }
            }
        }
    }
}

fn make_font(compilation: Compilation, glyphs: &GlyphMap) -> Font {
    let mut font = Font::new(fonttools::font::SfntVersion::TrueType);
    if let Some(gsub) = compilation.gsub {
        font.tables.insert(gsub);
    }
    if let Some(gpos) = compilation.gpos {
        font.tables.insert(gpos);
    }

    let maxp = tables::maxp::maxp::new05(glyphs.len().try_into().unwrap());
    font.tables.insert(maxp);
    font
}

fn make_glyph_map() -> GlyphMap {
    TEST_FONT_GLYPHS
        .iter()
        .map(|name| GlyphIdent::Name(GlyphName::new(*name)))
        //.chain((800_u16..=1001).into_iter().map(GlyphIdent::Cid))
        .collect()
}

#[rustfmt::skip]
static TEST_FONT_GLYPHS: &[&str] = &[
".notdef", "space", "slash", "fraction", "semicolon", "period", "comma",
"ampersand", "quotedblleft", "quotedblright", "quoteleft", "quoteright",
"zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
"nine", "zero.oldstyle", "one.oldstyle", "two.oldstyle",
"three.oldstyle", "four.oldstyle", "five.oldstyle", "six.oldstyle",
"seven.oldstyle", "eight.oldstyle", "nine.oldstyle", "onequarter",
"onehalf", "threequarters", "onesuperior", "twosuperior",
"threesuperior", "ordfeminine", "ordmasculine", "A", "B", "C", "D", "E",
"F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
"T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g",
"h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u",
"v", "w", "x", "y", "z", "A.sc", "B.sc", "C.sc", "D.sc", "E.sc", "F.sc",
"G.sc", "H.sc", "I.sc", "J.sc", "K.sc", "L.sc", "M.sc", "N.sc", "O.sc",
"P.sc", "Q.sc", "R.sc", "S.sc", "T.sc", "U.sc", "V.sc", "W.sc", "X.sc",
"Y.sc", "Z.sc", "A.alt1", "A.alt2", "A.alt3", "B.alt1", "B.alt2",
"B.alt3", "C.alt1", "C.alt2", "C.alt3", "a.alt1", "a.alt2", "a.alt3",
"a.end", "b.alt", "c.mid", "d.alt", "d.mid", "e.begin", "e.mid",
"e.end", "m.begin", "n.end", "s.end", "z.end", "Eng", "Eng.alt1",
"Eng.alt2", "Eng.alt3", "A.swash", "B.swash", "C.swash", "D.swash",
"E.swash", "F.swash", "G.swash", "H.swash", "I.swash", "J.swash",
"K.swash", "L.swash", "M.swash", "N.swash", "O.swash", "P.swash",
"Q.swash", "R.swash", "S.swash", "T.swash", "U.swash", "V.swash",
"W.swash", "X.swash", "Y.swash", "Z.swash", "f_l", "c_h", "c_k", "c_s",
"c_t", "f_f", "f_f_i", "f_f_l", "f_i", "o_f_f_i", "s_t", "f_i.begin",
"a_n_d", "T_h", "T_h.swash", "germandbls", "ydieresis", "yacute",
"breve", "grave", "acute", "dieresis", "macron", "circumflex",
"cedilla", "umlaut", "ogonek", "caron", "damma", "hamza", "sukun",
"kasratan", "lam_meem_jeem", "noon.final", "noon.initial", "by",
"feature", "lookup", "sub", "table", "uni0327", "uni0328", "e.fina",
];
