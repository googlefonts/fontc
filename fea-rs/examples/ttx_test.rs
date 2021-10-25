//! Run the compiler against a bunch of inputs, comparing them with
//! the results of fonttools.
//!
//! set FEA_TEST_VERBOSE=1 in the environment for verbose output.

use std::{
    collections::HashMap,
    convert::TryInto,
    env::temp_dir,
    ffi::OsStr,
    fmt::{Debug, Write},
    fs,
    path::{Path, PathBuf},
    process::Command,
    time::SystemTime,
};

use ansi_term::Color;
use fea_rs::{AstSink, Compilation, Diagnostic, GlyphIdent, GlyphMap, GlyphName, Node, Parser};
use fonttools::{font::Font, tables};
use rayon::prelude::*;

static TEST_DATA1: &str = "./fea-rs/test-data/fonttools-tests";

fn main() {
    let args = flags::Args::from_env().unwrap();

    let glyph_map = make_glyph_map();
    let reverse_map = glyph_map.reverse_map();
    let reverse_map = reverse_map
        .into_iter()
        .map(|(id, glyph)| {
            (
                format!("glyph{:05}", id.to_raw()),
                match glyph {
                    GlyphIdent::Cid(num) => format!("cid{:05}", num),
                    GlyphIdent::Name(name) => name.to_string(),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    if let Err(err) = run_all_tests(&glyph_map, &reverse_map, args.test.as_ref()) {
        let mut s = String::new();
        err.print_results_impl(&mut s, args.verbose);
        eprintln!("{}", s);
        std::process::exit(1);
    }
}

fn run_all_tests(
    glyph_map: &GlyphMap,
    reverse_map: &HashMap<String, String>,
    filter: Option<&String>,
) -> Result<(), Results> {
    let result = iter_compile_tests(TEST_DATA1, filter)
        .par_bridge()
        .map(|path| run_test(path, &glyph_map, &reverse_map))
        .collect::<Vec<_>>();

    let mut result = result
        .into_iter()
        .fold(Results::default(), |mut results, current| {
            match current {
                Err(e) => results.failures.push(e),
                Ok(path) => results.successes.push(path),
            }
            results
        });
    result.failures.sort_unstable_by(|a, b| {
        (a.reason.sort_order(), &a.path).cmp(&(b.reason.sort_order(), &b.path))
    });

    if result.failures.is_empty() {
        Ok(())
    } else {
        Err(result)
    }
}

fn run_test(
    path: PathBuf,
    glyph_map: &GlyphMap,
    reverse_map: &HashMap<String, String>,
) -> Result<PathBuf, Failure> {
    let compilation = match std::panic::catch_unwind(|| match try_parse_file(&path, &glyph_map) {
        Err(errs) => Err(Failure {
            path: path.clone(),
            reason: Reason::ParseFail(errs),
        }),
        Ok((node, contents)) => match fea_rs::compile(&node, &glyph_map) {
            Err(errs) => Err(Failure {
                path: path.clone(),
                reason: Reason::CompileFail(stringify_diagnostics(&node, &contents, &errs)),
            }),
            Ok(result) => Ok(result),
        },
    }) {
        Err(_) => {
            return Err(Failure {
                path,
                reason: Reason::Panic,
            })
        }
        Ok(Ok(thing)) => thing,
        Ok(Err(e)) => return Err(e),
    };
    let font = make_font(compilation, glyph_map);
    compare_ttx(font, &path, &reverse_map)?;
    Ok(path)
}

/// A way to customize output when our test fails
#[derive(Default)]
struct Results {
    failures: Vec<Failure>,
    successes: Vec<PathBuf>,
}

struct Failure {
    path: PathBuf,
    reason: Reason,
}

#[derive(PartialEq)]
enum Reason {
    Panic,
    ParseFail(String),
    CompileFail(String),
    TtxFail { code: Option<i32>, std_err: String },
    CompareFail { expected: String, result: String },
}

impl Results {
    fn len(&self) -> usize {
        self.failures.len() + self.successes.len()
    }
}

impl Reason {
    fn sort_order(&self) -> u8 {
        match self {
            Self::Panic => 1,
            Self::ParseFail(_) => 2,
            Self::CompileFail(_) => 3,
            Self::TtxFail { .. } => 4,
            Self::CompareFail { .. } => 5,
        }
    }
}

fn compare_ttx(
    mut font: Font,
    fea_path: &Path,
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
    let expected = rewrite_ttx(&expected, reverse_map);
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

    for line in input.lines() {
        if line.starts_with("<ttFont") {
            out.push_str("<ttFont>\n");
            continue;
        }
        let mut scan = line;
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
        out.push('\n');
    }
    out
}

fn iter_compile_tests(
    path: impl AsRef<Path>,
    filter: Option<&String>,
) -> impl Iterator<Item = PathBuf> + '_ {
    let filter_items = filter
        .map(|s| s.split(',').map(|s| s.trim()).collect::<Vec<_>>())
        .unwrap_or_default();
    let mut dir = path.as_ref().read_dir().unwrap();
    std::iter::from_fn(move || loop {
        let entry = dir.next()?.unwrap();
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) && path.with_extension("ttx").exists() {
            if !filter_items.is_empty() {
                if !filter_items
                    .iter()
                    .any(|item| path.to_str().unwrap().contains(item))
                {
                    continue;
                }
            }
            return Some(path);
        }
    })
}

/// returns the tree and any errors
fn try_parse_file(path: &Path, map: &GlyphMap) -> Result<(Node, String), String> {
    let contents = fs::read_to_string(path).expect("file read failed");
    let mut sink = AstSink::new(&contents, Some(map));
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);
    let (root, errors) = sink.finish();
    if errors.iter().any(Diagnostic::is_error) {
        Err(stringify_diagnostics(&root, &contents, &errors))
    } else {
        Ok((root, contents))
    }
}

fn stringify_diagnostics(root: &Node, features: &str, diagnostics: &[Diagnostic]) -> String {
    let tokens = root
        .iter_tokens()
        .map(|t| (t.kind, t.range()))
        .collect::<Vec<_>>();
    fea_rs::util::stringify_errors(&features, &tokens, &diagnostics)
}

impl Results {
    fn print_results_impl(&self, f: &mut impl Write, verbose: bool) -> std::fmt::Result {
        writeln!(f, "failed test cases")?;
        let widest = self
            .failures
            .iter()
            .map(|item| &item.path)
            .chain(self.successes.iter())
            .map(|p| p.file_name().unwrap().to_str().unwrap().len())
            .max()
            .unwrap_or(0)
            + 2;
        for success in &self.successes {
            writeln!(
                f,
                "{:width$} {}",
                success.file_name().unwrap().to_str().unwrap(),
                Color::Green.paint("success"),
                width = widest
            )?;
        }
        for failure in &self.failures {
            let file_name = failure.path.file_name().unwrap().to_str().unwrap();
            write!(f, "{:width$} ", file_name, width = widest)?;
            failure.reason.print_impl(f, verbose)?;
            writeln!(f, "")?;
        }
        writeln!(
            f,
            "failed {}/{} test cases",
            self.failures.len(),
            self.len()
        )?;
        Ok(())
    }
}

impl std::fmt::Debug for Results {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.print_results_impl(f, false)
    }
}

impl Reason {
    fn print_impl(&self, f: &mut impl std::fmt::Write, verbose: bool) -> std::fmt::Result {
        match self {
            Self::Panic => write!(f, "{}", Color::Red.paint("panic")),
            Self::ParseFail(diagnostics) => {
                write!(f, "{}", Color::Purple.paint("parse failure"))?;
                if verbose {
                    write!(f, "\n{}", diagnostics)?;
                }
                Ok(())
            }
            Self::CompileFail(diagnostics) => {
                write!(f, "{}", Color::Yellow.paint("compile failure"))?;
                if verbose {
                    write!(f, "\n{}", diagnostics)?;
                }
                Ok(())
            }
            Self::TtxFail { code, std_err } => {
                write!(f, "ttx failure ({:?}) stderr:\n{}", code, std_err)
            }
            Self::CompareFail { expected, result } => {
                if verbose {
                    writeln!(f, "compare failure")?;
                    fea_rs::util::write_line_diff(f, &result, &expected)
                } else {
                    write!(f, "{}", Color::Blue.paint("compare failure"))
                }
            }
        }
    }
}

impl Debug for Reason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print_impl(f, false)
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
        .chain((800_u16..=1001).into_iter().map(GlyphIdent::Cid))
        .collect()
}

mod flags {
    use std::path::{Path, PathBuf};
    xflags::xflags! {

        /// Compile a fea file into a source font
        cmd args {
            optional -v, --verbose
            /// Optional comma separated list of words matching tests to run.
            ///
            /// e.g.: -t "spec5,GPOS" matches spec5h1.fea, spec5fi2.fea, GPOS_2.fea, etc
            optional -t, --test test_filter: String
        }
    }
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
