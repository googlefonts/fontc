//! Run the compiler against a bunch of inputs, comparing them with
//! the results of fonttools.

use std::path::Path;

use fea_rs::util::ttx;

static TEST_DATA: &str = "./fea-rs/test-data/fonttools-tests";
static WIP_DIFF_DIR: &str = "./wip";

fn main() {
    let args = flags::Args::from_env().unwrap();

    let results = ttx::run_all_tests(TEST_DATA, args.test.as_ref());
    eprintln!("{:?}", results.printer(args.verbose));
    if args.write_diff {
        save_wip_diffs(&results);
    }
    if results.has_failures() {
        std::process::exit(1);
    }
}

fn save_wip_diffs(results: &ttx::Report) {
    if !Path::new(WIP_DIFF_DIR).exists() {
        std::fs::create_dir(WIP_DIFF_DIR).unwrap();
    }
    for failure in &results.results {
        if let ttx::TestResult::CompareFail {
            expected, result, ..
        } = &failure.reason
        {
            let file_name = failure.path.file_name().unwrap();
            let out_path = Path::new(WIP_DIFF_DIR)
                .join(file_name)
                .with_extension("expected_diff");
            let diff = ttx::plain_text_diff(expected, result);
            eprintln!("saved diff to {}", out_path.display());
            std::fs::write(out_path, diff).unwrap();
        }
    }
}

mod flags {
    xflags::xflags! {

        /// Compile a fea file into a source font
        cmd args {
            optional -v, --verbose
            /// Optional comma separated list of words matching tests to run.
            ///
            /// e.g.: -t "spec5,GPOS" matches spec5h1.fea, spec5fi2.fea, GPOS_2.fea, etc
            optional -t, --test test_filter: String
            /// Write diffs to a ./wip directory
            optional -d, --write-diff
        }
    }
}
