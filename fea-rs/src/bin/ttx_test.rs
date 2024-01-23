//! Run the compiler against a bunch of inputs, comparing them with
//! the results of fonttools.

use std::path::{Path, PathBuf};

use clap::Parser;
use fea_rs::util::ttx;

static WIP_DIFF_DIR: &str = "./wip";

fn main() {
    env_logger::init();
    let args = Args::parse();

    let results = ttx::run_fonttools_tests(args.test_filter.as_ref());

    if let Some(to_compare) = args
        .compare
        .as_ref()
        .map(std::fs::read)
        .transpose()
        .unwrap()
    {
        let old_result: ttx::Report = serde_json::from_slice(&to_compare).unwrap();
        eprintln!("{:?}", results.compare_printer(&old_result));
    } else {
        eprintln!("{results:?}",);
    }

    if let Some(path) = args.save {
        let serialized = serde_json::to_vec(&results).unwrap();
        std::fs::write(path, serialized).unwrap();
    }

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
            log::info!("saved diff to {}", out_path.display());
            std::fs::write(out_path, diff).unwrap();
        }
    }
}

/// Compare compilation output to expected results
#[derive(clap::Parser, Debug)]
#[command(author, version, long_about = None)]
struct Args {
    /// Optional comma separated list of words matching tests to run.
    ///
    /// e.g.: -t "spec5,GPOS" matches spec5h1.fea, spec5fi2.fea, GPOS_2.fea, etc
    #[arg(short, long = "test")]
    test_filter: Option<String>,
    /// Write diffs to a ./wip directory
    #[arg(short, long)]
    write_diff: bool,
    /// Save the results to a file, for later comparison.
    ///
    /// You can compare runs with the --compare option
    #[arg(short, long)]
    save: Option<PathBuf>,
    /// Compare results against those previously saved
    #[arg(short, long)]
    compare: Option<PathBuf>,
}
