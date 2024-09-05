use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
    process::Command,
};

use crate::{Results, RunResult};

static SCRIPT_PATH: &str = "./resources/scripts/ttx_diff.py";
// in the format expected by timeout(1)
static TTX_TIME_BUDGET: &str = "5h";

pub(super) fn run_ttx_diff(source: &Path) -> RunResult<DiffOutput, DiffError> {
    let tempdir = tempfile::tempdir().expect("couldn't create tempdir");
    let outdir = tempdir.path();
    let output = match Command::new("timeout")
        .arg(TTX_TIME_BUDGET)
        .arg("python")
        .arg(SCRIPT_PATH)
        .args(["--compare", "default"])
        .arg("--json")
        .arg("--outdir")
        .arg(outdir)
        .arg(source)
        .output()
    {
        Err(e) => return RunResult::Fail(DiffError::Other(e.to_string())),
        Ok(val) => val,
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    match output.status.code() {
        // success, diffs are identical
        Some(0) => RunResult::Success(DiffOutput::Identical),
        // there are diffs, or one or more compilers did not finish
        Some(2) => match serde_json::from_slice::<RawDiffOutput>(&output.stdout) {
            Err(_) => {
                let output = String::from_utf8_lossy(&output.stdout);
                eprintln!("MALFORMED JSON? '{output}'");
                std::process::exit(1);
            }
            Ok(RawDiffOutput::Success(success)) => {
                if success.is_empty() {
                    RunResult::Success(DiffOutput::Identical)
                } else {
                    RunResult::Success(DiffOutput::Diffs(success))
                }
            }
            Ok(RawDiffOutput::Error(error)) => RunResult::Fail(DiffError::CompileFailed(error)),
        },
        Some(124) => RunResult::Fail(DiffError::Other("ttx_diff timed out".to_string())),
        // unhandled exception or sigterm
        _ => RunResult::Fail(DiffError::Other(format!("unknown error '{stderr}'"))),
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum DiffOutput {
    Identical,
    Diffs(BTreeMap<String, DiffValue>),
}

/// Summary of one ttx_diff run
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct Summary {
    pub(crate) total_targets: u32,
    pub(crate) identical: u32,
    pub(crate) produced_diff: u32,
    pub(crate) fontc_failed: u32,
    pub(crate) fontmake_failed: u32,
    pub(crate) both_failed: u32,
    pub(crate) other_failure: u32,
    pub(crate) diff_perc_including_failures: f32,
    pub(crate) diff_perc_excluding_failures: f32,
}

impl Summary {
    pub(crate) fn new(result: &Results<DiffOutput, DiffError>) -> Self {
        let Results {
            success,
            failure,
            panic,
            skipped,
        } = &result;
        let total_targets = (success.len() + failure.len() + skipped.len() + panic.len()) as u32;
        let n_failed = failure.len() + panic.len();
        let identical = success
            .values()
            .filter(|x| matches!(x, DiffOutput::Identical))
            .count() as _;
        let produced_diff = success.len() as u32 - identical;
        let total_diff = success
            .values()
            .filter_map(|v| match v {
                DiffOutput::Identical => None,
                DiffOutput::Diffs(diffs) => diffs.get("total").and_then(|v| match v {
                    DiffValue::Ratio(v) => Some(*v),
                    DiffValue::Only(_) => None,
                }),
            })
            .sum::<f32>();
        let total_diff = total_diff + (identical as f32);
        let diff_perc_including_failures = total_diff / (n_failed + success.len()) as f32 * 100.;
        let diff_perc_excluding_failures = total_diff / success.len() as f32 * 100.;
        let (mut fontc_failed, mut fontmake_failed, mut both_failed, mut other_failure) =
            (0, 0, 0, 0);
        for fail in failure.values() {
            match fail {
                DiffError::CompileFailed(err) if err.fontc.is_some() && err.fontmake.is_some() => {
                    both_failed += 1
                }
                DiffError::CompileFailed(err) if err.fontc.is_some() => fontc_failed += 1,
                DiffError::CompileFailed(err) if err.fontmake.is_some() => fontmake_failed += 1,
                DiffError::CompileFailed(_) => unreachable!(),
                DiffError::Other(_) => other_failure += 1,
            }
        }

        Summary {
            total_targets,
            identical,
            produced_diff,
            fontc_failed,
            fontmake_failed,
            both_failed,
            other_failure,
            diff_perc_including_failures,
            diff_perc_excluding_failures,
        }
    }
}

pub(super) fn print_report(results: &Results<DiffOutput, DiffError>, verbose: bool) {
    let summary = Summary::new(results);
    let Results {
        success,
        failure,
        skipped,
        ..
    } = &results;
    println!("### tried to diff {} targets ###\n", summary.total_targets);
    println!(
        "{} identical, {} diffs, {} failed, {} skipped",
        summary.identical,
        summary.produced_diff,
        results.failure.len(),
        skipped.len()
    );
    println!(
        "total diff score {:.2}% (including failures: {:.2}%)\n",
        summary.diff_perc_excluding_failures, summary.diff_perc_including_failures
    );

    let comp_fails = failure
        .iter()
        .filter_map(|(k, v)| match v {
            DiffError::CompileFailed(error) => Some((k, error)),
            DiffError::Other(_) => None,
        })
        .collect::<BTreeMap<_, _>>();

    let other_fails = failure
        .iter()
        .filter_map(|(k, v)| match v {
            DiffError::CompileFailed(_) => None,
            DiffError::Other(e) => Some((k, e)),
        })
        .collect::<BTreeMap<_, _>>();

    let (mut fontc_fail, mut fontmake_fail, mut both_fail) =
        (BTreeSet::new(), BTreeSet::new(), BTreeSet::new());

    for (path, fail) in &comp_fails {
        match (fail.fontc.is_some(), fail.fontmake.is_some()) {
            (true, false) => fontc_fail.insert(*path),
            (false, true) => fontmake_fail.insert(*path),
            (true, true) => both_fail.insert(*path),
            _ => unreachable!(),
        };
    }

    println!("{} targets failed both compilers", both_fail.len());
    println!("{} targets failed on fontc only", fontc_fail.len(),);
    println!("{} targets failed on fontmake only", fontmake_fail.len());
    println!("{} targets failed for other reasons", other_fails.len());

    if !verbose {
        println!("\nfor more information, pass --verbose/-v");
        return;
    }

    if summary.identical != 0 {
        println!("\n### {} were identical: ###\n", summary.identical);
        for path in success
            .iter()
            .filter_map(|(k, v)| matches!(v, DiffOutput::Identical).then_some(k))
        {
            println!("{}", path.display())
        }
    }
    if !success.is_empty() && summary.identical != success.len() as u32 {
        println!(
            "\n### {} produced diffs: ###\n",
            success.len() as u32 - summary.identical
        );
        for (path, diff) in success.iter().filter_map(|(k, v)| match v {
            DiffOutput::Identical => None,
            DiffOutput::Diffs(diffs) => Some((k, diffs)),
        }) {
            let total = diff
                .get("total")
                .and_then(|v| match v {
                    DiffValue::Ratio(r) => Some(*r),
                    DiffValue::Only(_) => None,
                })
                .unwrap();
            println!("{}: {:.2}%", path.display(), total * 100.);
        }
    }

    fn print_compiler_failures(name: &str, fails: &BTreeSet<&PathBuf>) {
        if fails.is_empty() {
            return;
        }

        println!("\n### {} failed to compile on {} ###\n", fails.len(), name);
        for path in fails {
            println!("{}", path.display())
        }
    }

    print_compiler_failures("both", &both_fail);
    print_compiler_failures("fontmake", &fontmake_fail);
    print_compiler_failures("fontc", &fontc_fail);

    if !other_fails.is_empty() {
        println!(
            "\n### {} failed for other reasons: ###\n",
            other_fails.len()
        );

        for (path, reason) in other_fails {
            println!("{}: '{reason}'", path.display());
        }
    }

    if !skipped.is_empty() {
        println!("\n### {} were skipped: ###\n", skipped.len());
        for (path, reason) in skipped {
            println!("{}: '{reason}'", path.display());
        }
    }
}

/// make sure we can find and execute ttx_diff script
pub(super) fn assert_can_run_script() {
    let path = Path::new(SCRIPT_PATH);
    if !path.exists() {
        eprintln!(
            "cannot find script at {}",
            path.canonicalize().as_deref().unwrap_or(path).display()
        );
        std::process::exit(1);
    }
    match Command::new("python")
        .arg(SCRIPT_PATH)
        .arg("--only_check_args")
        .output()
    {
        Ok(output) if output.status.success() => return,
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            eprintln!("could not run ttx_diff.py. Have you setup your venv?");
            if !stdout.is_empty() {
                eprintln!("stdout: {stdout}");
            }
            if !stderr.is_empty() {
                eprintln!("stderr: {stderr}");
            }
        }
        Err(e) => eprintln!("Error executing ttx_diff script: '{e}'"),
    }

    std::process::exit(1)
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
enum RawDiffOutput {
    Success(BTreeMap<String, DiffValue>),
    Error(CompileFailed),
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DiffError {
    CompileFailed(CompileFailed),
    Other(String),
}

/// One or both compilers failed to run
#[derive(Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) struct CompileFailed {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) fontc: Option<CompilerFailure>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) fontmake: Option<CompilerFailure>,
}

/// Info regarding the failure of a single compiler
#[derive(Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) struct CompilerFailure {
    pub(crate) command: String,
    pub(crate) stderr: String,
}

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case", untagged)]
pub(super) enum DiffValue {
    Ratio(f32),
    Only(String),
}

impl DiffValue {
    pub(crate) fn ratio(&self) -> Option<f32> {
        match self {
            DiffValue::Ratio(r) => Some(*r),
            DiffValue::Only(_) => None,
        }
    }
}

impl DiffOutput {
    pub(crate) fn iter_tables(&self) -> impl Iterator<Item = &str> + '_ {
        let opt_map = match self {
            DiffOutput::Identical => None,
            DiffOutput::Diffs(diffs) => Some(diffs),
        };

        opt_map
            .into_iter()
            .flat_map(|diffs| diffs.keys().map(String::as_str))
    }
}

impl std::fmt::Display for DiffValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DiffValue::Ratio(ratio) => {
                let perc = ratio * 100.;
                write!(f, "{perc:.3}%")
            }
            DiffValue::Only(compiler) => write!(f, "{compiler} only"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_serde() {
        fn expect_success(s: &str, expected: &[(&str, f32)]) -> bool {
            let expected = expected
                .iter()
                .map(|(s, val)| (s.to_string(), DiffValue::Ratio(*val)))
                .collect();
            let result: RawDiffOutput = serde_json::from_str(s).unwrap();
            match result {
                RawDiffOutput::Error(CompileFailed { fontc, fontmake }) => {
                    eprintln!("unexpected error: {fontc:?} '{fontmake:?}'");
                    false
                }
                RawDiffOutput::Success(items) if items == expected => true,
                RawDiffOutput::Success(items) => {
                    eprintln!("wrong items: {items:?}");
                    false
                }
            }
        }

        let success = "{\"success\": {\"GPOS\": 0.9995}}";
        assert!(expect_success(success, &[("GPOS", 0.9995f32)]));
        let success = "{\"success\": {}}";
        assert!(expect_success(success, &[]));
        let error = "{\"error\": {\"fontmake\": {\"command\": \"fontmake -o variable --output-path fontmake.ttf\", \"stderr\": \"oh no\"}}}";
        let RawDiffOutput::Error(CompileFailed {
            fontc: None,
            fontmake: Some(CompilerFailure { command, stderr }),
        }) = serde_json::from_str(error).unwrap()
        else {
            panic!("a quite unlikely success")
        };
        assert!(command.starts_with("fontmake -o"));
        assert_eq!(stderr, "oh no");
    }
}
