use std::{collections::BTreeMap, path::PathBuf, process::Command};

use crate::{BuildType, Results, RunResult, Target, ci::ResultsCache};

// Run ttx-diff via python -m to ensure we use the venv's installed version
static TTX_DIFF_MODULE: &str = "ttx_diff";

pub(super) struct TtxContext {
    pub fontc_path: PathBuf,
    pub normalizer_path: PathBuf,
    pub source_cache: PathBuf,
    pub results_cache: ResultsCache,
}

pub(super) fn run_ttx_diff(ctx: &TtxContext, target: &Target) -> RunResult<DiffOutput, DiffError> {
    let tempdir = tempfile::tempdir().expect("couldn't create tempdir");
    let outdir = tempdir.path();
    let source_path = target.source_path(&ctx.source_cache);
    let compare = target.build.name();
    let build_dir = outdir.join(compare);
    ctx.results_cache
        .copy_cached_files_to_build_dir(target, &build_dir);
    let mut cmd = Command::new("python3");
    cmd.args([
        "-m",
        TTX_DIFF_MODULE,
        "--json",
        "--compare",
        compare,
        "--outdir",
    ])
    .arg(outdir)
    .arg("--fontc_path")
    .arg(&ctx.fontc_path)
    .arg("--normalizer_path")
    .arg(&ctx.normalizer_path)
    .args(["--rebuild", "fontc"]);
    if target.build == BuildType::GfTools {
        cmd.arg("--config")
            .arg(target.config_path(&ctx.source_cache));
    }
    cmd.arg(source_path)
        // set this flag so we have a stable 'modified date'
        .env("SOURCE_DATE_EPOCH", "1730302089");
    let output = match cmd.output() {
        Err(e) => return RunResult::Fail(DiffError::Other(e.to_string())),
        Ok(val) => val,
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    let result = match output.status.code() {
        // success, diffs are identical
        Some(0) => RunResult::Success(DiffOutput::Identical),
        // there are diffs, or one or more compilers did not finish
        Some(2) => match serde_json::from_slice::<RawDiffOutput>(&output.stdout) {
            Err(_) => {
                let output = String::from_utf8_lossy(&output.stdout);
                log::error!("MALFORMED JSON? '{output}'");
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
        Some(other) => RunResult::Fail(DiffError::Other(format!(
            "unknown error (status {other}): '{stderr}'"
        ))),
        None => {
            let signal = if cfg!(target_family = "unix") {
                use std::os::unix::process::ExitStatusExt;
                output.status.signal().unwrap_or(0)
            } else {
                0
            };
            RunResult::Fail(DiffError::Other(format!(
                "unknown error (signal {signal}): '{stderr}'"
            )))
        }
    };

    if let RunResult::Fail(DiffError::Other(err)) = &result {
        // these errors indicate something unexpected happening at runtime,
        // so it is useful to see them in our logs.
        log::warn!("error running {target} '{err}'");
    }

    if fontmake_finished(&result) {
        ctx.results_cache
            .save_built_files_to_cache(target, &build_dir);
    }
    result
}

fn fontmake_finished(result: &RunResult<DiffOutput, DiffError>) -> bool {
    match result {
        RunResult::Success(_) => true,
        RunResult::Fail(DiffError::CompileFailed(diff)) => diff.fontmake.is_none(),
        RunResult::Fail(DiffError::Other(_)) => false,
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum DiffOutput {
    Identical,
    Diffs(BTreeMap<String, DiffValue>),
}

/// Summary of one ttx_diff run
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
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
            success, failure, ..
        } = &result;
        let total_targets = (success.len() + failure.len()) as u32;
        let n_failed = failure.len();
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
        let diff_perc_including_failures =
            non_nan(total_diff / (n_failed + success.len()) as f32) * 100.;
        let diff_perc_excluding_failures = non_nan(total_diff / success.len() as f32) * 100.;
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

fn assert_has_timeout_coreutil() {
    match Command::new("which").arg("timeout").output() {
        Ok(out) if out.status.success() => return,
        Err(e) => eprintln!("'which' failed!? '{e}'"),
        Ok(_) => eprintln!(
            "could not find 'timeout'. You may need to install coreutils (on macos, `brew install coreutils`)"
        ),
    }
    std::process::exit(1);
}

// replace nan with 0
fn non_nan(val: f32) -> f32 {
    if val.is_nan() { 0.0 } else { val }
}

/// make sure we can find and execute ttx-diff module
pub(super) fn assert_can_run_script() {
    // first check that we can find timeout(1) (not present on macOS by default,
    // install via homebrew)
    assert_has_timeout_coreutil();
    // then check that we can run ttx-diff via python -m
    match Command::new("python3")
        .args(["-m", TTX_DIFF_MODULE, "--only_check_args"])
        .output()
    {
        Ok(output) if output.status.success() => return,
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            eprintln!("could not run ttx-diff. Have you setup your venv?");
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

    pub(crate) fn as_n_of_bytes(&self) -> i32 {
        match self {
            DiffValue::Ratio(r) => *r as _,
            // this branch shouldn't be hit but let's notice if it is??
            DiffValue::Only(_) => 123456789,
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
