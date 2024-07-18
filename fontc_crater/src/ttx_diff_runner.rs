use std::{collections::BTreeMap, path::Path, process::Command};

use crate::{Results, RunResult};

static SCRIPT_PATH: &str = "./resources/scripts/ttx_diff.py";

pub(super) fn run_ttx_diff(source: &Path) -> RunResult<DiffOutput, DiffError> {
    let tempdir = tempfile::tempdir().expect("couldn't create tempdir");
    let outdir = tempdir.path();
    let output = match Command::new("python")
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
        // unhandled exception or sigterm
        _ => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            RunResult::Fail(DiffError::Other(format!("unknown error '{stderr}'")))
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum DiffOutput {
    Identical,
    Diffs(BTreeMap<String, DiffValue>),
}

pub(super) fn print_report(results: &Results<DiffOutput, DiffError>, verbose: bool) {
    let Results {
        success,
        failure,
        panic,
        skipped,
    } = &results;
    let n_total = success.len() + failure.len() + skipped.len() + panic.len();
    let n_failed = failure.len() + panic.len();
    let n_identical = success
        .values()
        .filter(|x| matches!(x, DiffOutput::Identical))
        .count();
    let n_diffs = success.len() - n_identical;
    println!("### tried to diff {n_total} targets ###");
    println!(
        "{n_identical} identical, {n_diffs} diffs, {n_failed} failed, {} skipped",
        skipped.len()
    );
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
    let total_diff = total_diff + (n_identical as f32);
    let total_diff_with_failures = total_diff / (n_failed + success.len()) as f32 * 100.;
    let total_diff_no_failures = total_diff / success.len() as f32 * 100.;
    println!(
        "total diff score {:.2}% (including failures: {:.2}%)",
        total_diff_no_failures, total_diff_with_failures
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

    let fontc_fails = comp_fails.values().filter(|x| x.fontc.is_some()).count();
    let fontmake_fails = comp_fails.values().filter(|x| x.fontmake.is_some()).count();

    println!("{fontc_fails} targets failed on fontc");
    println!("{fontmake_fails} targets failed on fontmake");
    println!("{} targets failed for other reasons", other_fails.len());

    if !verbose {
        println!("\nfor more information, pass --verbose/-v");
        return;
    }

    if n_identical != 0 {
        println!("\n### {n_identical} were identical: ###\n");
        for path in success
            .iter()
            .filter_map(|(k, v)| matches!(v, DiffOutput::Identical).then_some(k))
        {
            println!("{}", path.display())
        }
    }
    if !success.is_empty() && n_identical != success.len() {
        println!(
            "\n### {} produced diffs: ###\n",
            success.len() - n_identical
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

    if !comp_fails.is_empty() {
        println!(
            "\n### {} failed to compile on at least one compiler: ###\n",
            comp_fails.len()
        );

        for (path, fail) in comp_fails {
            println!("{}", path.display());
            if let Some(fail) = &fail.fontc {
                println!("  {}", fail.command)
            }
            if let Some(fail) = &fail.fontmake {
                println!("  {}", fail.command)
            }
        }
    }

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
    fontc: Option<CompilerFailure>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fontmake: Option<CompilerFailure>,
}

/// Info regarding the failure of a single compiler
#[derive(Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) struct CompilerFailure {
    command: String,
    stderr: String,
}

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case", untagged)]
pub(super) enum DiffValue {
    Ratio(f32),
    Only(String),
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
