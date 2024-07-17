use std::{collections::BTreeMap, path::Path, process::Command};

use crate::RunResult;

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

#[derive(Clone, Debug, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum DiffOutput {
    Identical,
    Diffs(BTreeMap<String, DiffValue>),
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

#[derive(Debug, serde::Serialize)]
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
