//! Failures we have reviewed and accepted.
//!
//! These are listed in `accepted.json` in the results directory; see the
//! README for the format.

use std::{
    collections::{BTreeMap, HashSet},
    path::Path,
};

use crate::{
    Target, TargetId,
    error::Error,
    ttx_diff_runner::{CompileFailed, DiffError},
};

use super::DiffResults;

static ACCEPTED_FILE: &str = "accepted.json";

/// The targets whose failures we have accepted.
#[derive(Debug, Default)]
pub(crate) struct AcceptedFailures {
    entries: Vec<AcceptedFailure>,
    // values are indices into entries
    by_target: BTreeMap<TargetId, usize>,
}

/// A group of targets that fail for the same known reason.
#[derive(Debug, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct AcceptedFailure {
    targets: Vec<TargetId>,
    pub(crate) fails: FailingCompiler,
    stderr_contains: Option<String>,
    pub(crate) reason: String,
    pub(crate) link: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum FailingCompiler {
    Fontc,
    Fontmake,
    Both,
}

/// What happened to a target that no longer fails as accepted.
pub(crate) enum Surprise<'a> {
    Passed,
    FailedDifferently(&'a DiffError),
    NotRun,
}

impl AcceptedFailures {
    /// Load the accepted failures listed in `results_dir`, if there are any.
    pub(crate) fn load(results_dir: &Path) -> Result<Self, Error> {
        let path = results_dir.join(ACCEPTED_FILE);
        let entries = super::load_json_if_exists_else_default(&path)?;
        Ok(Self::new(entries))
    }

    fn new(entries: Vec<AcceptedFailure>) -> Self {
        let mut by_target = BTreeMap::new();
        for (i, entry) in entries.iter().enumerate() {
            for target in &entry.targets {
                if by_target.insert(target.clone(), i).is_some() {
                    log::warn!("'{target}' is listed in more than one accepted failure");
                }
            }
        }
        Self { entries, by_target }
    }

    /// The accepted failure that lists this target, at any revision.
    pub(crate) fn get(&self, target: &Target) -> Option<&AcceptedFailure> {
        self.by_target.get(target.id()).map(|i| &self.entries[*i])
    }

    /// Whether this target failed the way we accepted.
    pub(crate) fn is_accepted(&self, target: &Target, error: &DiffError) -> bool {
        self.get(target)
            .is_some_and(|accepted| accepted.matches(error))
    }

    /// Listed targets that no longer fail as accepted.
    pub(crate) fn surprises<'a>(
        &'a self,
        results: &'a DiffResults,
    ) -> Vec<(&'a TargetId, &'a AcceptedFailure, Surprise<'a>)> {
        let mut seen = HashSet::new();
        let mut surprises = Vec::new();
        for target in results.targets() {
            let Some(accepted) = self.get(target) else {
                continue;
            };
            let id = target.id();
            seen.insert(id);
            match results.failure.get(target) {
                None => surprises.push((id, accepted, Surprise::Passed)),
                Some(error) if !accepted.matches(error) => {
                    surprises.push((id, accepted, Surprise::FailedDifferently(error)))
                }
                Some(_) => (),
            }
        }
        for accepted in &self.entries {
            for target in &accepted.targets {
                if !seen.contains(target) {
                    surprises.push((target, accepted, Surprise::NotRun));
                }
            }
        }
        surprises
    }
}

impl AcceptedFailure {
    fn matches(&self, error: &DiffError) -> bool {
        let DiffError::CompileFailed(failed) = error else {
            return false;
        };
        FailingCompiler::of(failed) == Some(self.fails)
            && self.stderr_contains.as_ref().is_none_or(|text| {
                [&failed.fontc, &failed.fontmake]
                    .into_iter()
                    .flatten()
                    .any(|failure| failure.stderr.contains(text.as_str()))
            })
    }
}

impl FailingCompiler {
    pub(crate) fn of(failed: &CompileFailed) -> Option<Self> {
        match (&failed.fontc, &failed.fontmake) {
            (Some(_), Some(_)) => Some(FailingCompiler::Both),
            (Some(_), None) => Some(FailingCompiler::Fontc),
            (None, Some(_)) => Some(FailingCompiler::Fontmake),
            (None, None) => None,
        }
    }

    pub(crate) fn name(&self) -> &'static str {
        match self {
            FailingCompiler::Fontc => "fontc",
            FailingCompiler::Fontmake => "fontmake",
            FailingCompiler::Both => "both",
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;
    use crate::ttx_diff_runner::Summary;

    const OLD_REV: &str = "org/repo/sources/config.yaml Font.glyphs?0123456789 (default)";
    const NEW_REV: &str = "org/repo/sources/config.yaml Font.glyphs?abcdef0123 (default)";
    const OTHER: &str = "org/repo/sources/config.yaml Other.glyphs?abcdef0123 (default)";

    // Target only deserializes from borrowed strings, so go through text
    fn from_json<T: serde::de::DeserializeOwned>(value: serde_json::Value) -> T {
        serde_json::from_str(&value.to_string()).unwrap()
    }

    fn accept_both_fail(stderr_contains: Option<&str>) -> AcceptedFailures {
        let mut entry = json!({
            "targets": [OLD_REV],
            "fails": "both",
            "reason": "the source is broken",
        });
        if let Some(text) = stderr_contains {
            entry["stderr_contains"] = json!(text);
        }
        AcceptedFailures::new(from_json(json!([entry])))
    }

    fn fontc_fails(target: &str) -> serde_json::Value {
        json!({ target: { "compile_failed": {
            "fontc": { "command": "fontc", "stderr": "fontc says no" },
        } } })
    }

    fn both_fail(target: &str) -> serde_json::Value {
        json!({ target: { "compile_failed": {
            "fontc": { "command": "fontc", "stderr": "fontc says no" },
            "fontmake": { "command": "fontmake", "stderr": "fontmake says no" },
        } } })
    }

    fn results(success: serde_json::Value, failure: serde_json::Value) -> DiffResults {
        from_json(json!({ "success": success, "failure": failure }))
    }

    #[test]
    fn matches_across_revisions() {
        let accepted = accept_both_fail(None);
        let results = results(json!({}), both_fail(NEW_REV));
        let (target, error) = results.failure.iter().next().unwrap();
        assert!(accepted.is_accepted(target, error));
        assert!(accepted.surprises(&results).is_empty());
    }

    #[test]
    fn failing_differently_is_a_surprise() {
        let accepted = accept_both_fail(None);
        let results = results(json!({}), fontc_fails(NEW_REV));
        let (target, error) = results.failure.iter().next().unwrap();
        assert!(!accepted.is_accepted(target, error));
        assert!(matches!(
            accepted.surprises(&results).as_slice(),
            [(_, _, Surprise::FailedDifferently(_))]
        ));
    }

    #[test]
    fn stderr_must_match() {
        let results = results(json!({}), both_fail(NEW_REV));
        let (target, error) = results.failure.iter().next().unwrap();
        assert!(accept_both_fail(Some("fontmake says")).is_accepted(target, error));
        assert!(!accept_both_fail(Some("panicked")).is_accepted(target, error));
    }

    #[test]
    fn passing_and_missing_are_surprises() {
        let accepted = accept_both_fail(None);
        let passed = results(json!({ NEW_REV: "identical" }), json!({}));
        assert!(matches!(
            accepted.surprises(&passed).as_slice(),
            [(_, _, Surprise::Passed)]
        ));
        let not_run = results(json!({ OTHER: "identical" }), json!({}));
        assert!(matches!(
            accepted.surprises(&not_run).as_slice(),
            [(_, _, Surprise::NotRun)]
        ));
    }

    #[test]
    fn accepted_failures_count_as_matching() {
        let accepted = accept_both_fail(None);
        let results = results(json!({ OTHER: "identical" }), both_fail(NEW_REV));
        let summary = Summary::new(&results, &accepted);
        assert_eq!(summary.accepted_failure, 1);
        assert_eq!(summary.both_failed, 0);
        assert_eq!(summary.diff_perc_including_failures, 100.0);

        let summary = Summary::new(&results, &AcceptedFailures::default());
        assert_eq!(summary.accepted_failure, 0);
        assert_eq!(summary.both_failed, 1);
        assert_eq!(summary.diff_perc_including_failures, 50.0);
    }
}
