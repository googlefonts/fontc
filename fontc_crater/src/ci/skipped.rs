//! Configs we have reviewed and don't build.
//!
//! These are listed in `skipped.json` in the results directory; see the
//! README for the format.

use std::{
    collections::{BTreeMap, HashSet},
    path::Path,
};

use google_fonts_sources::FontSource;

use crate::error::Error;

static SKIPPED_FILE: &str = "skipped.json";

/// The configs we don't build.
#[derive(Debug, Default)]
pub(crate) struct SkippedConfigs(Vec<Skip>);

/// A group of configs we don't build, for the same reason.
#[derive(Debug, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Skip {
    /// Config key → the rev it was reviewed at
    configs: BTreeMap<String, String>,
    pub(crate) reason: String,
    pub(crate) link: Option<String>,
}

/// The inputs a run skipped, and what changed since they were reviewed.
pub(crate) struct SkipReport<'a> {
    /// Each entry with the inputs it skipped
    pub(crate) skipped: Vec<(&'a Skip, Vec<&'a FontSource>)>,
    /// Skipped inputs whose rev wasn't reviewed, as (config key, rev, reviewed rev)
    pub(crate) rev_changed: Vec<(String, &'a str, &'a str)>,
    /// Listed configs that have no input
    pub(crate) unused: Vec<&'a str>,
}

impl SkippedConfigs {
    /// Load the skipped configs listed in `results_dir`, if there are any.
    pub(crate) fn load(results_dir: &Path) -> Result<Self, Error> {
        let path = results_dir.join(SKIPPED_FILE);
        super::load_json_if_exists_else_default(&path).map(Self)
    }

    /// The entry that skips this input, at any rev, and the rev it was reviewed at.
    fn get(&self, source: &FontSource) -> Option<(usize, &str)> {
        let key = config_key(source);
        self.0
            .iter()
            .enumerate()
            .find_map(|(i, skip)| Some((i, skip.configs.get(&key)?.as_str())))
    }

    pub(crate) fn skips(&self, source: &FontSource) -> bool {
        self.get(source).is_some()
    }

    /// Group the skipped inputs by entry, and log what changed since the review.
    pub(crate) fn report<'a>(&'a self, inputs: &'a [FontSource]) -> SkipReport<'a> {
        let mut skipped = vec![Vec::new(); self.0.len()];
        let mut rev_changed = Vec::new();
        let mut seen = HashSet::new();
        for source in inputs {
            let Some((i, reviewed)) = self.get(source) else {
                continue;
            };
            let key = config_key(source);
            let rev = source.git_rev();
            if !rev.starts_with(reviewed) {
                let rev = rev.get(..10).unwrap_or(rev);
                log::warn!("skipped config '{key}' is at {rev}, but was reviewed at {reviewed}");
                rev_changed.push((key.clone(), rev, reviewed));
            }
            skipped[i].push(source);
            seen.insert(key);
        }
        let unused = self
            .0
            .iter()
            .flat_map(|skip| skip.configs.keys())
            .filter(|key| !seen.contains(*key))
            .map(String::as_str)
            .collect::<Vec<_>>();
        for key in &unused {
            log::warn!("skipped config '{key}' isn't in the inputs");
        }
        SkipReport {
            skipped: self.0.iter().zip(skipped).collect(),
            rev_changed,
            unused,
        }
    }
}

/// The config as it starts a target id: `org/repo/path/to/config.yaml`.
///
/// A config in google/fonts is `org/repo/$VIRTUAL/google/fonts/…`.
pub(crate) fn config_key(source: &FontSource) -> String {
    let virtual_dir = if source.config_is_external() {
        "$VIRTUAL/"
    } else {
        ""
    };
    format!(
        "{}/{}/{virtual_dir}{}",
        source.repo_org(),
        source.repo_name(),
        source.config.display()
    )
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;
    use crate::Target;

    const AKATAB: &str = "silnrsi/font-akatab/$VIRTUAL/google/fonts/ofl/akatab/config.yaml";
    const CABIN: &str = "impallari/Cabin/$VIRTUAL/google/fonts/ofl/cabin/config.yaml";
    const CHIRON: &str = "chiron-fonts/chiron-hei-hk/scripts/config.yaml";

    // an input whose config_key is `key`
    fn input(key: &str, rev: &str) -> FontSource {
        let mut parts = key.splitn(3, '/');
        let repo = format!("{}/{}", parts.next().unwrap(), parts.next().unwrap());
        let config = parts.next().unwrap();
        let (config, external) = match config.strip_prefix("$VIRTUAL/") {
            Some(config) => (config, true),
            None => (config, false),
        };
        serde_json::from_value(json!({
            "repo_url": format!("https://github.com/{repo}"),
            "rev": rev,
            "config": config,
            "config_is_external": external,
        }))
        .unwrap()
    }

    fn skipped() -> SkippedConfigs {
        SkippedConfigs(
            serde_json::from_value(json!([
                { "configs": { AKATAB: "54e989b80b", CABIN: "1234567890" }, "reason": "a" },
                { "configs": { CHIRON: "5d80c2cecb" }, "reason": "b", "link": "https://x" },
            ]))
            .unwrap(),
        )
    }

    #[test]
    fn key_is_the_start_of_a_target_id() {
        let akatab = input(AKATAB, "54e989b80b2fd6ba");
        let target = Target::new(
            "silnrsi/font-akatab",
            "54e989b80b",
            &akatab.config,
            true,
            "source/Akatab.designspace",
        );
        assert_eq!(config_key(&akatab), AKATAB);
        assert!(target.to_string().starts_with(&format!("{AKATAB} ")));

        let chiron = input(CHIRON, "5d80c2cecb8f8e7f");
        let target = Target::new(
            "chiron-fonts/chiron-hei-hk",
            "5d80c2cecb",
            &chiron.config,
            false,
            "ChironHeiHK.designspace",
        );
        assert_eq!(config_key(&chiron), CHIRON);
        assert!(target.to_string().starts_with(&format!("{CHIRON} ")));
    }

    #[test]
    fn reason_is_required() {
        let entry = json!([{ "configs": { CHIRON: "5d80c2cecb" } }]);
        assert!(serde_json::from_value::<Vec<Skip>>(entry).is_err());
    }

    #[test]
    fn configs_are_skipped_at_any_rev() {
        let skipped = skipped();
        assert!(skipped.skips(&input(AKATAB, "54e989b80b2fd6ba")));
        assert!(skipped.skips(&input(AKATAB, "0000000000")));
        let condensed = "impallari/Cabin/$VIRTUAL/google/fonts/ofl/cabincondensed/config.yaml";
        assert!(!skipped.skips(&input(condensed, "1234567890")));
    }

    #[test]
    fn report_warns_about_new_revs_and_unused_configs() {
        let skipped = skipped();
        let inputs = [
            input(AKATAB, "54e989b80b2fd6ba"),
            input(CHIRON, "0123456789abcdef"),
            input("org/built/sources/config.yaml", "0123456789"),
        ];
        let report = skipped.report(&inputs);
        let counts = report
            .skipped
            .iter()
            .map(|(skip, inputs)| (skip.reason.as_str(), inputs.len()))
            .collect::<Vec<_>>();
        assert_eq!(counts, [("a", 1), ("b", 1)]);
        assert_eq!(
            report.rev_changed,
            [(CHIRON.to_string(), "0123456789", "5d80c2cecb")]
        );
        assert_eq!(report.unused, [CABIN]);
    }
}
