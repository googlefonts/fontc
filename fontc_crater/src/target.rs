//! targets of a compilation

use std::{
    ffi::OsStr,
    fmt::{Display, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use serde::{Deserialize, Serialize};

static VIRTUAL_CONFIG_DIR: &str = "sources";

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Target {
    /// path to the source repo, relative to the cache dir root
    repo_dir: PathBuf,
    sha: String,
    /// Path to the config file.
    ///
    /// - relative to the cache root if it is virtual;
    /// - relative to the repo root otherwise
    pub(crate) config: PathBuf,
    is_virtual: bool,
    /// Path to source file, relative to the source_dir
    source: PathBuf,
    pub(crate) build: BuildType,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum BuildType {
    Default,
    GfTools,
}

impl Target {
    pub(crate) fn new(
        repo_dir: impl Into<PathBuf>,
        sha: impl Into<String>,
        config: impl Into<PathBuf>,
        is_virtual: bool,
        source: impl Into<PathBuf>,
    ) -> Self {
        let mut sha = sha.into();
        let config = config.into();
        let source = source.into();
        sha.truncate(10);
        let config_dir = config.parent().unwrap();
        // if source is a sibling of config we can trim that bit.
        let source = source
            .strip_prefix(config_dir)
            .map(PathBuf::from)
            .unwrap_or(source);
        Self {
            repo_dir: repo_dir.into(),
            sha,
            config,
            is_virtual,
            source,
            build: BuildType::Default,
        }
    }

    pub(crate) fn to_gftools_target(&self) -> Self {
        Self {
            build: BuildType::GfTools,
            ..self.clone()
        }
    }

    /// Invariant: the source path is always in a directory
    ///
    /// If the config is virtual, then the source dir is '$REPO/sources'
    /// Otherwise, it is the parent directory of the config file.
    fn source_dir(&self) -> PathBuf {
        if self.is_virtual {
            self.repo_dir.join(VIRTUAL_CONFIG_DIR)
        } else {
            self.repo_dir.join(self.config.parent().unwrap())
        }
    }

    /// The org/repo part of the path, used for looking up repo urls
    pub(crate) fn repo_path(&self) -> &Path {
        &self.repo_dir
    }

    pub(crate) fn source_path(&self, git_cache: &Path) -> PathBuf {
        let mut out = git_cache.join(self.source_dir());
        out.push(&self.source);
        out
    }

    pub(crate) fn config_path(&self, git_cache: &Path) -> PathBuf {
        if self.is_virtual {
            git_cache.join(&self.config)
        } else {
            git_cache.join(&self.repo_dir).join(&self.config)
        }
    }

    // if a target was built in a directory with a sha, the repro command
    // does not need to include that part of the directory, so remove it.
    fn config_path_stripping_disambiguating_sha_if_necessary(&self) -> String {
        let mut path = self
            .config_path(Path::new("~/.fontc_crater_cache"))
            .display()
            .to_string();
        // NOTE: this relies on the fact that we always trim the sha to 10 chars,
        // both when we create a target and in google-fonts-sources when we
        // create the disambiguated checkout directory.
        if let Some(ix) = (!self.sha.is_empty())
            .then(|| path.find(&self.sha))
            .flatten()
        {
            path.replace_range(ix - 1..ix + self.sha.len(), "");
        }
        path
    }

    /// Return the path where we should cache the results of running this target.
    ///
    /// This is unique for each target, and is in the form,
    ///
    /// {BASE}{source_dir}/{config_stem}/{file_stem}/{build}
    ///
    /// where {source_dir} is the path to the sources/Sources directory of this
    /// target, relative to the root git cache.
    pub(crate) fn cache_dir(&self, in_dir: &Path) -> PathBuf {
        let config = self.config.file_stem().unwrap_or(OsStr::new("config"));
        let mut result = in_dir.join(self.source_dir());
        result.push(config);
        result.push(self.source.file_stem().unwrap());
        result.push(self.build.name());
        result
    }

    pub(crate) fn repro_command(&self, repo_url: &str) -> String {
        let repo_url = repo_url.trim();
        let source_path = self.source_path(Path::new(""));
        let rel_source_path = source_path
            .strip_prefix(&self.repo_dir)
            .expect("source always in repo");
        let sha_part = if !self.sha.is_empty() {
            format!("?{}", self.sha)
        } else {
            Default::default()
        };
        let mut cmd = format!(
            "python3 resources/scripts/ttx_diff.py '{repo_url}{sha_part}#{}'",
            rel_source_path.display()
        );
        if self.build == BuildType::GfTools {
            cmd.push_str(" --compare gftools");
            // we hard code this; repro will only work if they're using default
            // cache location
            write!(
                &mut cmd,
                " --config {}",
                self.config_path_stripping_disambiguating_sha_if_necessary()
            )
            .unwrap();
        }
        cmd
    }
}

impl BuildType {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            BuildType::Default => "default",
            BuildType::GfTools => "gftools",
        }
    }
}

impl Display for BuildType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let config_path = if self.is_virtual {
            self.repo_dir.join("$VIRTUAL").join(&self.config)
        } else {
            self.repo_dir.join(&self.config)
        };

        write!(
            f,
            "{} {}?{} ({})",
            config_path.display(),
            self.source.display(),
            self.sha,
            self.build
        )
    }
}

impl Serialize for Target {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Target {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(s).map_err(serde::de::Error::custom)
    }
}

/// in the format,
///
/// $ORG/$REPO/$CONFIG_PATH?$SHA $SRC_PATH ($BUILD_TYPE)
///
/// where a virtual config's $CONFIG_PATH starts with the literal path element
/// '$VIRTUAL'.
impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        // old version contain '($CONFIG.y[a]ml)'
        if s.contains("ml)") {
            return legacy_from_str_impl(s);
        }

        let (head, type_) = s
            .rsplit_once('(')
            .ok_or_else(|| "missing opening paren".to_string())?;

        let head = head.trim();
        let (config_part, source_part) = head
            .split_once(' ')
            .ok_or_else(|| "missing a space?".to_string())?;
        let (source, sha) = source_part
            .trim()
            .split_once('?')
            .ok_or_else(|| "missing '?'".to_string())?;
        let (split_at, _) = config_part
            .match_indices('/')
            .nth(1)
            .ok_or("missing second '/'")?;

        let (org_repo, config_part) = config_part.split_at(split_at);
        let config_part = config_part.trim_start_matches('/').trim();

        let (is_virtual, config_path) = if let Some(path) = config_part.strip_prefix("$VIRTUAL/") {
            (true, path)
        } else {
            (false, config_part)
        };

        let result = Self::new(org_repo, sha, config_path, is_virtual, source);
        match type_.trim_end_matches(')') {
            "default" => Ok(result),
            "gftools" => Ok(result.to_gftools_target()),
            other => Err(format!("unknown build type '{other}'")),
        }
    }
}

// this can be deleted after the new format has been used twice.
fn legacy_from_str_impl(s: &str) -> Result<Target, String> {
    // expect the format, PATH [(config)] (default|gftools)
    let (head, type_) = s
        .rsplit_once('(')
        .ok_or_else(|| "missing opening paren".to_string())?;

    let head = head.trim();

    // now we may or may not have a config:
    let (source_part, config_part) = if head.ends_with(')') {
        let (source, config) = head
            .rsplit_once('(')
            .ok_or_else(|| format!("expected '(' in '{head}'"))?;
        (source.trim(), config.trim_end_matches(')'))
    } else {
        (head, "")
    };

    let (source_part, sha_part) = source_part.rsplit_once('?').unwrap_or((source_part, ""));
    let source = PathBuf::from(source_part.trim());
    let repo: PathBuf = source.iter().take(2).collect();
    let source = source.strip_prefix(&repo).unwrap();
    let config = source.with_file_name(config_part);
    let source = source.file_name().unwrap();

    let result = Target::new(repo, sha_part, config, false, source);
    match type_.trim_end_matches(')') {
        "default" => Ok(result),
        "gftools" => Ok(result.to_gftools_target()),
        other => Err(format!("unknown build type '{other}'")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_repr_simple() {
        let target = Target::new(
            "googlefonts/derp",
            "deadbeef",
            "sources/config.yaml",
            false,
            "sources/derp.glyphs",
        );

        let asstr = target.to_string();

        assert_eq!(
            asstr,
            "googlefonts/derp/sources/config.yaml derp.glyphs?deadbeef (default)"
        );

        let der = Target::from_str(&asstr).unwrap();
        assert_eq!(target, der)
    }

    #[test]
    fn string_repr_virtual() {
        let target = Target::new(
            "googlefonts/derp",
            "deadbeef",
            "ofl/derp/config.yaml",
            true,
            "derp.glyphs",
        );

        let asstr = target.to_string();

        assert_eq!(
            asstr,
            "googlefonts/derp/$VIRTUAL/ofl/derp/config.yaml derp.glyphs?deadbeef (default)"
        );

        let der = Target::from_str(&asstr).unwrap();
        assert_eq!(target, der)
    }

    #[test]
    fn target_for_disambiguated_source() {
        let target = Target::new(
            "org/repo_123456789a",
            "123456789a",
            "Sources/hmm.yaml",
            false,
            "hello.glyphs",
        );

        let hmm = target.config_path_stripping_disambiguating_sha_if_necessary();
        assert_eq!(hmm, "~/.fontc_crater_cache/org/repo/Sources/hmm.yaml")
    }

    #[test]
    fn repro_command_with_sha() {
        let target = Target::new(
            "org/repo",
            "123456789a",
            "sources/config.yaml",
            false,
            "sources/hi.glyphs",
        );

        let hmm = target.repro_command("example.com");
        assert_eq!(
            hmm,
            "python3 resources/scripts/ttx_diff.py 'example.com?123456789a#sources/hi.glyphs'"
        );
    }
}
