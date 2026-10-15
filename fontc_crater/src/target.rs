//! targets of a compilation

use std::{
    ffi::{OsStr, OsString},
    fmt::{Display, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use serde::{Deserialize, Serialize};

/// A target at a specific revision of its source repo.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Target {
    id: TargetId,
    sha: String,
}

/// A target, whatever revision of its source repo we build.
///
/// This is what stays the same when a source repo is updated, so it is how we
/// match targets between runs.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TargetId {
    /// The source repo, as `org/name`
    repo: PathBuf,
    /// Path to the config file.
    ///
    /// - relative to the cache root if it is virtual;
    /// - relative to the repo root otherwise
    config: PathBuf,
    is_virtual: bool,
    /// Path to source file, relative to the source_dir
    source: PathBuf,
    build: BuildType,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum BuildType {
    Default,
    GfTools,
}

impl Target {
    pub(crate) fn new(
        repo: impl Into<PathBuf>,
        sha: impl Into<String>,
        config: impl Into<PathBuf>,
        is_virtual: bool,
        source: impl Into<PathBuf>,
    ) -> Self {
        let mut sha = sha.into();
        sha.truncate(10);
        Self {
            id: TargetId::new(repo, config, is_virtual, source, BuildType::Default),
            sha,
        }
    }

    pub(crate) fn to_gftools_target(&self) -> Self {
        let mut target = self.clone();
        target.id.build = BuildType::GfTools;
        target
    }

    pub(crate) fn id(&self) -> &TargetId {
        &self.id
    }

    pub(crate) fn sha(&self) -> &str {
        &self.sha
    }

    pub(crate) fn build(&self) -> BuildType {
        self.id.build
    }

    pub(crate) fn config(&self) -> &Path {
        &self.id.config
    }

    /// The repo's checkout directory, relative to the cache dir root.
    ///
    /// This matches google-fonts-sources, which checks out each revision of a
    /// repo into its own directory: `{org}/{name}_{sha}`.
    pub(crate) fn repo_dir(&self) -> PathBuf {
        let mut dir = OsString::from(self.id.repo.as_os_str());
        dir.push("_");
        dir.push(&self.sha);
        dir.into()
    }

    /// The repo name without its org (e.g. "derp" from "org/derp")
    pub(crate) fn repo_name(&self) -> &str {
        self.id.repo_name()
    }

    /// Invariant: the source path is always in a directory
    ///
    /// If the config is virtual, then the source dir is '$REPO/sources'
    /// Otherwise, it is the parent directory of the config file.
    fn source_dir(&self) -> PathBuf {
        if self.id.is_virtual {
            self.repo_dir()
        } else {
            self.repo_dir().join(self.id.config.parent().unwrap())
        }
    }

    pub(crate) fn source_path(&self, git_cache: &Path) -> PathBuf {
        let mut out = git_cache.join(self.source_dir());
        out.push(&self.id.source);
        out
    }

    pub(crate) fn config_path(&self, git_cache: &Path) -> PathBuf {
        if self.id.is_virtual {
            git_cache.join(&self.id.config)
        } else {
            git_cache.join(self.repo_dir()).join(&self.id.config)
        }
    }

    // the repro command checks the repo out without the sha suffix, so the
    // config path doesn't have one either.
    fn repro_config_path(&self) -> PathBuf {
        let git_cache = Path::new("~/.fontc_crater_cache");
        if self.id.is_virtual {
            git_cache.join(&self.id.config)
        } else {
            git_cache.join(&self.id.repo).join(&self.id.config)
        }
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
        let config = self.id.config.file_stem().unwrap_or(OsStr::new("config"));
        let mut result = in_dir.join(self.source_dir());
        result.push(config);
        // Flatten the full source path (minus extension) into a single directory
        // name by replacing path separators, to avoid cache collisions when
        // multiple sources share the same filename in different directories
        // (e.g., styles/Light/font.ufo vs styles/Medium/font.ufo).
        let source_no_ext = self
            .id
            .source
            .with_extension("")
            .display()
            .to_string()
            .replace('/', "_");
        result.push(source_no_ext);
        result.push(self.id.build.name());
        result
    }

    pub(crate) fn repro_command(&self, repo_url: &str) -> String {
        let repo_url = repo_url.trim();
        let source_path = self.source_path(Path::new(""));
        let rel_source_path = source_path
            .strip_prefix(self.repo_dir())
            .expect("source always in repo");
        let sha_part = if !self.sha.is_empty() {
            format!("?{}", self.sha)
        } else {
            Default::default()
        };
        let mut cmd = format!(
            "python3 -m ttx_diff '{repo_url}{sha_part}#{}'",
            rel_source_path.display()
        );
        if self.id.build == BuildType::GfTools {
            cmd.push_str(" --compare gftools");
            // we hard code this; repro will only work if they're using default
            // cache location
            write!(&mut cmd, " --config {}", self.repro_config_path().display()).unwrap();
        }
        cmd
    }
}

impl TargetId {
    fn new(
        repo: impl Into<PathBuf>,
        config: impl Into<PathBuf>,
        is_virtual: bool,
        source: impl Into<PathBuf>,
        build: BuildType,
    ) -> Self {
        let config = config.into();
        let source = source.into();
        let config_dir = config.parent().unwrap();
        // if source is a sibling of config we can trim that bit.
        let source = source
            .strip_prefix(config_dir)
            .map(PathBuf::from)
            .unwrap_or(source);
        Self {
            repo: repo.into(),
            config,
            is_virtual,
            source,
            build,
        }
    }

    /// The repo name without its org (e.g. "derp" from "org/derp")
    pub(crate) fn repo_name(&self) -> &str {
        self.repo
            .file_name()
            .and_then(OsStr::to_str)
            .unwrap_or_default()
    }

    /// The `org/name` of the source repo.
    pub(crate) fn repo(&self) -> &Path {
        &self.repo
    }

    fn fmt_with_sha(&self, f: &mut std::fmt::Formatter<'_>, sha: Option<&str>) -> std::fmt::Result {
        let config_path = if self.is_virtual {
            self.repo.join("$VIRTUAL").join(&self.config)
        } else {
            self.repo.join(&self.config)
        };
        write!(f, "{} {}", config_path.display(), self.source.display())?;
        if let Some(sha) = sha {
            write!(f, "?{sha}")?;
        }
        write!(f, " ({})", self.build)
    }

    /// Parse a target, and its sha if there is one.
    ///
    /// See the `FromStr` impl for [`Target`] for the format.
    fn parse(s: &str) -> Result<(Self, Option<&str>), String> {
        let s = s.trim();
        let (head, type_) = s
            .rsplit_once('(')
            .ok_or_else(|| "missing opening paren".to_string())?;

        let head = head.trim();
        let (config_part, source_part) = head
            .split_once(' ')
            .ok_or_else(|| "missing a space?".to_string())?;
        let (source, sha) = match source_part.trim().split_once('?') {
            Some((source, sha)) => (source, Some(sha)),
            None => (source_part.trim(), None),
        };
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

        let build = match type_.trim_end_matches(')') {
            "default" => BuildType::Default,
            "gftools" => BuildType::GfTools,
            other => return Err(format!("unknown build type '{other}'")),
        };
        let id = Self::new(org_repo, config_path, is_virtual, source, build);
        Ok((id, sha))
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
        self.id.fmt_with_sha(f, Some(&self.sha))
    }
}

impl Display for TargetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_sha(f, None)
    }
}

/// in the format,
///
/// $ORG/$REPO/$CONFIG_PATH $SRC_PATH?$SHA ($BUILD_TYPE)
///
/// where a virtual config's $CONFIG_PATH starts with the literal path element
/// '$VIRTUAL'.
impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (id, sha) = TargetId::parse(s)?;
        let mut sha = sha.ok_or_else(|| "missing '?'".to_string())?.to_owned();
        sha.truncate(10);
        Ok(Self { id, sha })
    }
}

/// The same format as [`Target`], where the `?$SHA` is optional and ignored.
impl FromStr for TargetId {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TargetId::parse(s).map(|(id, _)| id)
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

impl Serialize for TargetId {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for TargetId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(s).map_err(serde::de::Error::custom)
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
    fn id_ignores_sha() {
        let old = "googlefonts/derp/sources/config.yaml derp.glyphs?deadbeef (default)";
        let new = "googlefonts/derp/sources/config.yaml derp.glyphs?c0ffee (default)";
        let gftools = "googlefonts/derp/sources/config.yaml derp.glyphs?c0ffee (gftools)";
        let [old, new, gftools] = [old, new, gftools].map(|s| Target::from_str(s).unwrap());

        assert_ne!(old, new);
        assert_eq!(old.id(), new.id());
        assert_ne!(new.id(), gftools.id());

        let id = "googlefonts/derp/sources/config.yaml derp.glyphs (default)";
        assert_eq!(id, old.id().to_string());
        assert_eq!(&TargetId::from_str(id).unwrap(), old.id());
        assert!(Target::from_str(id).is_err());
    }

    #[test]
    fn repo_dir_has_sha() {
        let target = Target::new(
            "org/repo",
            "123456789abc",
            "Sources/hmm.yaml",
            false,
            "hello.glyphs",
        );
        assert_eq!(target.repo_dir(), Path::new("org/repo_123456789a"));
        assert_eq!(target.repo_name(), "repo");
        assert_eq!(
            target.cache_dir(Path::new("cache")),
            Path::new("cache/org/repo_123456789a/Sources/hmm/hello/default")
        );
        assert_eq!(
            target.repro_config_path(),
            Path::new("~/.fontc_crater_cache/org/repo/Sources/hmm.yaml")
        );
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
            "python3 -m ttx_diff 'example.com?123456789a#sources/hi.glyphs'"
        );
    }
}
