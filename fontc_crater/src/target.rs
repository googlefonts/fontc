//! targets of a compilation

use std::{
    ffi::OsStr,
    fmt::{Display, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Target {
    /// path to the source dir for this target (relative to the git cache root)
    source_dir: PathBuf,
    sha: String,
    /// Filename of config file, in the source directory.
    pub(crate) config: Option<PathBuf>,
    /// Path to source file, relative to the source_dir
    source: PathBuf,
    pub(crate) build: BuildType,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum BuildType {
    Default,
    GfTools,
}

fn get_source_dir(source_path: &Path) -> Result<PathBuf, InvalidTargetPath> {
    let mut result = source_path.to_owned();
    loop {
        match result.file_name() {
            Some(name) if name == "sources" || name == "Sources" => return Ok(result),
            Some(_) | None => {
                if !result.pop() {
                    break;
                }
            }
        }
    }
    Err(InvalidTargetPath {
        path: source_path.to_owned(),
        reason: BadPathReason::NoSourceDir,
    })
}

/// A source path must always be a file in side a directory named 'sources' or 'Sources'
#[derive(Clone, Debug, Error)]
#[error("invalid path '{path}' for target: {reason}")]
pub(crate) struct InvalidTargetPath {
    path: PathBuf,
    reason: BadPathReason,
}

#[derive(Clone, Debug, PartialEq)]
enum BadPathReason {
    NoSourceDir,
    BadConfigPath,
}

impl Display for BadPathReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BadPathReason::NoSourceDir => f.write_str("missing sources/Sources directory"),
            BadPathReason::BadConfigPath => f.write_str("config is not relative to source"),
        }
    }
}

impl Target {
    pub(crate) fn new(
        source: PathBuf,
        config: impl Into<Option<PathBuf>>,
        mut sha: String,
        build: BuildType,
    ) -> Result<Self, InvalidTargetPath> {
        let source_dir = get_source_dir(&source)?;
        sha.truncate(10);
        let source = source
            .strip_prefix(&source_dir)
            .expect("must be a base")
            .to_owned();
        let config = match config.into() {
            // if we're just a filename, that's fine; assume relative to source
            Some(config)
                if config.file_name().is_some() && config.parent() == Some(Path::new("")) =>
            {
                Some(config)
            }
            // else we have to be in the same source directory as our source
            Some(config) => Some(
                config
                    .strip_prefix(&source_dir)
                    .map(PathBuf::from)
                    .map_err(|_| InvalidTargetPath {
                        path: config,
                        reason: BadPathReason::BadConfigPath,
                    })?,
            ),
            None => None,
        };

        Ok(Self {
            source_dir,
            config,
            source,
            build,
            sha,
        })
    }

    /// The org/repo part of the path, used for looking up repo urls
    pub(crate) fn repo_path(&self) -> &Path {
        self.source_dir.parent().unwrap()
    }

    pub(crate) fn source_path(&self, git_cache: &Path) -> PathBuf {
        let mut out = git_cache.join(&self.source_dir);
        out.push(&self.source);
        out
    }

    pub(crate) fn config_path(&self, git_cache: &Path) -> Option<PathBuf> {
        let config = self.config.as_ref()?;
        let mut out = git_cache.join(&self.source_dir);
        out.push(config);
        Some(out)
    }

    // if a target was built in a directory with a sha, the repro command
    // does not need to include that part of the directory, so remove it.
    fn config_path_stripping_disambiguating_sha_if_necessary(&self) -> Option<String> {
        let mut path = self
            .config_path(Path::new("~/.fontc_crater_cache"))?
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
        Some(path)
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
        let config = self
            .config
            .as_ref()
            .and_then(|p| p.file_stem())
            .unwrap_or(OsStr::new("config"));
        let mut result = in_dir.join(&self.source_dir);
        result.push(config);
        result.push(self.source.file_stem().unwrap());
        result.push(self.build.name());
        result
    }

    pub(crate) fn repro_command(&self, repo_url: &str) -> String {
        let repo_url = repo_url.trim();
        let just_source_dir = self.source_dir.file_name().unwrap();
        let rel_source_path = Path::new(just_source_dir).join(&self.source);
        let sha_part = if !self.sha.is_empty() {
            format!("?{}", self.sha)
        } else {
            Default::default()
        };
        let mut cmd = format!(
            "python resources/scripts/ttx_diff.py '{repo_url}{sha_part}#{}'",
            rel_source_path.display()
        );
        if self.build == BuildType::GfTools {
            cmd.push_str(" --compare gftools");
            // we hard code this; repro will only work if they're using default
            // cache location
            if let Some(config) = self.config_path_stripping_disambiguating_sha_if_necessary() {
                write!(&mut cmd, " --config {config}").unwrap();
            }
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
        let source = self.source_dir.join(&self.source);
        write!(f, "{}", source.display())?;
        if !self.sha.is_empty() {
            write!(f, "?{}", self.sha)?;
        }
        if let Some(config) = self.config.as_ref() {
            write!(f, " ({})", config.display())?
        }
        write!(f, " ({})", self.build)
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

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        // before gftools we just identified targets as paths, so let's keep that working
        if !s.ends_with(')') {
            return Self::new(s.into(), None, Default::default(), BuildType::Default)
                .map_err(|e| format!("failed to parse target '{s}': {e}"));
        }
        // else expect the format,
        // PATH [(config)] (default|gftools)
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
        let config = Some(config_part)
            .filter(|s| !s.is_empty())
            .map(PathBuf::from);

        let type_ = match type_.trim_end_matches(')') {
            "default" => BuildType::Default,
            "gftools" => BuildType::GfTools,
            other => return Err(format!("unknown build type '{other}'")),
        };

        Self::new(source, config, sha_part.to_owned(), type_)
            .map_err(|e| format!("failed to parse target '{s}': {e}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construct_target_with_source() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let target = Target::new(source, None, "".to_owned(), BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources");
        assert_eq!(target.source.as_os_str(), "Mysource.glyphs");
    }

    #[test]
    fn invalid_source_path() {
        let source = PathBuf::from("org/repo/not_sources/Mysource.glyphs");
        let target = Target::new(source, None, "".to_owned(), BuildType::Default);
        assert!(matches!(target, Err(e) if e.reason == BadPathReason::NoSourceDir));
    }

    #[test]
    fn relative_source_path() {
        let relpath = PathBuf::from("org/repo/sources/../source/dir/Mysource.designspace");

        let target = Target::new(relpath, None, "".to_owned(), BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources",);
        assert_eq!(
            target.source.as_os_str(),
            "../source/dir/Mysource.designspace"
        );
    }

    #[test]
    fn good_config_path() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("org/repo/sources/my-config.yaml");
        let target = Target::new(source, Some(config), "".into(), BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources");
        assert_eq!(target.source.as_os_str(), "Mysource.glyphs");
        assert_eq!(target.config.as_deref(), Some(Path::new("my-config.yaml")));
    }

    #[test]
    fn bare_config_path() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("my-config.yaml");
        let target = Target::new(source, Some(config), "".into(), BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources");
        assert_eq!(target.source.as_os_str(), "Mysource.glyphs");
        assert_eq!(target.config.as_deref(), Some(Path::new("my-config.yaml")));
    }

    #[test]
    fn bad_config_path() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("somewhere_else/my-config.yaml");
        let target = Target::new(source, Some(config), "".into(), BuildType::Default);
        assert!(matches!(target, Err(e) if e.reason == BadPathReason::BadConfigPath));
    }

    #[test]
    fn target_for_disambiguated_source() {
        let target = Target::new(
            "org/repo_123456789a/sources/hi.glyphs".into(),
            Some("config.yaml".into()),
            "123456789a".into(),
            BuildType::Default,
        )
        .unwrap();

        let hmm = target
            .config_path_stripping_disambiguating_sha_if_necessary()
            .unwrap();
        assert_eq!(hmm, "~/.fontc_crater_cache/org/repo/sources/config.yaml")
    }

    #[test]
    fn repro_command_with_sha() {
        let target = Target::new(
            "org/repo/sources/hi.glyphs".into(),
            Some("config.yaml".into()),
            "123456789a".into(),
            BuildType::Default,
        )
        .unwrap();

        let hmm = target.repro_command("example.com");
        assert_eq!(
            hmm,
            "python resources/scripts/ttx_diff.py 'example.com?123456789a#sources/hi.glyphs'"
        );
    }

    #[test]
    fn serde_target_full() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("config.yaml");
        let target = Target::new(
            source,
            Some(config),
            "abc123def456".into(),
            BuildType::Default,
        )
        .unwrap();

        let to_json = serde_json::to_string(&target).unwrap();
        let from_json: Target = serde_json::from_str(&to_json).unwrap();
        assert_eq!(target, from_json)
    }

    #[test]
    fn serde_no_config() {
        let json = "\"org/repo/sources/myfile.is_here (gftools)\"";
        let from_json: Target = serde_json::from_str(json).unwrap();
        assert_eq!(from_json.source.as_os_str(), "myfile.is_here");
        assert!(from_json.config.is_none());
        assert!(from_json.build == BuildType::GfTools);
    }

    #[test]
    fn serde_path_only() {
        let json = "\"repo/Sources/mypath.hello\"";
        let from_json: Target = serde_json::from_str(json).unwrap();
        assert_eq!(from_json.source.as_os_str(), "mypath.hello");
        assert_eq!(from_json.build, BuildType::Default);
    }
}
