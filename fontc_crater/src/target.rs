//! targets of a compilation

use std::{
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
    /// Filename of config file, in the source directory.
    config: Option<PathBuf>,
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
            Some(_) => {
                if !result.pop() {
                    break;
                }
            }
            None => break,
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
        build: BuildType,
    ) -> Result<Self, InvalidTargetPath> {
        let source_dir = get_source_dir(&source)?;
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

    pub(crate) fn repro_command(&self, repo_url: &str) -> String {
        let repo_url = repo_url.trim();
        let just_source_dir = self.source_dir.file_name().unwrap();
        let rel_source_path = Path::new(just_source_dir).join(&self.source);
        let mut cmd = format!(
            "python resources/scripts/ttx_diff.py {repo_url}#{}",
            rel_source_path.display()
        );
        if self.build == BuildType::GfTools {
            cmd.push_str(" --compare gftools");
            // we hard code this; repro will only work if they're using default
            // cache location
            if let Some(config) = self.config_path(Path::new("~/.fontc_crater_cache")) {
                write!(&mut cmd, " --config {}", config.display()).unwrap();
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
            return Self::new(s.into(), None, BuildType::Default)
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

        let source = PathBuf::from(source_part.trim());
        let config = Some(config_part)
            .filter(|s| !s.is_empty())
            .map(PathBuf::from);

        let type_ = match type_.trim_end_matches(')') {
            "default" => BuildType::Default,
            "gftools" => BuildType::GfTools,
            other => return Err(format!("unknown build type '{other}'")),
        };

        Self::new(source, config, type_).map_err(|e| format!("failed to parse target '{s}': {e}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construct_target_with_source() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let target = Target::new(source, None, BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources");
        assert_eq!(target.source.as_os_str(), "Mysource.glyphs");
    }

    #[test]
    fn invalid_source_path() {
        let source = PathBuf::from("org/repo/not_sources/Mysource.glyphs");
        let target = Target::new(source, None, BuildType::Default);
        assert!(matches!(target, Err(e) if e.reason == BadPathReason::NoSourceDir));
    }

    #[test]
    fn good_config_path() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("org/repo/sources/my-config.yaml");
        let target = Target::new(source, Some(config), BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources");
        assert_eq!(target.source.as_os_str(), "Mysource.glyphs");
        assert_eq!(target.config.as_deref(), Some(Path::new("my-config.yaml")));
    }

    #[test]
    fn bare_config_path() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("my-config.yaml");
        let target = Target::new(source, Some(config), BuildType::Default).unwrap();
        assert_eq!(target.source_dir.as_os_str(), "org/repo/sources");
        assert_eq!(target.source.as_os_str(), "Mysource.glyphs");
        assert_eq!(target.config.as_deref(), Some(Path::new("my-config.yaml")));
    }

    #[test]
    fn bad_config_path() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("somewhere_else/my-config.yaml");
        let target = Target::new(source, Some(config), BuildType::Default);
        assert!(matches!(target, Err(e) if e.reason == BadPathReason::BadConfigPath));
    }

    #[test]
    fn serde_target_full() {
        let source = PathBuf::from("org/repo/sources/Mysource.glyphs");
        let config = PathBuf::from("config.yaml");
        let target = Target::new(source, Some(config), BuildType::Default).unwrap();

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
