//! targets of a compilation

use std::{fmt::Display, path::PathBuf, str::FromStr};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Target {
    /// Filename of config file, always a sibling of source.
    pub(crate) config: Option<PathBuf>,
    /// Path to source, relative to the git cache directory.
    pub(crate) source: PathBuf,
    pub(crate) build: BuildType,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum BuildType {
    Default,
    GfTools,
}

impl Target {
    pub(crate) fn new(
        source: PathBuf,
        config: impl Into<Option<PathBuf>>,
        build: BuildType,
    ) -> Self {
        Self {
            config: config.into(),
            source,
            build,
        }
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
        write!(f, "{}", self.source.display(),)?;
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
            return Ok(Self {
                source: PathBuf::from(s),
                config: None,
                build: BuildType::Default,
            });
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

        Ok(Target {
            source,
            config,
            build: type_,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serde_target_full() {
        let id = Target {
            source: PathBuf::from("../my/file.is_here"),
            config: Some("config.yaml".into()),
            build: BuildType::GfTools,
        };

        let to_json = serde_json::to_string(&id).unwrap();
        let from_json: Target = serde_json::from_str(&to_json).unwrap();
        assert_eq!(id, from_json)
    }

    #[test]
    fn serde_no_config() {
        let json = "\"myfile.is_here (gftools)\"";
        let from_json: Target = serde_json::from_str(json).unwrap();
        assert_eq!(from_json.source.as_os_str(), "myfile.is_here");
        assert!(from_json.build == BuildType::GfTools);
    }

    #[test]
    fn serde_path_only() {
        let json = "\"mypath.hello\"";
        let from_json: Target = serde_json::from_str(json).unwrap();
        assert_eq!(from_json.source.as_os_str(), "mypath.hello");
        assert_eq!(from_json.build, BuildType::Default);
    }
}
