//! targets of a compilation

use std::{fmt::Display, path::PathBuf, str::FromStr};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
pub(crate) struct Target {
    // will be used in gftools mode
    pub(crate) config: PathBuf,
    pub(crate) source: PathBuf,
    pub(crate) build: BuildType,
}

/// Uniquely identify a source + build type (default, gftools)
///
/// this is separate from 'target' because it doesn't preserve the config path.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TargetId {
    pub(crate) path: PathBuf,
    pub(crate) build: BuildType,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum BuildType {
    Default,
    GfTools,
}

impl Target {
    pub(crate) fn id(&self) -> TargetId {
        TargetId {
            path: self.source.clone(),
            build: self.build,
        }
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id().fmt(f)
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

impl Display for TargetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.path.display(), self.build)
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

impl FromStr for TargetId {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if !s.ends_with(')') {
            return Ok(Self {
                path: PathBuf::from(s),
                build: BuildType::Default,
            });
        }
        // else expect the format,
        // PATH (default|gftools)
        let (path, type_) = s
            .rsplit_once('(')
            .ok_or_else(|| "missing opening paren".to_string())?;

        let path = PathBuf::from(path.trim());
        let type_ = match type_.trim_end_matches(')') {
            "default" => BuildType::Default,
            "gftools" => BuildType::GfTools,
            other => return Err(format!("unknown build type '{other}'")),
        };

        Ok(TargetId { path, build: type_ })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serde_target_id() {
        let id = TargetId {
            path: PathBuf::from("../my/file.is_here"),
            build: BuildType::GfTools,
        };

        let to_json = serde_json::to_string(&id).unwrap();
        let from_json: TargetId = serde_json::from_str(&to_json).unwrap();
        assert_eq!(id, from_json)
    }
}
