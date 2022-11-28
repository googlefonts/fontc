use std::path::PathBuf;

use filetime::FileTime;
use serde::{Deserialize, Serialize};

use crate::filestate::{FileState, FileStateSet};

// We use a named field because toml doesn't like tuple-structs very much
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct FileStateSetSerdeRepr {
    entries: Vec<PathStateSerdeRepr>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct FileStateSerdeRepr {
    path: PathBuf,
    depends_on: Vec<PathBuf>,
}

impl From<FileStateSetSerdeRepr> for FileStateSet {
    fn from(from: FileStateSetSerdeRepr) -> Self {
        FileStateSet {
            entries: from
                .entries
                .iter()
                .map(|d| {
                    (
                        PathBuf::from(&d.path),
                        FileState {
                            mtime: FileTime::from_unix_time(d.unix_seconds, d.nanos),
                            size: d.size,
                        },
                    )
                })
                .collect(),
        }
    }
}

impl From<FileStateSet> for FileStateSetSerdeRepr {
    fn from(fs: FileStateSet) -> Self {
        let mut entries: Vec<PathStateSerdeRepr> = fs
            .entries
            .iter()
            .map(|e| {
                let path = e.0.to_str().expect("Only UTF names please").to_string();
                PathStateSerdeRepr {
                    path,
                    unix_seconds: e.1.mtime.unix_seconds(),
                    nanos: e.1.mtime.nanoseconds(),
                    size: e.1.size,
                }
            })
            .collect();
        entries.sort_by(|e1, e2| e1.path.cmp(&e2.path));
        FileStateSetSerdeRepr { entries }
    }
}

/// The serde-friendly representation of a [FileState] for a [Path].
///
/// SystemTime lacks a platform independent representation we can
/// depend on so use FileTime's unix_seconds,nanos.
#[derive(Serialize, Deserialize, Debug, Clone)]
struct PathStateSerdeRepr {
    path: String,
    unix_seconds: i64,
    nanos: u32,
    size: u64,
}
