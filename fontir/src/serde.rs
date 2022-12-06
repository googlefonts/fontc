use std::{collections::HashMap, path::PathBuf};

use filetime::FileTime;
use serde::{Deserialize, Serialize};

use crate::stateset::{FileState, MemoryState, State, StateIdentifier, StateSet};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct StateSetSerdeRepr {
    files: Vec<FileStateSerdeRepr>,
    slices: Vec<SliceStateSerdeRepr>,
}

impl From<StateSetSerdeRepr> for StateSet {
    fn from(from: StateSetSerdeRepr) -> Self {
        let entries: HashMap<StateIdentifier, State> = from
            .files
            .into_iter()
            .map(|serde_repr| {
                (
                    StateIdentifier::File(PathBuf::from(&serde_repr.path)),
                    State::File(FileState {
                        mtime: FileTime::from_unix_time(serde_repr.unix_seconds, serde_repr.nanos),
                        size: serde_repr.size,
                    }),
                )
            })
            .chain(from.slices.into_iter().map(|serde_repr| {
                (
                    StateIdentifier::Memory(serde_repr.identifier),
                    State::Memory(MemoryState {
                        hash: blake3::Hash::from_hex(serde_repr.hash).unwrap(),
                        size: serde_repr.size,
                    }),
                )
            }))
            .collect();
        StateSet { entries }
    }
}

impl From<StateSet> for StateSetSerdeRepr {
    fn from(fs: StateSet) -> Self {
        let mut files = Vec::new();
        let mut slices = Vec::new();

        for (key, state) in fs.entries {
            match state {
                State::File(state) => {
                    let StateIdentifier::File(path) = key else {
                        panic!("A file state *must* use a file key");
                    };
                    files.push(FileStateSerdeRepr {
                        path: path.to_str().expect("Only UTF names please").to_string(),
                        unix_seconds: state.mtime.unix_seconds(),
                        nanos: state.mtime.nanoseconds(),
                        size: state.size,
                    });
                }
                State::Memory(state) => {
                    let StateIdentifier::Memory(identifier) = key else {
                        panic!("A file state *must* use a file key");
                    };
                    slices.push(SliceStateSerdeRepr {
                        identifier,
                        hash: state.hash.to_hex().to_string(),
                        size: state.size,
                    });
                }
            }
        }
        files.sort_by(|e1, e2| e1.path.cmp(&e2.path));
        slices.sort_by(|e1, e2| e1.identifier.cmp(&e2.identifier));
        StateSetSerdeRepr { files, slices }
    }
}

/// The serde-friendly representation of a [FileState].
///
/// SystemTime lacks a platform independent representation we can
/// depend on so use FileTime's unix_seconds,nanos.
#[derive(Serialize, Deserialize, Debug, Clone)]
struct FileStateSerdeRepr {
    path: String,
    unix_seconds: i64,
    nanos: u32,
    size: u64,
}

/// The serde-friendly representation of a [MemoryState].
///
/// SystemTime lacks a platform independent representation we can
/// depend on so use FileTime's unix_seconds,nanos.
#[derive(Serialize, Deserialize, Debug, Clone)]
struct SliceStateSerdeRepr {
    identifier: String,
    hash: String,
    size: u64,
}
