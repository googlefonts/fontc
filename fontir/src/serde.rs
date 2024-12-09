use std::{collections::HashSet, path::PathBuf};

use serde::{Deserialize, Serialize};

use crate::stateset::{PartialSource, PartialSourceIdentifier};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct PartialSourceSerdeRepr {
    files: Vec<String>,
    slices: Vec<String>,
}

impl From<PartialSourceSerdeRepr> for PartialSource {
    fn from(from: PartialSourceSerdeRepr) -> Self {
        let entries: HashSet<PartialSourceIdentifier> = from
            .files
            .into_iter()
            .map(|serde_repr| PartialSourceIdentifier::File(PathBuf::from(&serde_repr)))
            .chain(
                from.slices
                    .into_iter()
                    .map(|serde_repr| PartialSourceIdentifier::Memory(serde_repr)),
            )
            .collect();
        PartialSource { entries }
    }
}

impl From<PartialSource> for PartialSourceSerdeRepr {
    fn from(fs: PartialSource) -> Self {
        let mut files = Vec::new();
        let mut slices = Vec::new();

        for partial_source_id in fs.entries {
            match partial_source_id {
                PartialSourceIdentifier::File(path) => {
                    files.push(path.to_str().expect("Only UTF names please").to_string())
                }
                PartialSourceIdentifier::Memory(identifier) => slices.push(identifier),
            }
        }
        files.sort();
        slices.sort();
        PartialSourceSerdeRepr { files, slices }
    }
}
