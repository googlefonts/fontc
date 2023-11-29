use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use chrono::{DateTime, Utc};
use filetime::FileTime;
use fontdrasil::{coords::NormalizedLocation, types::Axis};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use write_fonts::{tables::os2::SelectionFlags, types::Tag};

use crate::{
    ir::{
        GlobalMetric, GlobalMetrics, Glyph, GlyphBuilder, GlyphInstance, GlyphOrder, MiscMetadata,
        NameKey, NamedInstance, PostscriptNames, StaticMetadata,
    },
    stateset::{FileState, MemoryState, State, StateIdentifier, StateSet},
};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct StaticMetadataSerdeRepr {
    pub units_per_em: u16,
    pub axes: Vec<Axis>,
    pub named_instances: Vec<NamedInstance>,
    pub glyph_locations: Vec<NormalizedLocation>,
    pub names: HashMap<NameKey, String>,
    pub postscript_names: PostscriptNames,
    pub misc: MiscSerdeRepr,
}

impl From<StaticMetadataSerdeRepr> for StaticMetadata {
    fn from(from: StaticMetadataSerdeRepr) -> Self {
        let mut static_metadata = StaticMetadata::new(
            from.units_per_em,
            from.names,
            from.axes,
            from.named_instances,
            from.glyph_locations.into_iter().collect(),
            from.postscript_names,
        )
        .unwrap();
        static_metadata.misc = from.misc.into();
        static_metadata
    }
}

impl From<StaticMetadata> for StaticMetadataSerdeRepr {
    fn from(from: StaticMetadata) -> Self {
        let glyph_locations = from.variation_model.locations().cloned().collect();
        StaticMetadataSerdeRepr {
            units_per_em: from.units_per_em,
            axes: from.all_source_axes,
            named_instances: from.named_instances,
            glyph_locations,
            names: from.names,
            postscript_names: from.postscript_names,
            misc: from.misc.into(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct GlyphOrderSerdeRepr(Vec<String>);

impl From<GlyphOrderSerdeRepr> for GlyphOrder {
    fn from(value: GlyphOrderSerdeRepr) -> Self {
        value.0.into_iter().map(|v| v.into()).collect()
    }
}

impl From<GlyphOrder> for GlyphOrderSerdeRepr {
    fn from(value: GlyphOrder) -> Self {
        GlyphOrderSerdeRepr(value.into_iter().map(|v| v.to_string()).collect())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct MiscSerdeRepr {
    pub fs_type: Option<u16>,
    pub selection_flags: u16,
    pub vendor_id: Tag,
    pub underline_thickness: f32,
    pub underline_position: f32,
    pub version_major: i32,
    pub version_minor: u32,
    pub head_flags: u16,
    pub lowest_rec_ppm: u16,
    pub created: Option<DateTime<Utc>>,
}

impl From<MiscSerdeRepr> for MiscMetadata {
    fn from(from: MiscSerdeRepr) -> Self {
        MiscMetadata {
            fs_type: from.fs_type,
            selection_flags: SelectionFlags::from_bits_truncate(from.selection_flags),
            vendor_id: from.vendor_id,
            underline_thickness: from.underline_thickness.into(),
            underline_position: from.underline_position.into(),
            version_major: from.version_major,
            version_minor: from.version_minor,
            head_flags: from.head_flags,
            lowest_rec_ppm: from.lowest_rec_ppm,
            created: from.created,
        }
    }
}

impl From<MiscMetadata> for MiscSerdeRepr {
    fn from(from: MiscMetadata) -> Self {
        MiscSerdeRepr {
            fs_type: from.fs_type,
            selection_flags: from.selection_flags.bits(),
            vendor_id: from.vendor_id,
            underline_thickness: from.underline_thickness.into(),
            underline_position: from.underline_position.into(),
            version_major: from.version_major,
            version_minor: from.version_minor,
            head_flags: from.head_flags,
            lowest_rec_ppm: from.lowest_rec_ppm,
            created: from.created,
        }
    }
}

// The metric maps of HashMap<NormalizedLocation, OrderedFloat> seems to throw serde for a loop
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct GlobalMetricsSerdeRepr(
    Vec<(GlobalMetric, NormalizedLocation, OrderedFloat<f32>)>,
);

impl From<GlobalMetricsSerdeRepr> for GlobalMetrics {
    fn from(from: GlobalMetricsSerdeRepr) -> Self {
        GlobalMetrics(from.0.into_iter().fold(
            HashMap::<GlobalMetric, HashMap<NormalizedLocation, OrderedFloat<f32>>>::new(),
            |mut acc, (metric, pos, value)| {
                acc.entry(metric).or_default().insert(pos, value);
                acc
            },
        ))
    }
}

impl From<GlobalMetrics> for GlobalMetricsSerdeRepr {
    fn from(from: GlobalMetrics) -> Self {
        GlobalMetricsSerdeRepr(
            from.0
                .into_iter()
                .flat_map(|(metric, values)| {
                    values
                        .into_iter()
                        .map(move |(pos, value)| (metric, pos, value))
                })
                .collect(),
        )
    }
}

// The HashMap<NormalizedLocation, GlyphInstance> seems to throw serde for a loop sometimes
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct GlyphInstanceSerdeRepr {
    location: NormalizedLocation,
    instance: GlyphInstance,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct GlyphSerdeRepr {
    pub name: String,
    pub emit_to_binary: bool,
    pub codepoints: HashSet<u32>,
    pub instances: Vec<GlyphInstanceSerdeRepr>,
}

impl From<GlyphSerdeRepr> for Glyph {
    fn from(from: GlyphSerdeRepr) -> Self {
        GlyphBuilder {
            name: from.name.into(),
            emit_to_binary: from.emit_to_binary,
            codepoints: from.codepoints,
            sources: from
                .instances
                .into_iter()
                .map(|g| (g.location, g.instance))
                .collect(),
        }
        .build()
        .unwrap()
    }
}

impl From<Glyph> for GlyphSerdeRepr {
    fn from(from: Glyph) -> Self {
        let from: GlyphBuilder = from.into();
        GlyphSerdeRepr {
            name: from.name.as_str().to_string(),
            emit_to_binary: from.emit_to_binary,
            codepoints: from.codepoints,
            instances: from
                .sources
                .into_iter()
                .map(|(loc, inst)| GlyphInstanceSerdeRepr {
                    location: loc,
                    instance: inst,
                })
                .collect(),
        }
    }
}

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
