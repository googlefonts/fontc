use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use chrono::{DateTime, Utc};
use filetime::FileTime;
use font_types::Tag;
use fontdrasil::{axis::Axis, coords::NormalizedLocation};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use write_fonts::tables::os2::SelectionFlags;

use crate::{
    ir::{
        GlobalMetric, GlobalMetrics, Glyph, GlyphBuilder, GlyphInstance, KernParticipant, Kerning,
        MiscMetadata, NameKey, NamedInstance, StaticMetadata,
    },
    stateset::{FileState, MemoryState, State, StateIdentifier, StateSet},
};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct StaticMetadataSerdeRepr {
    pub units_per_em: u16,
    pub axes: Vec<Axis>,
    pub named_instances: Vec<NamedInstance>,
    pub glyph_locations: Vec<NormalizedLocation>,
    pub names: HashMap<NameKey, String>,
    pub misc: MiscSerdeRepr,
    pub glyph_order: Vec<String>,
}

impl From<StaticMetadataSerdeRepr> for StaticMetadata {
    fn from(from: StaticMetadataSerdeRepr) -> Self {
        StaticMetadata::new(
            from.units_per_em,
            from.names,
            from.axes,
            from.named_instances,
            from.glyph_order.into_iter().map(|s| s.into()).collect(),
            from.glyph_locations.into_iter().collect(),
        )
        .unwrap()
    }
}

impl From<StaticMetadata> for StaticMetadataSerdeRepr {
    fn from(from: StaticMetadata) -> Self {
        let glyph_locations = from.variation_model.locations().cloned().collect();
        StaticMetadataSerdeRepr {
            units_per_em: from.units_per_em,
            axes: from.axes,
            named_instances: from.named_instances,
            glyph_locations,
            names: from.names,
            misc: from.misc.into(),
            glyph_order: from
                .glyph_order
                .into_iter()
                .map(|n| n.as_str().to_string())
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct KerningSerdeRepr {
    pub groups: Vec<KerningGroupSerdeRepr>,
    pub kerns: Vec<KernSerdeRepr>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct KerningGroupSerdeRepr {
    name: String,
    glyphs: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct KernSerdeRepr {
    side1: KernParticipant,
    side2: KernParticipant,
    values: Vec<(NormalizedLocation, f32)>,
}

impl From<Kerning> for KerningSerdeRepr {
    fn from(from: Kerning) -> Self {
        KerningSerdeRepr {
            groups: from
                .groups
                .into_iter()
                .map(|(name, glyphs)| {
                    let name = name.to_string();
                    let mut glyphs: Vec<_> = glyphs.iter().map(|g| g.to_string()).collect();
                    glyphs.sort();
                    KerningGroupSerdeRepr { name, glyphs }
                })
                .collect(),
            kerns: from
                .kerns
                .into_iter()
                .map(|((side1, side2), values)| {
                    let mut values: Vec<_> = values
                        .into_iter()
                        .map(|(pos, adjustment)| (pos, adjustment.0))
                        .collect();
                    values.sort_by_key(|(pos, _)| pos.clone());
                    KernSerdeRepr {
                        side1,
                        side2,
                        values,
                    }
                })
                .collect(),
        }
    }
}

impl From<KerningSerdeRepr> for Kerning {
    fn from(from: KerningSerdeRepr) -> Self {
        Kerning {
            groups: from
                .groups
                .into_iter()
                .map(|g| {
                    (
                        g.name.into(),
                        g.glyphs.into_iter().map(|n| n.into()).collect(),
                    )
                })
                .collect(),
            kerns: from
                .kerns
                .into_iter()
                .map(|k| {
                    (
                        (k.side1, k.side2),
                        k.values
                            .into_iter()
                            .map(|(pos, value)| (pos, value.into()))
                            .collect(),
                    )
                })
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MiscSerdeRepr {
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
pub struct GlobalMetricsSerdeRepr(Vec<(GlobalMetric, NormalizedLocation, OrderedFloat<f32>)>);

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
pub struct GlyphInstanceSerdeRepr {
    location: NormalizedLocation,
    instance: GlyphInstance,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct GlyphSerdeRepr {
    pub name: String,
    pub codepoints: HashSet<u32>,
    pub instances: Vec<GlyphInstanceSerdeRepr>,
}

impl From<GlyphSerdeRepr> for Glyph {
    fn from(from: GlyphSerdeRepr) -> Self {
        GlyphBuilder {
            name: from.name.into(),
            codepoints: from.codepoints,
            sources: from
                .instances
                .into_iter()
                .map(|g| (g.location, g.instance))
                .collect(),
        }
        .try_into()
        .unwrap()
    }
}

impl From<Glyph> for GlyphSerdeRepr {
    fn from(from: Glyph) -> Self {
        let from: GlyphBuilder = from.into();
        GlyphSerdeRepr {
            name: from.name.as_str().to_string(),
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
