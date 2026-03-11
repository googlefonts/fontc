//! Normalizing the ligature caret table

use std::{collections::BTreeSet, fmt::Display, io};

use fontdrasil::types::GlyphName;
use write_fonts::read::{
    ReadError,
    tables::gdef::{CaretValue, Gdef, LigGlyph},
    tables::layout::DeviceOrVariationIndex,
};

use crate::{Error, NameMap, common::DeviceOrDeltas, variations::DeltaComputer};

/// Collect the IVS outer (subtable) indices referenced by lig caret data.
fn collect_caret_subtables(table: &Gdef) -> Result<BTreeSet<u16>, Error> {
    let mut outers = BTreeSet::new();
    let Some(lig_carets) = table.lig_caret_list().transpose().unwrap() else {
        return Ok(outers);
    };
    for lig_glyph in lig_carets.lig_glyphs().iter() {
        let lig_glyph = lig_glyph?;
        for caret in lig_glyph.caret_values().iter() {
            if let CaretValue::Format3(table_ref) = caret?
                && let DeviceOrVariationIndex::VariationIndex(idx) = table_ref.device()?
            {
                outers.insert(idx.delta_set_outer_index());
            }
        }
    }
    Ok(outers)
}

/// Print normalized GDEF ligature carets
pub fn print(f: &mut dyn io::Write, table: &Gdef, names: &NameMap) -> Result<(), Error> {
    let var_store = if table.item_var_store().is_some() {
        let used_subtables = collect_caret_subtables(table)?;
        table
            .item_var_store()
            .map(|ivs| ivs.and_then(|ivs| DeltaComputer::new(ivs, Some(&used_subtables))))
            .transpose()
            .unwrap()
    } else {
        None
    };

    // so this is relatively simple; we're just looking at the ligature caret list.
    // - realistically, we only care if this has variations? but I think it's simpler
    // if we just always normalize, variations or no.

    let Some(lig_carets) = table.lig_caret_list().transpose().unwrap() else {
        return Ok(());
    };

    let coverage = lig_carets.coverage()?;
    for (gid, lig_glyph) in coverage.iter().zip(lig_carets.lig_glyphs().iter()) {
        let lig_glyph = lig_glyph?;
        let name = names.get(gid);
        print_lig_carets(f, name, lig_glyph, var_store.as_ref())?;
    }

    Ok(())
}

enum ResolvedCaret {
    Coordinate {
        pos: i16,
        device_or_deltas: Option<DeviceOrDeltas>,
    },
    // basically never used?
    ContourPoint {
        idx: u16,
    },
}

impl ResolvedCaret {
    fn new(raw: CaretValue, computer: Option<&DeltaComputer>) -> Result<Self, ReadError> {
        match raw {
            CaretValue::Format1(table_ref) => Ok(Self::Coordinate {
                pos: table_ref.coordinate(),
                device_or_deltas: None,
            }),
            CaretValue::Format2(table_ref) => Ok(Self::ContourPoint {
                idx: table_ref.caret_value_point_index(),
            }),
            CaretValue::Format3(table_ref) => {
                let pos = table_ref.coordinate();
                let device = table_ref.device()?;
                let device_or_deltas = DeviceOrDeltas::new(pos, device, computer)?;
                Ok(Self::Coordinate {
                    pos,
                    device_or_deltas: Some(device_or_deltas),
                })
            }
        }
    }
}

fn print_lig_carets(
    f: &mut dyn io::Write,
    gname: &GlyphName,
    lig_glyph: LigGlyph,
    computer: Option<&DeltaComputer>,
) -> Result<(), Error> {
    writeln!(f, "{gname}")?;
    for (i, caret) in lig_glyph.caret_values().iter().enumerate() {
        let resolved = caret.and_then(|caret| ResolvedCaret::new(caret, computer))?;
        writeln!(f, "  {i}: {resolved}")?;
    }

    Ok(())
}

impl Display for ResolvedCaret {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedCaret::Coordinate {
                pos,
                device_or_deltas,
            } => {
                write!(f, "coord {pos}")?;
                if let Some(device_or_deltas) = device_or_deltas {
                    write!(f, "{device_or_deltas}")?;
                }
            }
            ResolvedCaret::ContourPoint { idx } => write!(f, "idx {idx}")?,
        }
        Ok(())
    }
}
