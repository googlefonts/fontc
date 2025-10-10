use std::collections::HashSet;

use write_fonts::read::{ReadError, tables::gpos::CursivePosFormat1};

use crate::{
    common::PrintNames,
    gpos::{GlyphId16, ResolvedAnchor},
    variations::DeltaComputer,
};

#[derive(Clone, Debug)]
pub(crate) struct CursivePosRule {
    pub glyph: GlyphId16,
    entry: Option<ResolvedAnchor>,
    exit: Option<ResolvedAnchor>,
}

impl PrintNames for CursivePosRule {
    fn fmt_names(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        names: &crate::NameMap,
    ) -> std::fmt::Result {
        let name = names.get(self.glyph);
        writeln!(f, "{name}")?;
        write!(f, "  entry: ")?;
        match self.entry.as_ref() {
            Some(anchor) => writeln!(f, "{anchor}"),
            None => writeln!(f, "<NULL>"),
        }?;
        write!(f, "  exit: ")?;
        match self.exit.as_ref() {
            Some(anchor) => write!(f, "{anchor}"),
            None => write!(f, "<NULL>"),
        }
    }
}

pub(super) fn get_cursive_rules(
    subtables: &[CursivePosFormat1],
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<CursivePosRule>, ReadError> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();

    for sub in subtables {
        let coverage = sub.coverage().unwrap();
        let entry_exits = sub.entry_exit_record();
        let data = sub.offset_data();
        for (gid, entry_exit) in coverage.iter().zip(entry_exits) {
            if !seen.insert(gid) {
                continue;
            }

            let entry = entry_exit
                .entry_anchor(data)
                .transpose()?
                .map(|a| ResolvedAnchor::new(&a, delta_computer))
                .transpose()?;
            let exit = entry_exit
                .exit_anchor(data)
                .transpose()?
                .map(|a| ResolvedAnchor::new(&a, delta_computer))
                .transpose()?;

            result.push(CursivePosRule {
                glyph: gid,
                entry,
                exit,
            });
        }
    }

    Ok(result)
}
