//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use std::collections::BTreeMap;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    OffsetMarker,
    tables::name::{Name, NameRecord},
    types::NameId,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct NameWork {}

pub fn create_name_work() -> Box<BeWork> {
    Box::new(NameWork {})
}

impl Work<Context, AnyWorkId, Error> for NameWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Name.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(AnyWorkId::Fe(FeWorkId::StaticMetadata))
            .variant(AnyWorkId::Be(WorkId::ExtraFeaTables))
            .build()
    }

    /// Generate [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let extra_tables = context.extra_fea_tables.try_get();

        let mut name_records = static_metadata
            .names
            .iter()
            .map(|(key, value)| NameRecord {
                name_id: key.name_id,
                platform_id: key.platform_id,
                encoding_id: key.encoding_id,
                language_id: key.lang_id,
                string: OffsetMarker::new(value.clone()),
            })
            .collect::<Vec<_>>();

        if let Some(name) = extra_tables
            .as_ref()
            .and_then(|tables| tables.name.as_ref())
        {
            log::info!("merging name records from FEA");
            name_records = merge_name_records(name_records, name);
        } else {
            name_records.sort();
        }

        // Stamp the caller's compiler identity (e.g. "fontc 0.6.1-dev...") into
        // the version string (name ID 5), so the tool that built a shipping font
        // binary can be identified. The whole stamp is supplied by the caller;
        // fontbe doesn't assume it was fontc.
        if let Some(stamp) = context.compiler_version.as_deref() {
            stamp_compiler_version(&mut name_records, stamp);
        }

        context.name.set(Name::new(name_records));
        Ok(())
    }
}

/// Append `;{stamp}` to every version string (name ID 5) record, replacing any
/// prior stamp from the same tool.
///
/// `stamp` is the caller's `<tool> <version>` (e.g. "fontc 0.6.1-dev..."); the
/// replace marker `;<tool> ` is derived from its leading token. Replacing rather
/// than keeping matters when a source version string carries a *stale* stamp
/// from an earlier build (e.g. a round-tripped `openTypeNameVersion`).
///
/// A stamp not of that `<tool> <digit-led version>` shape is still appended, but
/// re-runs won't detect/replace it (idempotence relies on the shape).
fn stamp_compiler_version(records: &mut [NameRecord], stamp: &str) {
    let tool = stamp.split(' ').next().unwrap_or(stamp);
    let marker = format!(";{tool} ");
    for record in records.iter_mut() {
        if record.name_id != NameId::VERSION_STRING {
            continue;
        }
        let value = &mut *record.string;
        // A prior stamp is always appended last, so it runs to the end; drop it
        // before re-stamping. Only treat the marker as a stamp when a digit
        // follows (the version is SemVer, digit-led), so a human-authored note
        // like "...;fontc rules" is left untouched.
        if let Some(idx) = value.find(&marker)
            && value
                .as_bytes()
                .get(idx + marker.len())
                .is_some_and(u8::is_ascii_digit)
        {
            value.truncate(idx);
        }
        // Don't emit a bare ";<tool> ..." when there's no actual version in
        // front of it (an empty record, or one stripped down to nothing).
        if !value.is_empty() {
            value.push(';');
            value.push_str(stamp);
        }
    }
}

// records from fea overwrite records derived elsewhere:
// https://github.com/fonttools/fonttools/blob/b90ac3c29f6030ec/Lib/fontTools/feaLib/builder.py#L452
fn merge_name_records(records: Vec<NameRecord>, fea_names: &Name) -> Vec<NameRecord> {
    fn split_key_and_string(name_record: NameRecord) -> ((u16, u16, u16, NameId), String) {
        let NameRecord {
            platform_id,
            encoding_id,
            language_id,
            name_id,
            string,
        } = name_record;
        (
            (platform_id, encoding_id, language_id, name_id),
            string.into_inner(),
        )
    }

    let sorted_and_deduped = records
        .into_iter()
        .chain(fea_names.name_record.iter().cloned())
        .map(split_key_and_string)
        .collect::<BTreeMap<_, _>>();

    sorted_and_deduped
        .into_iter()
        .map(|((platform, encoding, language, name), string)| {
            NameRecord::new(platform, encoding, language, name, string.into())
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn version_record(value: &str) -> NameRecord {
        NameRecord::new(
            3,
            1,
            0x409,
            NameId::VERSION_STRING,
            value.to_string().into(),
        )
    }

    #[test]
    fn stamps_version_string() {
        let mut records = vec![version_record("Version 1.000")];
        stamp_compiler_version(&mut records, "fontc 0.6.1-dev.394+gd62ba016");
        assert_eq!(
            records[0].string.as_str(),
            "Version 1.000;fontc 0.6.1-dev.394+gd62ba016"
        );
    }

    #[test]
    fn appends_after_existing_note() {
        // The version string may already carry a `;`-delimited note (allowed by
        // the OT spec); the stamp is appended after it, leaving it intact.
        let mut records = vec![version_record("Version 2.000; my custom note")];
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        assert_eq!(
            records[0].string.as_str(),
            "Version 2.000; my custom note;fontc 0.6.0"
        );
    }

    #[test]
    fn only_stamps_version_records() {
        let mut records = vec![
            NameRecord::new(3, 1, 0x409, NameId::FAMILY_NAME, "Foo".to_string().into()),
            version_record("Version 1.000"),
        ];
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        assert_eq!(records[0].string.as_str(), "Foo");
        assert_eq!(records[1].string.as_str(), "Version 1.000;fontc 0.6.0");
    }

    #[test]
    fn stamp_is_idempotent() {
        let mut records = vec![version_record("Version 1.000")];
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        assert_eq!(records[0].string.as_str(), "Version 1.000;fontc 0.6.0");
    }

    #[test]
    fn replaces_stale_stamp() {
        // A source version string copied from a font built by an earlier fontc
        // must be re-stamped with the fontc that's actually building now,
        // otherwise the provenance lies.
        let mut records = vec![version_record("Version 1.000;fontc 0.5.0")];
        stamp_compiler_version(&mut records, "fontc 0.6.1-dev.394+gd62ba016");
        assert_eq!(
            records[0].string.as_str(),
            "Version 1.000;fontc 0.6.1-dev.394+gd62ba016"
        );
    }

    #[test]
    fn leaves_non_stamp_fontc_note() {
        // A human note that merely starts a segment with "fontc " (no version)
        // is not our stamp (a digit always follows ours) so it survives.
        let mut records = vec![version_record("Version 1.000;fontc is great")];
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        assert_eq!(
            records[0].string.as_str(),
            "Version 1.000;fontc is great;fontc 0.6.0"
        );
    }

    #[test]
    fn does_not_stamp_empty_version_string() {
        // An empty version record (e.g. an explicit empty nameID 5 from FEA) is
        // left empty rather than turned into a bare ";fontc ..." with no version.
        let mut records = vec![version_record("")];
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        assert_eq!(records[0].string.as_str(), "");
    }

    #[test]
    fn stamps_every_platform_version_record() {
        // e.g. when FEA adds a Mac (platform 1) version string alongside Windows.
        let mut records = vec![
            NameRecord::new(
                3,
                1,
                0x409,
                NameId::VERSION_STRING,
                "Version 1.000".to_string().into(),
            ),
            NameRecord::new(
                1,
                0,
                0,
                NameId::VERSION_STRING,
                "Version 1.000".to_string().into(),
            ),
        ];
        stamp_compiler_version(&mut records, "fontc 0.6.0");
        assert!(
            records
                .iter()
                .all(|r| r.string.as_str() == "Version 1.000;fontc 0.6.0")
        );
    }
}
