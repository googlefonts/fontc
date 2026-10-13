//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use std::collections::BTreeMap;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    OffsetMarker,
    read::tables::name::{Encoding, MacRomanMapping},
    tables::name::{Name, NameRecord},
    types::NameId,
};

use crate::{
    error::{Error, NameRecordProblem},
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
    #[tracing::instrument(name = "fontbe::NameWork::exec", skip_all)]
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

        // Stamp the fontc version into the version string (name ID 5) so a
        // shipping font binary records which fontc built it (see #2048). fontc
        // threads the version in; None means don't stamp.
        if let Some(version) = context.compiler_version.as_deref() {
            stamp_compiler_version(&mut name_records, version);
        }

        check_encodable(&name_records)?;

        context.name.set(Name::new(name_records));
        Ok(())
    }
}

/// Error on any record write-fonts would panic on when writing the table.
///
/// It only writes UTF-16 (Unicode platform, Windows platform encodings 0, 1
/// and 10) and Mac Roman (Mac platform, encoding 0), and in Mac Roman every
/// character of the string must be representable.
fn check_encodable(records: &[NameRecord]) -> Result<(), Error> {
    for record in records {
        let problem = match Encoding::new(record.platform_id, record.encoding_id) {
            Encoding::Utf16Be => continue,
            Encoding::MacRoman => {
                match record
                    .string
                    .chars()
                    .find(|c| MacRomanMapping.encode(*c).is_none())
                {
                    Some(c) => NameRecordProblem::NotMacRoman(c),
                    None => continue,
                }
            }
            Encoding::Unknown => NameRecordProblem::UnsupportedEncoding,
        };
        return Err(Error::NameRecordEncoding {
            name_id: record.name_id,
            platform_id: record.platform_id,
            encoding_id: record.encoding_id,
            language_id: record.language_id,
            string: record.string.to_string(),
            problem,
        });
    }
    Ok(())
}

/// Append `;fontc {version}` to every version string (name ID 5) record,
/// replacing any prior `;fontc ...` stamp.
///
/// Replacing rather than keeping matters when a source version string carries a
/// *stale* stamp from an earlier build (e.g. a round-tripped
/// `openTypeNameVersion`): the stamp must report the fontc that actually built
/// the font. Our stamp always runs to the end (we append last), so truncating at
/// the marker drops it; this also makes re-stamping idempotent.
fn stamp_compiler_version(records: &mut [NameRecord], version: &str) {
    const MARKER: &str = ";fontc ";
    for record in records.iter_mut() {
        if record.name_id != NameId::VERSION_STRING {
            continue;
        }
        let value = &mut *record.string;
        if let Some(idx) = value.find(MARKER) {
            value.truncate(idx);
        }
        // Don't emit a bare ";fontc ..." when there's no actual version in front
        // of it (an empty record, or one stripped down to nothing).
        if !value.is_empty() {
            value.push_str(MARKER);
            value.push_str(version);
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
        stamp_compiler_version(&mut records, "0.6.1-dev.394+gd62ba016");
        assert_eq!(
            records[0].string.as_str(),
            "Version 1.000;fontc 0.6.1-dev.394+gd62ba016"
        );
    }

    #[test]
    fn appends_after_existing_note() {
        // The version string may already carry a `;`-delimited note (allowed by
        // the OT spec); a note that isn't our stamp is left intact.
        let mut records = vec![version_record("Version 2.000; my custom note")];
        stamp_compiler_version(&mut records, "0.6.0");
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
        stamp_compiler_version(&mut records, "0.6.0");
        assert_eq!(records[0].string.as_str(), "Foo");
        assert_eq!(records[1].string.as_str(), "Version 1.000;fontc 0.6.0");
    }

    #[test]
    fn stamp_is_idempotent() {
        let mut records = vec![version_record("Version 1.000")];
        stamp_compiler_version(&mut records, "0.6.0");
        stamp_compiler_version(&mut records, "0.6.0");
        assert_eq!(records[0].string.as_str(), "Version 1.000;fontc 0.6.0");
    }

    #[test]
    fn replaces_stale_stamp() {
        // A source version string copied from a font built by an earlier fontc
        // must be re-stamped with the fontc that's actually building now,
        // otherwise the provenance lies.
        let mut records = vec![version_record("Version 1.000;fontc 0.5.0")];
        stamp_compiler_version(&mut records, "0.6.1-dev.394+gd62ba016");
        assert_eq!(
            records[0].string.as_str(),
            "Version 1.000;fontc 0.6.1-dev.394+gd62ba016"
        );
    }

    #[test]
    fn does_not_stamp_empty_version_string() {
        // An empty version record (e.g. an explicit empty nameID 5 from FEA) is
        // left empty rather than turned into a bare ";fontc ..." with no version.
        let mut records = vec![version_record("")];
        stamp_compiler_version(&mut records, "0.6.0");
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
        stamp_compiler_version(&mut records, "0.6.0");
        assert!(
            records
                .iter()
                .all(|r| r.string.as_str() == "Version 1.000;fontc 0.6.0")
        );
    }

    fn record(platform_id: u16, encoding_id: u16, language_id: u16, value: &str) -> NameRecord {
        NameRecord::new(
            platform_id,
            encoding_id,
            language_id,
            NameId::FAMILY_NAME,
            value.to_string().into(),
        )
    }

    #[test]
    fn encodable_records_pass() {
        check_encodable(&[
            record(3, 1, 0x409, "Duck Duck"),
            record(3, 1, 0x412, "가석체"),
            record(3, 10, 0x409, "Duck 😀"),
            record(0, 4, 0, "Duck 😀"),
            record(1, 0, 0, "Duck Duck ÄÖÜ"),
        ])
        .unwrap();
    }

    #[test]
    fn unsupported_encoding_is_an_error() {
        let error =
            check_encodable(&[record(3, 1, 0x409, "Duck Duck"), record(1, 3, 23, "가석체")])
                .unwrap_err();
        assert_eq!(
            error.to_string(),
            "name ID 1 (platform 1, encoding 3, language 23) uses an unsupported platform/encoding: \"가석체\""
        );
    }

    #[test]
    fn non_mac_roman_character_is_an_error() {
        let error = check_encodable(&[record(1, 0, 0, "Duck 가석체")]).unwrap_err();
        assert_eq!(
            error.to_string(),
            "name ID 1 (platform 1, encoding 0, language 0) has a character not in Mac Roman, '가' (U+AC00): \"Duck 가석체\""
        );
    }
}
