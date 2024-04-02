//! Generates a [head](https://learn.microsoft.com/en-us/typography/opentype/spec/head) table.

use std::env;

use chrono::{DateTime, TimeZone, Utc};
use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use log::warn;
use write_fonts::{
    tables::{
        head::{Head, MacStyle},
        loca::LocaFormat,
        os2::SelectionFlags,
    },
    types::{Fixed, LongDateTime},
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct HeadWork {}

pub fn create_head_work() -> Box<BeWork> {
    Box::new(HeadWork {})
}

// The TrueType epoch (1st January 1904) as a Unix timestamp.
// Equivalent to Utc.with_ymd_and_hms(1904, 1, 1, 0, 0, 0).unwrap().timestamp()
const MACINTOSH_EPOCH: i64 = -2082844800;

fn seconds_since_mac_epoch(datetime: DateTime<Utc>) -> i64 {
    let mac_epoch = Utc.timestamp_opt(MACINTOSH_EPOCH, 0).unwrap();
    datetime.signed_duration_since(mac_epoch).num_seconds()
}

/// The number of seconds since 00:00 1904-01-01 (GMT/UTC).
///
/// If the [SOURCE_DATE_EPOCH](https://reproducible-builds.org/specs/source-date-epoch/)
/// environment variable is set, use that instead of the current time.
fn current_timestamp() -> i64 {
    let mut src_date = None;
    if let Ok(src_date_var) = env::var("SOURCE_DATE_EPOCH") {
        if let Ok(timestamp) = src_date_var.parse::<i64>() {
            src_date = Utc.timestamp_opt(timestamp, 0).single();
        };
        if src_date.is_none() {
            warn!(
                "Invalid SOURCE_DATE_EPOCH value: {:?}. Falling back to Utc::now().",
                src_date_var
            );
        }
    }
    seconds_since_mac_epoch(src_date.unwrap_or_else(Utc::now))
}

fn init_head(units_per_em: u16, loca_format: LocaFormat, flags: u16, lowest_rec_ppem: u16) -> Head {
    Head {
        units_per_em,
        lowest_rec_ppem,
        flags,
        index_to_loc_format: match loca_format {
            LocaFormat::Short => 0,
            LocaFormat::Long => 1,
        },
        ..Default::default()
    }
}

/// See:
/// * <https://www.microsoft.com/typography/otspec/recom.htm>
/// * <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/outlineCompiler.py#L313-L326>
fn apply_font_revision(head: &mut Head, major: i32, minor: u32) {
    let major = major as f64;
    let minor = if minor != 0 {
        // Make minor the decimal part
        let mut minor = minor as f64;
        minor /= 10.0_f64.powi(minor.log10().ceil() as i32);
        // Keep 3 decimal places per OTSpec recommendation
        minor = (minor * 1000.0).round() / 1000.0;
        minor
    } else {
        0.0
    };

    head.font_revision = Fixed::from_f64(major + minor);
}

fn apply_macstyle(head: &mut Head, selection_flags: SelectionFlags) {
    head.mac_style = MacStyle::empty();
    if selection_flags.contains(SelectionFlags::BOLD) {
        head.mac_style |= MacStyle::BOLD;
    }

    if selection_flags.contains(SelectionFlags::ITALIC) {
        head.mac_style |= MacStyle::ITALIC;
    }
}

fn apply_created_modified(head: &mut Head, created: Option<DateTime<Utc>>) {
    let now = current_timestamp();
    head.created = LongDateTime::new(created.map(seconds_since_mac_epoch).unwrap_or(now));
    head.modified = LongDateTime::new(now);
}

impl Work<Context, AnyWorkId, Error> for HeadWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Head.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(WorkId::Glyf)
            .variant(WorkId::LocaFormat)
            .build()
    }

    /// Generate [head](https://learn.microsoft.com/en-us/typography/opentype/spec/head)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let loca_format = (*context.loca_format.get().as_ref()).into();
        let mut head = init_head(
            static_metadata.units_per_em,
            loca_format,
            static_metadata.misc.head_flags,
            static_metadata.misc.lowest_rec_ppm,
        );
        apply_font_revision(
            &mut head,
            static_metadata.misc.version_major,
            static_metadata.misc.version_minor,
        );
        apply_created_modified(&mut head, static_metadata.misc.created);
        apply_macstyle(&mut head, static_metadata.misc.selection_flags);
        context.head.set_unconditionally(head.into());

        // Defer x/y Min/Max to metrics and limits job

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use chrono::{TimeZone, Utc};
    use more_asserts::assert_ge;
    use temp_env;
    use write_fonts::tables::{head::MacStyle, loca::LocaFormat, os2::SelectionFlags};

    use crate::head::{apply_created_modified, apply_macstyle};

    use super::{init_head, seconds_since_mac_epoch};

    #[test]
    fn init_head_simple() {
        // if SOURCE_DATE_EPOCH is not set, use the current time for created/modified
        temp_env::with_var_unset("SOURCE_DATE_EPOCH", || {
            let now = seconds_since_mac_epoch(Utc::now());
            let mut head = init_head(1000, LocaFormat::Long, 3, 42);
            apply_created_modified(&mut head, None);
            assert_eq!(head.units_per_em, 1000);
            assert_eq!(head.index_to_loc_format, 1);
            assert_ge!(head.created.as_secs(), now);
            assert_ge!(head.modified.as_secs(), now);
            assert_eq!(head.lowest_rec_ppem, 42);
        });
    }

    #[test]
    fn init_head_with_valid_source_date_epoch() {
        // set SOURCE_DATE_EPOCH to the TrueType epoch, expect 0 for created/modified
        let source_date = Utc
            .with_ymd_and_hms(1904, 1, 1, 0, 0, 0)
            .unwrap()
            .timestamp();
        temp_env::with_var("SOURCE_DATE_EPOCH", Some(source_date.to_string()), || {
            let mut head = init_head(1000, LocaFormat::Short, 3, 42);
            apply_created_modified(&mut head, None);
            assert_eq!(head.created.as_secs(), 0);
            assert_eq!(head.modified.as_secs(), 0);
        });
    }

    #[test]
    fn init_head_with_invalid_source_date_epoch() {
        // if SOURCE_DATE_EPOCH is invalid, set the current time for created/modified
        let now = seconds_since_mac_epoch(Utc::now());
        temp_env::with_var(
            "SOURCE_DATE_EPOCH",
            Some("I am not a Unix timestamp!"),
            || {
                let mut head = init_head(1000, LocaFormat::Short, 3, 42);
                apply_created_modified(&mut head, None);
                assert_ge!(head.created.as_secs(), now);
                assert_ge!(head.modified.as_secs(), now);
            },
        );
    }

    #[test]
    fn apply_head_macstyle() {
        let mut head = init_head(1000, LocaFormat::Long, 3, 42);
        apply_macstyle(&mut head, SelectionFlags::ITALIC);
        assert_eq!(head.mac_style, MacStyle::ITALIC);

        apply_macstyle(&mut head, SelectionFlags::BOLD);
        assert_eq!(head.mac_style, MacStyle::BOLD);

        apply_macstyle(&mut head, SelectionFlags::BOLD | SelectionFlags::ITALIC);
        assert_eq!(head.mac_style, MacStyle::BOLD | MacStyle::ITALIC);
    }
}
