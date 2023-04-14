//! Generates a [head](https://learn.microsoft.com/en-us/typography/opentype/spec/head) table.

use std::env;

use chrono::{DateTime, TimeZone, Utc};
use font_types::LongDateTime;
use fontdrasil::orchestration::Work;
use log::warn;
use write_fonts::tables::head::Head;

use crate::{
    error::Error,
    orchestration::{BeWork, Context, LocaFormat},
};

struct HeadWork {}

pub fn create_head_work() -> Box<BeWork> {
    Box::new(HeadWork {})
}

// The TrueType epoch (1st January 1904) as a Unix timestamp.
// Equivalent to Utc.with_ymd_and_hms(1904, 1, 1, 0, 0, 0).unwrap().timestamp()
const MACINTOSH_EPOCH: i64 = -2082844800;

fn timestamp_since_mac_epoch(datetime: DateTime<Utc>) -> i64 {
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
    timestamp_since_mac_epoch(src_date.unwrap_or_else(Utc::now))
}

fn build_head(units_per_em: u16, loca_format: LocaFormat) -> Head {
    let now = LongDateTime::new(current_timestamp());
    Head {
        units_per_em,
        created: now, // TODO: support GSFont.date / UFO.info.openTypeHeadCreated
        modified: now,
        index_to_loc_format: match loca_format {
            LocaFormat::Short => 0,
            LocaFormat::Long => 1,
        },
        ..Default::default()
    }
}

impl Work<Context, Error> for HeadWork {
    /// Generate [head](https://learn.microsoft.com/en-us/typography/opentype/spec/head)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_final_static_metadata();
        let loca_format = context.get_loca_format();
        let head = build_head(static_metadata.units_per_em, *loca_format);
        context.set_head(head);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use chrono::{TimeZone, Utc};
    use more_asserts::assert_ge;
    use temp_env;

    use crate::orchestration::LocaFormat;

    use super::{build_head, timestamp_since_mac_epoch};

    #[test]
    fn build_head_simple() {
        // if SOURCE_DATE_EPOCH is not set, use the current time for created/modified
        temp_env::with_var_unset("SOURCE_DATE_EPOCH", || {
            let now = timestamp_since_mac_epoch(Utc::now());
            let head = build_head(1000, LocaFormat::Long);
            assert_eq!(head.units_per_em, 1000);
            assert_eq!(head.index_to_loc_format, 1);
            assert_ge!(head.created.as_secs(), now);
            assert_ge!(head.modified.as_secs(), now);
        });
    }

    #[test]
    fn build_head_with_valid_source_date_epoch() {
        // set SOURCE_DATE_EPOCH to the TrueType epoch, expect 0 for created/modified
        let source_date = Utc
            .with_ymd_and_hms(1904, 1, 1, 0, 0, 0)
            .unwrap()
            .timestamp();
        temp_env::with_var("SOURCE_DATE_EPOCH", Some(source_date.to_string()), || {
            let head = build_head(1000, LocaFormat::Short);
            assert_eq!(head.created.as_secs(), 0);
            assert_eq!(head.modified.as_secs(), 0);
        });
    }

    #[test]
    fn build_head_with_invalid_source_date_epoch() {
        // if SOURCE_DATE_EPOCH is invalid, set the current time for created/modified
        let now = timestamp_since_mac_epoch(Utc::now());
        temp_env::with_var(
            "SOURCE_DATE_EPOCH",
            Some("I am not a Unix timestamp!"),
            || {
                let head = build_head(1000, LocaFormat::Short);
                assert_ge!(head.created.as_secs(), now);
                assert_ge!(head.modified.as_secs(), now);
            },
        );
    }
}
