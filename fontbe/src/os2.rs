//! Generates a [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2) table.

use fontdrasil::orchestration::Work;
use fontir::ir::GlobalMetricsInstance;
use read_fonts::types::Tag;
use write_fonts::{tables::os2::Os2, OtRound};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct Os2Work {}

pub fn create_os2_work() -> Box<BeWork> {
    Box::new(Os2Work {})
}

fn build_os2(vendor_id: Tag, metrics: &GlobalMetricsInstance) -> Os2 {
    Os2 {
        ach_vend_id: vendor_id,

        s_cap_height: Some(metrics.cap_height.ot_round()),
        sx_height: Some(metrics.x_height.ot_round()),

        // Avoid "field must be present for version 2"
        ul_code_page_range_1: Some(0),
        ul_code_page_range_2: Some(0),
        us_default_char: Some(0),
        us_break_char: Some(0),
        us_max_context: Some(0),

        ..Default::default()
    }
}

impl Work<Context, Error> for Os2Work {
    /// Generate [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_init_static_metadata();
        let metrics = context
            .ir
            .get_global_metrics()
            .at(static_metadata.default_location());
        context.set_os2(build_os2(static_metadata.vendor_id, &metrics));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontir::{
        coords::NormalizedLocation,
        ir::{GlobalMetric, GlobalMetrics},
    };
    use read_fonts::types::Tag;

    use super::build_os2;

    #[test]
    fn build_basic_os2() {
        let default_location = NormalizedLocation::new();
        let mut global_metrics = GlobalMetrics::new(default_location.clone(), 1000);

        global_metrics.set(GlobalMetric::CapHeight, default_location.clone(), 37.5);
        global_metrics.set(GlobalMetric::XHeight, default_location.clone(), 112.2);

        let os2 = build_os2(Tag::new(b"DUCK"), &global_metrics.at(&default_location));

        assert_eq!(Tag::new(b"DUCK"), os2.ach_vend_id);
        assert_eq!(Some(38), os2.s_cap_height);
        assert_eq!(Some(112), os2.sx_height);
    }
}
