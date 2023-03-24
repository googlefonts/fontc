//! Generates a [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2) table.

use fontdrasil::orchestration::Work;
use fontir::ir::StaticMetadata;
use write_fonts::{tables::os2::Os2, OtRound};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct Os2Work {}

pub fn create_os2_work() -> Box<BeWork> {
    Box::new(Os2Work {})
}

fn build_os2(static_metadata: &StaticMetadata) -> Os2 {
    let default_metrics = static_metadata.default_metrics();
    Os2 {
        s_cap_height: Some(default_metrics.cap_height.ot_round()),
        sx_height: Some(default_metrics.x_height.ot_round()),

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
        let static_metadata = context.ir.get_final_static_metadata();
        context.set_os2(build_os2(&static_metadata));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontir::ir::StaticMetadata;

    use super::build_os2;

    #[test]
    fn build_basic_os2() {
        let mut static_metadata = StaticMetadata::new(
            975,
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
        )
        .unwrap();

        static_metadata.default_metrics_mut().cap_height = 37.5.into();
        static_metadata.default_metrics_mut().x_height = 112.2.into();

        let os2 = build_os2(&static_metadata);

        assert_eq!(Some(38), os2.s_cap_height);
        assert_eq!(Some(112), os2.sx_height);
    }
}
