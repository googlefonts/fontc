//! Generates a [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp) table.

use fontdrasil::orchestration::Work;
use write_fonts::tables::maxp::Maxp;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct MaxpWork {}

pub fn create_maxp_work() -> Box<BeWork> {
    Box::new(MaxpWork {})
}

impl Work<Context, Error> for MaxpWork {
    /// Generate [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_final_static_metadata();

        let maxp = Maxp {
            num_glyphs: static_metadata.glyph_order.len().try_into().unwrap(),
            // maxp computes it's version based on whether fields are set
            // if you fail to set any of them it gets angry with you so set all of them
            max_points: Some(0),
            max_contours: Some(0),
            max_composite_points: Some(0),
            max_composite_contours: Some(0),
            max_zones: Some(1),
            max_twilight_points: Some(0),
            max_storage: Some(0),
            max_function_defs: Some(0),
            max_instruction_defs: Some(0),
            max_stack_elements: Some(0),
            max_size_of_instructions: Some(0),
            max_component_elements: Some(0),
            max_component_depth: Some(0),
        };
        context.set_maxp(maxp);
        Ok(())
    }
}
