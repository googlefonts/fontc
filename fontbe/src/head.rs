//! Generates a [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp) table.

use fontdrasil::orchestration::Work;
use write_fonts::tables::head::Head;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct HeadWork {}

pub fn create_head_work() -> Box<BeWork> {
    Box::new(HeadWork {})
}

impl Work<Context, Error> for HeadWork {
    /// Generate [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_final_static_metadata();
        let head = Head {
            units_per_em: static_metadata.units_per_em,
            index_to_loc_format: 1, // TODO: set based on num glyphs
            ..Default::default()
        };
        context.set_head(head);
        Ok(())
    }
}
