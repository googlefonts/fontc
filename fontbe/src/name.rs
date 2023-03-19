//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use fontdrasil::orchestration::Work;
use write_fonts::tables::maxp::Maxp;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct NameWork {}

pub fn create_name_work() -> Box<BeWork> {
    Box::new(NameWork {})
}

impl Work<Context, Error> for NameWork {
    /// Generate [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_init_static_metadata();

        Ok(())
    }
}
