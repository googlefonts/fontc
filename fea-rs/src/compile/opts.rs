//! Options used during compilation

/// Options for configuring compilation behaviour.
#[derive(Clone, Debug, Default)]
pub struct Opts {
    pub(crate) make_post_table: bool,
}

impl Opts {
    /// Create a new empty set of options
    pub fn new() -> Self {
        Self::default()
    }

    /// If `true`, we will generate a post table from the glyph map.
    pub fn make_post_table(mut self, flag: bool) -> Self {
        self.make_post_table = flag;
        self
    }
}
