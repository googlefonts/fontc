//! Options used during compilation
//!

// NOTE: This was designed to originate from the command line, but that isn't
// a very important part of our API, and a more natural place for us to specify
// options is in the 'Compiler' struct itself.

const DEFAULT_N_MESSAGES_TO_PRINT: usize = 100;

/// Options for configuring compilation behaviour.
#[derive(Clone, Debug)]
pub struct Opts {
    pub(crate) make_post_table: bool,
    pub(crate) max_n_errors: usize,
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

    /// Specify the number of errors to print when printing a [`DiagnosticSet`].
    ///
    /// Default is some arbitrary 'reasonable' number (currently 100.) To
    /// suppress errors, pass `0`. For 'all errors', pass `usize::MAX`.
    ///
    /// [`DiagnosticSet`]: crate::DiagnosticSet
    pub fn max_error_messages(mut self, max_n_errors: usize) -> Self {
        self.max_n_errors = max_n_errors;
        self
    }
}

impl Default for Opts {
    fn default() -> Self {
        Self {
            make_post_table: false,
            max_n_errors: DEFAULT_N_MESSAGES_TO_PRINT,
        }
    }
}
