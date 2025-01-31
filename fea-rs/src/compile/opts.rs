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
    pub(crate) compile_gsub: bool,
    pub(crate) compile_gpos: bool,
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

    /// Specify whether or not we should compile the GPOS table. Default is `true`.
    pub fn compile_gpos(mut self, flag: bool) -> Self {
        self.compile_gpos = flag;
        self
    }

    /// Specify whether or not we should compile the GSUB table. Default is `true`.
    pub fn compile_gsub(mut self, flag: bool) -> Self {
        self.compile_gsub = flag;
        self
    }
}

impl Default for Opts {
    fn default() -> Self {
        Self {
            make_post_table: false,
            max_n_errors: DEFAULT_N_MESSAGES_TO_PRINT,
            compile_gsub: true,
            compile_gpos: true,
        }
    }
}

#[cfg(test)]
mod tests {

    static OSWALD_DIR: &str = "./test-data/real-files/oswald";
    use std::path::Path;

    use crate::{
        compile::{Compilation, MockVariationInfo, NopFeatureProvider},
        Compiler,
    };

    use super::*;

    #[test]
    fn skip_tables() {
        fn compile_oswald(opts: Opts) -> Compilation {
            let glyph_order = Path::new(OSWALD_DIR).join("glyph_order.txt");
            let features = Path::new(OSWALD_DIR).join("features.fea");
            let glyph_order = std::fs::read_to_string(glyph_order).unwrap();
            let glyph_order = crate::compile::parse_glyph_order(&glyph_order).unwrap();
            Compiler::<NopFeatureProvider, MockVariationInfo>::new(features, &glyph_order)
                .with_opts(opts)
                .compile()
                .unwrap()
        }

        // compile everything:
        let compilation = compile_oswald(Opts::new());
        assert!(compilation.gpos.is_some());
        assert!(compilation.gsub.is_some());

        // only gpos
        let compilation = compile_oswald(Opts::new().compile_gsub(false));
        assert!(compilation.gpos.is_some());
        assert!(compilation.gsub.is_none());

        // only gsub
        let compilation = compile_oswald(Opts::new().compile_gpos(false));
        assert!(compilation.gpos.is_none());
        assert!(compilation.gsub.is_some());
    }
}
