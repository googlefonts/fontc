//! The main public API for compilation

use std::{
    ffi::OsString,
    path::{Path, PathBuf},
};

use crate::{
    parse::{FileSystemResolver, SourceResolver},
    Diagnostic, GlyphMap, ParseTree,
};

use super::{
    error::{CompilerError, DiagnosticSet},
    Compilation, Opts, VariationInfo,
};

/// A builder-style entry point for the compiler.
///
/// This is intended as the principal public API for this crate.
///
/// ```no_run
/// # use fea_rs::Compiler;
/// # fn make_glyph_map() -> fea_rs::GlyphMap { todo!() }
/// let glyph_map = make_glyph_map();
/// let my_font_bytes = Compiler::new("path/to/features.fea", &glyph_map, None)
///     .verbose(true)
///     .compile_binary().unwrap();
/// ```
pub struct Compiler<'a> {
    root_path: OsString,
    project_root: Option<PathBuf>,
    glyph_map: &'a GlyphMap,
    // variable fonts only
    var_info: Option<&'a dyn VariationInfo>,
    verbose: bool,
    opts: Opts,
    resolver: Option<Box<dyn SourceResolver>>,
}

impl<'a> Compiler<'a> {
    /// Configure a new compilation run with a root source and a glyph map.
    ///
    /// In the general case, `root_path` will be a path to a feature file on disk;
    /// however you may compile from memory by passing a custom [`SourceResolver`]
    /// to the [`with_resolver`] method, in which case `root_path` can be any
    /// identifier that your resolver will resolve.
    ///
    /// [`with_resolver`]: Self::with_resolver
    pub fn new(root_path: impl Into<OsString>, glyph_map: &'a GlyphMap) -> Self {
        Compiler {
            root_path: root_path.into(),
            glyph_map,
            var_info: None,
            opts: Default::default(),
            verbose: false,
            resolver: Default::default(),
            project_root: Default::default(),
        }
    }

    /// Provide a custom `SourceResolver`, for mapping paths to their contents.
    pub fn with_resolver(mut self, resolver: impl SourceResolver + 'static) -> Self {
        self.resolver = Some(Box::new(resolver));
        self
    }

    /// Provide [`VariationInfo`], necessary when compiling features for a variable font.
    pub fn with_variable_info(mut self, var_info: &'a dyn VariationInfo) -> Self {
        self.var_info = Some(var_info);
        self
    }

    /// Specify verbosity.
    ///
    /// When verbose is true, we will print all warnings.
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Specify an explicit project root.
    ///
    /// This is useful in cases where import resolution is based on an explicit
    /// base directory, such as when dealing with certain source formats.
    pub fn with_project_root(mut self, project_root: impl Into<PathBuf>) -> Self {
        self.project_root = Some(project_root.into());
        self
    }

    /// Specify additional compiler options.
    pub fn with_opts(mut self, opts: Opts) -> Self {
        self.opts = opts;
        self
    }

    /// Parse, validate and compile this source.
    ///
    /// This returns a `Compilation` object that contains all of the features
    /// and lookups generated during compilation. If you would like to go directly
    /// to a binary font, you can use [`compile_binary`] instead.
    ///
    /// [`compile_binary`]: Self::compile_binary
    pub fn compile(self) -> Result<Compilation, CompilerError> {
        let resolver = self.resolver.unwrap_or_else(|| {
            let project_root = self.project_root.unwrap_or_else(|| {
                Path::new(&self.root_path)
                    .parent()
                    .map(PathBuf::from)
                    .unwrap_or_default()
            });
            Box::new(FileSystemResolver::new(project_root))
        });

        let (tree, diagnostics) =
            crate::parse::ParseContext::parse(self.root_path, Some(self.glyph_map), resolver)?
                .generate_parse_tree();
        print_warnings_return_errors(diagnostics, &tree, self.verbose)
            .map_err(CompilerError::ParseFail)?;
        let diagnostics = super::validate(&tree, self.glyph_map, self.var_info);
        print_warnings_return_errors(diagnostics, &tree, self.verbose)
            .map_err(CompilerError::ValidationFail)?;
        let mut ctx = super::CompilationCtx::new(self.glyph_map, tree.source_map(), self.var_info);
        ctx.compile(&tree.typed_root());

        // we 'take' the errors here because it's easier for us to handle the
        // warnings using our helper method.
        print_warnings_return_errors(std::mem::take(&mut ctx.errors), &tree, self.verbose)
            .map_err(CompilerError::CompilationFail)?;
        Ok(ctx.build().unwrap()) // we've taken the errors, so this can't fail
    }

    /// Compile to a binary font.
    pub fn compile_binary(self) -> Result<Vec<u8>, CompilerError> {
        let opts = self.opts.clone();
        let glyph_map = self.glyph_map;
        Ok(self.compile()?.to_binary(glyph_map, opts)?)
    }
}

fn print_warnings_return_errors(
    mut diagnostics: Vec<Diagnostic>,
    tree: &ParseTree,
    verbose: bool,
) -> Result<(), DiagnosticSet> {
    use std::io::IsTerminal as _;
    let is_tty = std::io::stderr().is_terminal();
    diagnostics.sort_unstable_by_key(|diag| diag.level);
    let split_at = diagnostics
        .iter()
        .position(|x| !x.is_error())
        .unwrap_or(diagnostics.len());
    let warnings = diagnostics.split_off(split_at);
    if verbose {
        for w in warnings {
            eprintln!("{}", tree.format_diagnostic(&w, is_tty));
        }
    }
    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(DiagnosticSet {
            messages: diagnostics,
            sources: tree.sources.clone(),
        })
    }
}
