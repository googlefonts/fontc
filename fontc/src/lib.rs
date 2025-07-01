//! A font compiler with aspirations of being fast and safe.

#[cfg(feature = "cli")]
mod args;
mod error;
mod timing;
pub mod work;
mod workload;

#[cfg(feature = "cli")]
pub use args::Args;
pub use error::Error;

pub use fontir::orchestration::Flags; // Re-export for library users
use fontra2fontir::source::FontraIrSource;
use glyphs2fontir::source::GlyphsIrSource;
pub use timing::JobTimer;
use ufo2fontir::source::DesignSpaceIrSource;
use workload::Workload;

use fontbe::orchestration::AnyWorkId;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use fontir::{orchestration::Context as FeContext, source::Source};

use fontbe::{orchestration::Context as BeContext, paths::Paths as BePaths};
use fontir::paths::Paths as IrPaths;

use log::debug;

/// The input source for the font compiler.
///
/// This allows us to load a font source in a variety of formats, and also
/// from a variety of sources (on disk or in memory).
pub enum Input {
    DesignSpacePath(PathBuf),
    GlyphsPath(PathBuf),
    FontraPath(PathBuf),
    GlyphsMemory(String),
}

impl Input {
    pub fn new(path: &Path) -> Result<Self, Error> {
        if !path.exists() {
            return Err(Error::FileExpected(path.to_path_buf()));
        }
        let ext = path
            .extension()
            .and_then(OsStr::to_str)
            .ok_or_else(|| Error::UnrecognizedSource(path.to_path_buf()))?;
        match ext {
            "designspace" => Ok(Input::DesignSpacePath(path.to_path_buf())),
            "ufo" => Ok(Input::DesignSpacePath(path.to_path_buf())),
            "glyphs" => Ok(Input::GlyphsPath(path.to_path_buf())),
            "glyphspackage" => Ok(Input::GlyphsPath(path.to_path_buf())),
            "fontra" => Ok(Input::FontraPath(path.to_path_buf())),
            _ => Err(Error::UnrecognizedSource(path.to_path_buf())),
        }
    }

    /// Creates a new input from a Glyphs source in memory
    pub fn from_glyphs(source: String) -> Self {
        Input::GlyphsMemory(source)
    }

    /// Creates the implementation of [`Source`] to feed to fontir.
    fn create_source(&self) -> Result<Box<dyn Source>, Error> {
        match self {
            Input::DesignSpacePath(path) => Ok(Box::new(DesignSpaceIrSource::new(path)?)),
            Input::GlyphsPath(path) => Ok(Box::new(GlyphsIrSource::new(path)?)),
            Input::FontraPath(path) => Ok(Box::new(FontraIrSource::new(path)?)),
            Input::GlyphsMemory(source) => Ok(Box::new(GlyphsIrSource::new_from_memory(source)?)),
        }
    }
}

impl TryFrom<&Path> for Input {
    type Error = Error;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        Input::new(path)
    }
}

/// Run the compiler with the provided arguments
///
/// This is the main entry point for the fontc command line utility.
#[cfg(feature = "cli")]
pub fn run(args: Args, timer: JobTimer) -> Result<(), Error> {
    let source = args.source()?;
    let (be_root, mut timing) = _generate_font(
        &source,
        &args.build_dir,
        args.output_file.as_ref(),
        args.flags(),
        args.skip_features,
        timer,
    )?;

    if args.flags().contains(Flags::EMIT_TIMING) {
        let path = args.build_dir.join("threads.svg");
        let out_file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path)
            .map_err(|source| Error::FileIo {
                path: path.clone(),
                source,
            })?;
        let mut buf = std::io::BufWriter::new(out_file);
        timing
            .write_svg(&mut buf)
            .map_err(|source| Error::FileIo { path, source })?;
    }

    // At long last!
    write_font_file(&args, &be_root)
}

/// Run and return an OpenType font
///
/// This is the library entry point to fontc.
pub fn generate_font(
    source: &Input,
    build_dir: &Path,
    output_file: Option<&PathBuf>,
    flags: Flags,
    skip_features: bool,
) -> Result<Vec<u8>, Error> {
    _generate_font(
        source,
        build_dir,
        output_file,
        flags,
        skip_features,
        JobTimer::default(),
    )
    .map(|(be_root, _timing)| be_root.font.get().get().to_vec())
}

fn _generate_font(
    source: &Input,
    build_dir: &Path,
    output_file: Option<&PathBuf>,
    flags: Flags,
    skip_features: bool,
    mut timer: JobTimer,
) -> Result<(BeContext, JobTimer), Error> {
    let time = timer
        .create_timer(AnyWorkId::InternalTiming("Init config"), 0)
        .run();
    let (ir_paths, be_paths) = init_paths(output_file, build_dir, flags)?;
    timer.add(time.complete());
    let workload = Workload::new(source, timer, skip_features)?;
    let fe_root = FeContext::new_root(flags, ir_paths);
    let be_root = BeContext::new_root(flags, be_paths, &fe_root);
    let timing = workload.exec(&fe_root, &be_root)?;
    Ok((be_root, timing))
}

pub fn require_dir(dir: &Path) -> Result<(), Error> {
    // skip empty paths
    if dir == Path::new("") {
        return Ok(());
    }
    if dir.exists() && !dir.is_dir() {
        return Err(Error::ExpectedDirectory(dir.to_owned()));
    }
    if !dir.exists() {
        fs::create_dir(dir).map_err(|source| Error::FileIo {
            path: dir.to_owned(),
            source,
        })?;
    }
    debug!("require_dir {dir:?}");
    Ok(())
}

pub fn init_paths(
    output_file: Option<&PathBuf>,
    build_dir: &Path,
    flags: Flags,
) -> Result<(IrPaths, BePaths), Error> {
    let ir_paths = IrPaths::new(build_dir);
    let be_paths = if let Some(output_file) = output_file {
        BePaths::with_output_file(build_dir, output_file)
    } else {
        BePaths::new(build_dir)
    };
    // create the output file's parent directory if it doesn't exist
    if let Some(output_file) = output_file {
        if let Some(parent) = output_file.parent() {
            require_dir(parent)?;
        }
    }

    // the build dir stores the IR (for incremental builds) and the default output
    // file ('font.ttf') so we don't need to create one unless we're writing to it
    if output_file.is_none() || flags.contains(Flags::EMIT_IR) {
        require_dir(build_dir)?;
    }
    if flags.contains(Flags::EMIT_IR) {
        require_dir(ir_paths.anchor_ir_dir())?;
        require_dir(ir_paths.glyph_ir_dir())?;
        require_dir(be_paths.glyph_dir())?;
    }
    // It's confusing to have leftover debug files
    if be_paths.debug_dir().is_dir() {
        fs::remove_dir_all(be_paths.debug_dir()).map_err(|source| Error::FileIo {
            path: be_paths.debug_dir().to_owned(),
            source,
        })?;
    }
    if flags.contains(Flags::EMIT_DEBUG) {
        require_dir(be_paths.debug_dir())?;
    }
    Ok((ir_paths, be_paths))
}

#[cfg(feature = "cli")]
pub fn write_font_file(args: &Args, be_context: &BeContext) -> Result<(), Error> {
    // if IR is off the font didn't get written yet (nothing did), otherwise it's done already
    let font_file = be_context.font_file();
    if !args.emit_ir {
        fs::write(&font_file, be_context.font.get().get()).map_err(|source| Error::FileIo {
            path: font_file,
            source,
        })?;
    } else if !font_file.exists() {
        return Err(Error::FileExpected(font_file));
    }
    Ok(())
}

#[cfg(test)]
pub fn testdata_dir() -> std::path::PathBuf {
    // cargo test seems to run in the project directory
    // VSCode test seems to run in the workspace directory
    // probe for the file we want in hopes of finding it regardless

    ["./resources/testdata", "../resources/testdata"]
        .iter()
        .map(std::path::PathBuf::from)
        .find(|pb| pb.exists())
        .unwrap()
}

#[cfg(test)]
mod tests {

    use std::{
        collections::{HashMap, HashSet, VecDeque},
        fs::{self, File},
        io::Read,
        path::{Path, PathBuf},
        sync::Arc,
    };

    use ordered_float::OrderedFloat;

    use chrono::{Duration, TimeZone, Utc};
    use fontbe::orchestration::{
        AnyWorkId, Context as BeContext, Glyph, LocaFormatWrapper, WorkId as BeWorkIdentifier,
    };
    use fontdrasil::{
        coords::{NormalizedCoord, NormalizedLocation},
        paths::string_to_filename,
        types::{GlyphName, WidthClass},
    };
    use fontir::{
        ir::{self, GlobalMetric, GlyphOrder, KernGroup, KernPair, KernSide},
        orchestration::{Context as FeContext, Persistable, WorkId as FeWorkIdentifier},
    };
    use kurbo::{Point, Rect};
    use log::info;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use skrifa::{charmap::Charmap, instance::Size, outline::DrawSettings, MetadataProvider};
    use tempfile::{tempdir, TempDir};
    use write_fonts::{
        dump_table,
        read::{
            tables::{
                cmap::{Cmap, CmapSubtable},
                cpal::ColorRecord,
                gasp::GaspRangeBehavior,
                glyf::{self, CompositeGlyph, CurvePoint, Glyf},
                gpos::{AnchorTable, Gpos, MarkBasePosFormat1Marker, PositionLookup},
                gsub::{SingleSubst, SubstitutionLookup},
                hmtx::Hmtx,
                layout::FeatureParams,
                loca::Loca,
                name::Name,
                os2::SelectionFlags,
                post::Post,
                variations::{DeltaSetIndexMap, ItemVariationData},
            },
            FontData, FontRead, FontReadWithArgs, FontRef, TableProvider, TableRef,
        },
        tables::{
            gdef::GlyphClassDef,
            glyf::{Bbox, Glyph as RawGlyph},
            loca::LocaFormat,
            meta::{DataMapRecord, Metadata, ScriptLangTag},
        },
        types::{F2Dot14, GlyphId, GlyphId16, NameId, Tag},
    };

    use super::*;

    struct TestCompile {
        /// we need to hold onto this because when it goes out of scope,
        /// the directory is deleted.
        _temp_dir: TempDir,
        build_dir: PathBuf,
        work_executed: HashSet<AnyWorkId>,
        fe_context: FeContext,
        be_context: BeContext,
        raw_font: Vec<u8>,
    }

    impl TestCompile {
        fn compile_source(source: &str) -> TestCompile {
            TestCompile::compile(source, |args| args)
        }

        fn compile(source: &str, adjust_args: impl Fn(Args) -> Args) -> TestCompile {
            let timer = JobTimer::new();
            let _ = env_logger::builder().is_test(true).try_init();

            let temp_dir = tempdir().unwrap();
            let build_dir = temp_dir.path();
            let args = adjust_args(Args::for_test(build_dir, source));
            let flags = args.flags();

            info!("Compile {args:?}");

            let (ir_paths, be_paths) =
                init_paths(args.output_file.as_ref(), &args.build_dir, flags).unwrap();

            let build_dir = be_paths.build_dir().to_path_buf();

            let fe_context = FeContext::new_root(flags, ir_paths);
            let be_context = BeContext::new_root(flags, be_paths, &fe_context.read_only());
            let mut result = TestCompile {
                _temp_dir: temp_dir,
                build_dir,
                work_executed: HashSet::new(),
                fe_context,
                be_context,
                raw_font: Vec::new(),
            };

            let source = args.source().unwrap();
            let mut workload = Workload::new(&source, timer, args.skip_features).unwrap();
            let completed = workload.run_for_test(&result.fe_context, &result.be_context);

            result.work_executed = completed;

            write_font_file(&args, &result.be_context).unwrap();

            result.raw_font = fs::read(result.build_dir.join("font.ttf")).unwrap();

            result
        }

        fn get_glyph_index(&self, name: &str) -> Option<u32> {
            self.fe_context
                .glyph_order
                .get()
                .glyph_id(name)
                .map(|v| v.to_u16() as u32)
        }

        fn contains_glyph(&self, name: &str) -> bool {
            self.get_glyph_index(name).is_some()
        }

        /// Returns the GlyphId16 (or NOTDEF, if not present)
        fn get_gid(&self, name: &str) -> GlyphId16 {
            let raw = self.get_glyph_index(name).unwrap_or_default();
            GlyphId16::new(raw as u16)
        }

        fn glyphs(&self) -> Glyphs {
            Glyphs::new(&self.build_dir)
        }

        fn font(&self) -> FontRef {
            FontRef::new(&self.raw_font).unwrap()
        }
    }

    struct Glyphs {
        loca_format: LocaFormat,
        raw_glyf: Vec<u8>,
        raw_loca: Vec<u8>,
    }

    impl Glyphs {
        fn new(build_dir: &Path) -> Self {
            Glyphs {
                loca_format: LocaFormatWrapper::read(
                    &mut File::open(build_dir.join("loca.format")).unwrap(),
                )
                .into(),
                raw_glyf: read_file(build_dir, Path::new("glyf.table")),
                raw_loca: read_file(build_dir, Path::new("loca.table")),
            }
        }

        fn read(&self) -> Vec<Option<glyf::Glyph>> {
            let glyf = Glyf::read(FontData::new(&self.raw_glyf)).unwrap();
            let loca = Loca::read(
                FontData::new(&self.raw_loca),
                self.loca_format == LocaFormat::Long,
            )
            .unwrap();
            (0..loca.len())
                .map(|gid| loca.get_glyf(GlyphId16::new(gid as u16).into(), &glyf))
                .map(|r| r.unwrap())
                .collect()
        }
    }

    fn assert_compile_work(source: &str, glyphs: Vec<&str>) {
        let result = TestCompile::compile_source(source);
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();

        let mut expected = vec![
            AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata),
            FeWorkIdentifier::ColorPalettes.into(),
            FeWorkIdentifier::GlobalMetrics.into(),
            FeWorkIdentifier::PaintGraph.into(),
            FeWorkIdentifier::PreliminaryGlyphOrder.into(),
            FeWorkIdentifier::GlyphOrder.into(),
            FeWorkIdentifier::Features.into(),
            FeWorkIdentifier::KerningGroups.into(),
            FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(&[("wght", 0.0)])).into(),
            FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(&[("wght", 1.0)])).into(),
            BeWorkIdentifier::Features.into(),
            BeWorkIdentifier::FeaturesAst.into(),
            BeWorkIdentifier::Avar.into(),
            BeWorkIdentifier::Cmap.into(),
            BeWorkIdentifier::Colr.into(),
            BeWorkIdentifier::Cpal.into(),
            BeWorkIdentifier::Font.into(),
            BeWorkIdentifier::Fvar.into(),
            BeWorkIdentifier::Gasp.into(),
            BeWorkIdentifier::Glyf.into(),
            BeWorkIdentifier::Gpos.into(),
            BeWorkIdentifier::Gsub.into(),
            BeWorkIdentifier::Gdef.into(),
            BeWorkIdentifier::Gvar.into(),
            BeWorkIdentifier::Head.into(),
            BeWorkIdentifier::Hhea.into(),
            BeWorkIdentifier::Hmtx.into(),
            BeWorkIdentifier::Hvar.into(),
            BeWorkIdentifier::GatherIrKerning.into(),
            BeWorkIdentifier::KernFragment(0).into(),
            BeWorkIdentifier::GatherBeKerning.into(),
            BeWorkIdentifier::ExtraFeaTables.into(),
            BeWorkIdentifier::Loca.into(),
            BeWorkIdentifier::LocaFormat.into(),
            BeWorkIdentifier::Marks.into(),
            BeWorkIdentifier::Maxp.into(),
            BeWorkIdentifier::Meta.into(),
            BeWorkIdentifier::Mvar.into(),
            BeWorkIdentifier::Name.into(),
            BeWorkIdentifier::Os2.into(),
            BeWorkIdentifier::Post.into(),
            BeWorkIdentifier::Stat.into(),
            BeWorkIdentifier::Vhea.into(),
            BeWorkIdentifier::Vmtx.into(),
        ];

        expected.extend(
            glyphs
                .iter()
                .map(|n| FeWorkIdentifier::Glyph((*n).into()).into()),
        );
        expected.extend(
            glyphs
                .iter()
                .map(|n| FeWorkIdentifier::Anchor((*n).into()).into()),
        );
        expected.extend(
            glyphs
                .iter()
                .map(|n| BeWorkIdentifier::GlyfFragment((*n).into()).into()),
        );
        expected.extend(
            glyphs
                .iter()
                .map(|n| BeWorkIdentifier::GvarFragment((*n).into()).into()),
        );

        expected.extend(vec![
            BeWorkIdentifier::GlyfFragment((".notdef").into()).into(),
            BeWorkIdentifier::GvarFragment((".notdef").into()).into(),
        ]);

        completed.sort();
        expected.sort();
        assert_eq!(expected, completed);
    }

    #[test]
    fn compile_work_for_designspace() {
        assert_compile_work("wght_var.designspace", vec!["bar", "plus"])
    }

    #[test]
    fn compile_work_for_glyphs() {
        assert_compile_work(
            "glyphs3/WghtVar.glyphs",
            vec![
                "bracketleft",
                "bracketright",
                "exclam",
                "hyphen",
                "manual-component",
                "space",
            ],
        )
    }

    fn assert_compiles_with_gpos_and_gsub(
        source: &str,
        adjust_args: impl Fn(Args) -> Args,
    ) -> TestCompile {
        let result = TestCompile::compile(source, adjust_args);

        let font = result.font();

        let gpos = font.gpos();
        let gsub = font.gsub();
        assert!(
            gpos.is_ok() && gsub.is_ok(),
            "\ngpos: {gpos:?}\ngsub: {gsub:?}"
        );

        result
    }
    #[test]
    fn compile_fea() {
        assert_compiles_with_gpos_and_gsub("static.designspace", |a| a);
    }

    #[test]
    fn compile_fea_with_includes() {
        assert_compiles_with_gpos_and_gsub("fea_include.designspace", |a| a);
    }

    #[test]
    fn compile_fea_with_includes_no_ir() {
        assert_compiles_with_gpos_and_gsub("fea_include.designspace", |mut args| {
            args.emit_debug = false;
            args.emit_ir = false;
            args
        });
    }

    fn build_contour_and_composite_glyph(prefer_simple_glyphs: bool) -> (TestCompile, ir::Glyph) {
        let result = TestCompile::compile("glyphs2/MixedContourComponent.glyphs", |mut args| {
            args.prefer_simple_glyphs = prefer_simple_glyphs; // <-- important :)
            args
        });

        let glyph = result
            .fe_context
            .glyphs
            .get(&FeWorkIdentifier::Glyph("contour_and_component".into()));

        assert_eq!(1, glyph.sources().len());
        (result, (*glyph).clone())
    }

    fn read_file(build_dir: &Path, path: &Path) -> Vec<u8> {
        assert!(build_dir.is_dir(), "{build_dir:?} isn't a directory?!");
        let path = build_dir.join(path);
        if !path.exists() {
            // When a path is missing it's very helpful to know what's present
            use std::io::Write;
            let mut stderr = std::io::stderr().lock();
            writeln!(stderr, "Build dir tree").unwrap();
            let mut pending = VecDeque::new();
            pending.push_back(build_dir);
            while let Some(pending_dir) = pending.pop_front() {
                for entry in fs::read_dir(pending_dir).unwrap() {
                    let entry = entry.unwrap();
                    writeln!(stderr, "{}", entry.path().to_str().unwrap()).unwrap();
                }
            }
        }
        assert!(path.exists(), "{path:?} not found");
        let mut buf = Vec::new();
        File::open(path).unwrap().read_to_end(&mut buf).unwrap();
        buf
    }

    fn read_ir_glyph(build_dir: &Path, name: &str) -> ir::Glyph {
        let raw_glyph = read_file(
            build_dir,
            &Path::new("glyph_ir").join(string_to_filename(name, ".yml")),
        );
        ir::Glyph::read(&mut raw_glyph.as_slice())
    }

    fn read_be_glyph(build_dir: &Path, name: &str) -> RawGlyph {
        let raw_glyph = read_file(
            build_dir,
            &Path::new("glyphs").join(string_to_filename(name, ".glyf")),
        );
        let read: &mut dyn Read = &mut raw_glyph.as_slice();
        Glyph::read(read).data
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_non_legacy_mode() {
        let (result, glyph) = build_contour_and_composite_glyph(false);
        assert!(glyph.default_instance().contours.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().components.len(), "{glyph:?}");

        let RawGlyph::Composite(glyph) = read_be_glyph(&result.build_dir, glyph.name.as_str())
        else {
            panic!("Expected a simple glyph");
        };
        let raw_glyph = dump_table(&glyph).unwrap();
        let glyph = CompositeGlyph::read(FontData::new(&raw_glyph)).unwrap();
        // -1: composite, per https://learn.microsoft.com/en-us/typography/opentype/spec/glyf
        assert_eq!(-1, glyph.number_of_contours());
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_legacy_mode() {
        let (result, glyph) = build_contour_and_composite_glyph(true);
        assert!(glyph.default_instance().components.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().contours.len(), "{glyph:?}");

        let RawGlyph::Simple(glyph) = read_be_glyph(&result.build_dir, glyph.name.as_str()) else {
            panic!("Expected a simple glyph");
        };
        assert_eq!(2, glyph.contours.len());
    }

    #[test]
    fn compile_simple_binary_glyph() {
        let result = TestCompile::compile_source("static.designspace");

        let RawGlyph::Simple(glyph) = read_be_glyph(&result.build_dir, "bar") else {
            panic!("Expected a simple glyph");
        };

        assert_eq!(1, glyph.contours.len());
        assert_eq!(
            4_usize,
            glyph
                .contours
                .iter()
                .map(|c| c.iter().count())
                .sum::<usize>()
        );
        assert_eq!(
            Bbox {
                x_min: 222,
                y_min: -241,
                x_max: 295,
                y_max: 760,
            },
            glyph.bbox,
        );
    }

    #[test]
    fn compile_simple_glyphs_to_glyf_loca() {
        let result = TestCompile::compile_source("static.designspace");

        // See resources/testdata/Static-Regular.ufo/glyphs
        // generated .notdef, 8 points, 2 contours
        // space, 0 points, 0 contour
        // bar, 4 points, 1 contour
        // plus, 12 points, 1 contour
        // element of, 0 points, 0 contours
        assert_eq!(
            vec![(8, 2), (0, 0), (4, 1), (12, 1), (0, 0)],
            result
                .glyphs()
                .read()
                .iter()
                .map(|g| match g {
                    None => (0, 0),
                    Some(glyf::Glyph::Simple(glyph)) =>
                        (glyph.num_points(), glyph.number_of_contours()),
                    Some(glyf::Glyph::Composite(glyph)) => (0, glyph.number_of_contours()),
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_sets_xmin_eq_lsb_flag() {
        let result = TestCompile::compile_source("fontinfo.designspace");
        let head = result.font().head().unwrap();
        assert_eq!(0b10, head.flags() & 0b10);
    }

    #[test]
    fn compile_obeys_family_class() {
        let result = TestCompile::compile_source("fontinfo.designspace");
        let os2 = result.font().os2().unwrap();
        assert_eq!((1 << 8) | 2, os2.s_family_class());
    }

    #[test]
    fn compile_obeys_designspace_gasp_records() {
        let result = TestCompile::compile_source("fontinfo.designspace");
        let gasp = result.font().gasp().unwrap();
        assert_eq!(
            vec![
                (
                    7,
                    GaspRangeBehavior::GASP_DOGRAY | GaspRangeBehavior::GASP_SYMMETRIC_SMOOTHING
                ),
                (
                    65535,
                    GaspRangeBehavior::GASP_GRIDFIT
                        | GaspRangeBehavior::GASP_DOGRAY
                        | GaspRangeBehavior::GASP_SYMMETRIC_GRIDFIT
                        | GaspRangeBehavior::GASP_SYMMETRIC_SMOOTHING
                ),
            ],
            gasp.gasp_ranges()
                .iter()
                .map(|r| (r.range_max_ppem(), r.range_gasp_behavior()))
                .collect::<Vec<_>>(),
        )
    }

    #[test]
    fn compile_obeys_glyphs_gasp_records() {
        let result = TestCompile::compile_source("glyphs3/WghtVarGasp.glyphs");
        let gasp = result.font().gasp().unwrap();
        assert_eq!(
            vec![
                (
                    8,
                    GaspRangeBehavior::GASP_DOGRAY | GaspRangeBehavior::GASP_SYMMETRIC_SMOOTHING
                ),
                (
                    20,
                    GaspRangeBehavior::GASP_GRIDFIT
                        | GaspRangeBehavior::GASP_DOGRAY
                        | GaspRangeBehavior::GASP_SYMMETRIC_GRIDFIT,
                ),
                (
                    65535,
                    GaspRangeBehavior::GASP_GRIDFIT
                        | GaspRangeBehavior::GASP_DOGRAY
                        | GaspRangeBehavior::GASP_SYMMETRIC_GRIDFIT
                        | GaspRangeBehavior::GASP_SYMMETRIC_SMOOTHING
                ),
            ],
            gasp.gasp_ranges()
                .iter()
                .map(|r| (r.range_max_ppem(), r.range_gasp_behavior()))
                .collect::<Vec<_>>(),
        )
    }

    #[test]
    fn compile_gasp_only_from_default() {
        let result = TestCompile::compile_source("fontinfo_var.designspace");
        assert!(result.font().gasp().is_err());
    }

    #[test]
    fn compile_prefers_variable_default_to_fontinfo_value() {
        let result = TestCompile::compile_source("fontinfo_var.designspace");
        let os2 = result.font().os2().unwrap();
        assert_eq!(
            100,
            os2.us_weight_class(),
            "We should use wght default, NOT openTypeOS2WeightClass"
        );
    }

    #[test]
    fn compile_variable_simple_glyph_with_implied_oncurves() {
        let result = TestCompile::compile_source("glyphs3/Oswald-O.glyphs");

        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        // the glyph 'O' contains several quad splines
        let uppercase_o = &glyphs[result.get_glyph_index("O").unwrap() as usize];
        let Some(glyf::Glyph::Simple(glyph)) = uppercase_o else {
            panic!("Expected 'O' to be a simple glyph, got {uppercase_o:?}");
        };
        assert_eq!(2, glyph.number_of_contours());
        assert_eq!(35, glyph.num_points());
        assert_eq!(
            [48, -9, 491, 817],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
        // for brevity, we only check the first 5 points to confirm that we now get
        // multiple off-curve points in a row with impliable on-curve points omitted
        assert_eq!(
            vec![
                CurvePoint::on_curve(270, -9),
                CurvePoint::off_curve(188, -9),
                CurvePoint::off_curve(90, 55),
                CurvePoint::off_curve(48, 174),
                CurvePoint::on_curve(48, 254),
            ],
            glyph.points().take(5).collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_composite_glyphs_has_expected_glyph_types() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");
        // Per source, glyphs should be period, comma, non_uniform_scale
        // Period is simple, the other two use it as a component
        let glyph_data = result.glyphs();
        let all_glyphs = glyph_data.read();

        assert!(matches!(
            (
                all_glyphs[result.get_glyph_index("period").unwrap() as usize]
                    .as_ref()
                    .unwrap(),
                all_glyphs[result.get_glyph_index("comma").unwrap() as usize]
                    .as_ref()
                    .unwrap(),
                all_glyphs[result.get_glyph_index("non_uniform_scale").unwrap() as usize]
                    .as_ref()
                    .unwrap(),
            ),
            (
                glyf::Glyph::Simple(..),
                glyf::Glyph::Composite(..),
                glyf::Glyph::Composite(..),
            ),
        ));
    }

    #[test]
    fn compile_composite_glyphs_to_glyf_loca() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");
        // non-uniform scaling of period
        let period_idx = result.get_glyph_index("period").unwrap();
        let non_uniform_scale_idx = result.get_glyph_index("non_uniform_scale").unwrap();
        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        let Some(glyf::Glyph::Composite(glyph)) = &glyphs[non_uniform_scale_idx as usize] else {
            panic!("Expected a composite\n{glyphs:#?}");
        };
        let component = glyph.components().next().unwrap();
        assert_eq!(period_idx, component.glyph.to_u16() as u32);

        // If we all work together we're a real transform!
        assert_component_transform(
            &component,
            [0.84519, 0.58921, -1.16109, 1.66553, -233.0, -129.0],
        );
    }

    fn assert_component_transform(component: &glyf::Component, expected_transform: [f32; 6]) {
        let [xx, yx, xy, yy, dx, dy] = expected_transform;
        assert_eq!(
            glyf::Anchor::Offset {
                x: dx as i16,
                y: dy as i16
            },
            component.anchor
        );
        assert_eq!(
            glyf::Transform {
                xx: F2Dot14::from_f32(xx),
                yx: F2Dot14::from_f32(yx),
                xy: F2Dot14::from_f32(xy),
                yy: F2Dot14::from_f32(yy),
            },
            component.transform
        );
    }

    #[test]
    fn compile_composite_glyphs_to_glyf_loca_applies_transforms() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");

        let gid = result.get_glyph_index("simple_transform_again").unwrap();
        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        let Some(glyf::Glyph::Composite(glyph)) = &glyphs[gid as usize] else {
            panic!("Expected a composite\n{glyphs:#?}");
        };

        let components: Vec<_> = glyph.components().collect();
        assert_eq!(2, components.len(), "{components:#?}");
        assert_component_transform(&components[0], [2.0, 0.0, 0.0, 1.5, 50.0, 50.0]);
        assert_component_transform(&components[1], [1.5, 0.0, 0.0, 2.0, 50.0, 50.0]);

        // Have ye bbox?
        assert_eq!(
            [425, 125, 800, 250],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
    }

    #[test]
    fn eliminate_2x2_transforms() {
        let result = TestCompile::compile("glyphs2/Component.glyphs", |mut args| {
            args.decompose_transformed_components = true;
            args
        });

        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();

        // Not an identity 2x2, should be simplified
        let Some(glyf::Glyph::Simple(..)) =
            &glyphs[result.get_glyph_index("simple_transform_again").unwrap() as usize]
        else {
            panic!("Expected a simple glyph\n{glyphs:#?}");
        };

        // Identity 2x2, should be left as a component
        let Some(glyf::Glyph::Composite(..)) =
            &glyphs[result.get_glyph_index("translate_only").unwrap() as usize]
        else {
            panic!("Expected a composite glyph\n{glyphs:#?}");
        };
    }

    #[test]
    fn decompose_all_components() {
        let result = TestCompile::compile("glyphs2/Component.glyphs", |mut args| {
            args.decompose_components = true;
            args
        });

        // We expect ALL composite glyphs to be simplified with --decompose-components
        let glyph_data = result.glyphs();
        for (i, glyph) in glyph_data.read().iter().enumerate() {
            let Some(glyf::Glyph::Simple(_)) = glyph else {
                panic!("Expected a simple glyph at index {i}");
            };
        }
    }

    #[test]
    fn writes_cmap() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");

        let raw_cmap = dump_table(result.be_context.cmap.get().as_ref()).unwrap();
        let font_data = FontData::new(&raw_cmap);
        let cmap = Cmap::read(font_data).unwrap();

        assert!(!cmap.encoding_records().is_empty());
        for encoding_record in cmap.encoding_records() {
            let CmapSubtable::Format4(cmap4) = encoding_record.subtable(font_data).unwrap() else {
                panic!("Wrong subtable {encoding_record:#?}");
            };

            // Hopefully we find the codepoints in the .glyphs file, with glyph ids based on order in that file
            let cp_and_gid: Vec<(u16, u16)> = cmap4
                .start_code()
                .iter()
                .zip(cmap4.end_code())
                .zip(cmap4.id_delta())
                .filter_map(|((start, end), id_delta)| {
                    let start = start.get();
                    let end = end.get();
                    if (start, end) == (0xFFFF, 0xFFFF) {
                        return None;
                    }
                    Some((start, end, id_delta.get()))
                })
                .flat_map(|(start, end, id_delta)| {
                    (start..=end)
                        .map(move |cp| (cp, (cp as i32 + id_delta as i32).try_into().unwrap()))
                })
                .collect();
            assert_eq!(
                vec![
                    (0x002C, 2),
                    (0x002E, 1),
                    (0x0030, 3),
                    (0x031, 4),
                    (0x032, 5),
                    (0x033, 6)
                ],
                cp_and_gid,
                "start {:?}\nend {:?}id_delta {:?}",
                cmap4.start_code(),
                cmap4.end_code(),
                cmap4.id_delta()
            );
        }
    }

    #[test]
    fn hmtx_of_one() {
        let result = TestCompile::compile_source("glyphs2/NotDef.glyphs");

        let raw_hmtx = result.be_context.hmtx.get();
        let hmtx = Hmtx::read_with_args(FontData::new(raw_hmtx.get()), &(1, 1)).unwrap();
        assert_eq!(
            vec![(600, 250)],
            hmtx.h_metrics()
                .iter()
                .map(|m| (m.advance.get(), m.side_bearing.get()))
                .collect::<Vec<_>>()
        );
        assert!(hmtx.left_side_bearings().is_empty());
    }

    #[test]
    fn metrics_and_limits_of_mono() {
        let result = TestCompile::compile_source("glyphs2/Mono.glyphs");

        let hhea = result.be_context.hhea.get();
        assert_eq!(1, hhea.number_of_h_metrics);
        assert_eq!(175, hhea.min_left_side_bearing.to_i16());
        assert_eq!(50, hhea.min_right_side_bearing.to_i16());
        assert_eq!(375, hhea.x_max_extent.to_i16());
        assert_eq!(425, hhea.advance_width_max.to_u16());

        let maxp = result.be_context.maxp.get();
        assert_eq!(3, maxp.num_glyphs);
        assert_eq!(Some(4), maxp.max_points);
        assert_eq!(Some(1), maxp.max_contours);

        let raw_hmtx = result.be_context.hmtx.get();
        let hmtx = Hmtx::read_with_args(
            FontData::new(raw_hmtx.get()),
            &(hhea.number_of_h_metrics, maxp.num_glyphs),
        )
        .unwrap();
        assert_eq!(
            vec![(425, 175)],
            hmtx.h_metrics()
                .iter()
                .map(|m| (m.advance.get(), m.side_bearing.get()))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![200, 225],
            hmtx.left_side_bearings()
                .iter()
                .map(|m| m.get())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn wght_var_has_expected_tables() {
        let result = TestCompile::compile_source("wght_var.designspace");
        let font = result.font();

        assert_eq!(
            vec![
                Tag::new(b"GDEF"),
                Tag::new(b"GPOS"),
                Tag::new(b"GSUB"),
                Tag::new(b"HVAR"),
                Tag::new(b"MVAR"),
                Tag::new(b"OS/2"),
                Tag::new(b"STAT"),
                Tag::new(b"cmap"),
                Tag::new(b"fvar"),
                Tag::new(b"glyf"),
                Tag::new(b"gvar"),
                Tag::new(b"head"),
                Tag::new(b"hhea"),
                Tag::new(b"hmtx"),
                Tag::new(b"loca"),
                Tag::new(b"maxp"),
                Tag::new(b"name"),
                Tag::new(b"post"),
            ],
            font.table_directory
                .table_records()
                .iter()
                .map(|tr| tr.tag())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_mov_xy_and_move_around() {
        let result = TestCompile::compile_source("mov_xy.designspace");
        let font = result.font();
        // Confirm movx then movy
        assert_eq!(
            vec!["movx", "movy"],
            font.fvar()
                .unwrap()
                .axes()
                .unwrap()
                .iter()
                .map(|a| a.axis_tag.get().to_string())
                .collect::<Vec<_>>()
        );

        // Confirm movement on axes has the intended effect
        assert_eq!(
            vec![
                Rect::new(50.0, 50.0, 150.0, 150.0),   // default
                Rect::new(850.0, 50.0, 950.0, 150.0),  // movx max
                Rect::new(50.0, 850.0, 150.0, 950.0),  // movy max
                Rect::new(850.0, 850.0, 950.0, 950.0), // movx and movy max
            ],
            vec![
                cbox_of_char(0x2e, &font, vec![0.0, 0.0]),
                cbox_of_char(0x2e, &font, vec![1.0, 0.0]),
                cbox_of_char(0x2e, &font, vec![0.0, 1.0]),
                cbox_of_char(0x2e, &font, vec![1.0, 1.0]),
            ]
        )
    }

    fn cbox_of_char(ch: u32, font: &FontRef, coords: Vec<f32>) -> Rect {
        let gid = Charmap::new(font).map(ch).unwrap();

        let mut bp = CboxPen::new();
        let coords = coords
            .into_iter()
            .map(F2Dot14::from_f32)
            .collect::<Vec<_>>();
        font.outline_glyphs()
            .get(gid)
            .unwrap()
            .draw(
                DrawSettings::unhinted(Size::new(1000.0), coords.as_slice()),
                &mut bp,
            )
            .unwrap();
        bp.cbox().unwrap()
    }

    struct CboxPen {
        last_move: Point,
        bounds: Option<Rect>,
    }

    impl CboxPen {
        fn new() -> Self {
            Self {
                last_move: Default::default(),
                bounds: None,
            }
        }

        fn cbox(self) -> Option<Rect> {
            self.bounds
        }

        fn update_bounds(&mut self, x: f32, y: f32) {
            let pt = Point::new(x as f64, y as f64);
            self.bounds = Some(match self.bounds {
                None => Rect::from_points(pt, pt),
                Some(bounds) => bounds.union_pt(pt),
            });
        }
    }

    impl skrifa::outline::OutlinePen for CboxPen {
        fn move_to(&mut self, x: f32, y: f32) {
            self.last_move = Point::new(x as f64, y as f64);
            self.update_bounds(x, y);
        }

        fn line_to(&mut self, x: f32, y: f32) {
            self.update_bounds(x, y);
        }

        fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
            self.update_bounds(cx0, cy0);
            self.update_bounds(x, y);
        }

        fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
            self.update_bounds(cx0, cy0);
            self.update_bounds(cx1, cy1);
            self.update_bounds(x, y);
        }

        fn close(&mut self) {
            // nop
        }
    }

    fn axes(font: &FontRef) -> Vec<(Tag, f32, f32, f32)> {
        font.fvar()
            .unwrap()
            .axes()
            .unwrap()
            .iter()
            .map(|a| {
                (
                    a.axis_tag(),
                    a.min_value.get().to_f64() as f32,
                    a.default_value.get().to_f64() as f32,
                    a.max_value.get().to_f64() as f32,
                )
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn compile_generates_notdef() {
        let result = TestCompile::compile_source("glyphs2/WghtVar.glyphs");

        assert!(!result
            .fe_context
            .preliminary_glyph_order
            .get()
            .contains(&GlyphName::NOTDEF));
        assert_eq!(
            Some(GlyphId16::NOTDEF),
            result
                .fe_context
                .glyph_order
                .get()
                .glyph_id(&GlyphName::NOTDEF)
        );

        let font = result.font();

        // Character 0x0000 (NULL) != '.notdef' glyph, and neither are any other
        // characters actually, because '.notdef' (glyph index 0) means the absence
        // of a character-to-glyph mapping:
        // https://github.com/googlefonts/fontc/pull/423/files#r1309257127
        // https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#overview
        assert_eq!(None, font.cmap().unwrap().map_codepoint(0u32));
    }

    #[test]
    fn compile_glyphs_font_with_weight_axis() {
        let result = TestCompile::compile_source("glyphs2/WghtVar.glyphs");
        let font = result.font();

        assert_eq!(vec![(Tag::new(b"wght"), 400.0, 400.0, 700.0)], axes(&font),);

        assert_eq!(
            vec![
                GlyphId16::new(1),
                GlyphId16::new(2),
                GlyphId16::new(3),
                GlyphId16::new(6),
            ],
            [0x20, 0x21, 0x2d, 0x3d]
                .iter()
                .map(|cp| font.cmap().unwrap().map_codepoint(*cp as u32).unwrap())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_glyphs_font_with_avar() {
        let result = TestCompile::compile_source("glyphs2/WghtVar_Avar.glyphs");
        let font = result.font();
        // Default 400 is important, it means we found the index of Regular
        assert_eq!(vec![(Tag::new(b"wght"), 300.0, 400.0, 700.0)], axes(&font),);

        let axis_maps: Vec<_> = font
            .avar()
            .unwrap()
            .axis_segment_maps()
            .iter()
            .map(|m| m.unwrap())
            .map(|m| {
                m.axis_value_maps()
                    .iter()
                    .map(|e| (e.from_coordinate().to_f32(), e.to_coordinate().to_f32()))
                    .collect::<Vec<_>>()
            })
            .collect();

        // 0 is 400, 1 is 700, .33 => .66 is mapping 500 => 600 as per instance definition
        assert_eq!(
            vec![vec![
                (-1.0, -1.0),
                (0.0, 0.0),
                (0.333313, 0.666687),
                (1.0, 1.0)
            ]],
            axis_maps
        );
    }

    #[test]
    fn compile_without_ir() {
        let result = TestCompile::compile("glyphs2/WghtVar.glyphs", |mut args| {
            args.emit_ir = false;
            args
        });

        let outputs = fs::read_dir(&result.build_dir)
            .unwrap()
            .map(|e| {
                e.unwrap()
                    .path()
                    .strip_prefix(&result.build_dir)
                    .unwrap()
                    .to_owned()
            })
            .collect::<Vec<_>>();
        assert_eq!(vec![Path::new("font.ttf")], outputs);
    }

    fn resolve_name(name: &Name, id: NameId) -> Option<String> {
        name.name_record().iter().find_map(|nr| {
            (nr.name_id() == id).then(|| {
                nr.string(name.string_data())
                    .unwrap()
                    .chars()
                    .collect::<String>()
            })
        })
    }

    #[test]
    fn standard_axis_names() {
        // test that we match fonttools' naming of standard axes
        // https://github.com/googlefonts/fontc/issues/1020
        let result = TestCompile::compile_source("glyphs3/StandardAxisNames.glyphs");
        let static_metadata = result.fe_context.static_metadata.get();

        assert_eq!(
            vec![
                "weight".to_string(),
                "width".to_string(),
                "italic".to_string(),
                "slant".to_string(),
                "optical".to_string(),
                "foobarbaz".to_string(),
            ],
            static_metadata
                .axes
                .iter()
                .map(|axis| axis.name.clone())
                .collect::<Vec<_>>()
        );

        let font = result.font();
        let name = font.name().unwrap();
        let fvar = font.fvar().unwrap();

        assert_eq!(
            vec![
                "Weight".to_string(),
                "Width".to_string(),
                "Italic".to_string(),
                "Slant".to_string(),
                "Optical Size".to_string(),
                // This axis is not 'standard' so its UI label was not renamed
                "foobarbaz".to_string(),
            ],
            fvar.axes()
                .unwrap()
                .iter()
                .map(|axis| resolve_name(&name, axis.axis_name_id()).unwrap())
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn custom_english_ui_label_name() {
        // The 'weight' axis has a custom `<labelname xml:lang="en">Heaviness</labelname>`;
        // we expect this to be used for the axis name for UI purposes (fvar, STAT).
        let result = TestCompile::compile_source("wght_var_axis_label_names.designspace");
        let static_metadata = result.fe_context.static_metadata.get();

        assert_eq!(
            vec!["weight".to_string()],
            static_metadata
                .axes
                .iter()
                .map(|axis| axis.name.clone())
                .collect::<Vec<_>>()
        );

        let font = result.font();
        let name = font.name().unwrap();
        let fvar = font.fvar().unwrap();

        assert_eq!(
            vec!["Heaviness".to_string()],
            fvar.axes()
                .unwrap()
                .iter()
                .map(|axis| resolve_name(&name, axis.axis_name_id()).unwrap())
                .collect::<Vec<_>>()
        );

        let stat = font.stat().unwrap();

        assert_eq!(
            vec!["Heaviness".to_string()],
            stat.design_axes()
                .unwrap()
                .iter()
                .map(|axis| resolve_name(&name, axis.axis_name_id()).unwrap())
                .collect::<Vec<_>>()
        )
    }

    fn assert_named_instances(source: &str, expected: Vec<(String, Vec<(&str, f64)>)>) {
        let result = TestCompile::compile_source(source);
        let font = result.font();

        let name = font.name().unwrap();
        let fvar = font.fvar().unwrap();
        let axis_names: Vec<_> = fvar
            .axes()
            .unwrap()
            .iter()
            .map(|axis| resolve_name(&name, axis.axis_name_id()).unwrap())
            .collect();
        let instances = fvar.instances().unwrap();

        assert_eq!(
            expected,
            instances
                .iter()
                .map(|i| i.unwrap())
                .map(|inst| (
                    resolve_name(&name, inst.subfamily_name_id)
                        .unwrap_or_else(|| format!("{:?}", inst.subfamily_name_id)),
                    inst.coordinates
                        .iter()
                        .enumerate()
                        .map(|(i, coord)| { (axis_names[i].as_str(), coord.get().to_f64()) })
                        .collect(),
                ))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_named_instances_from_glyphs() {
        assert_named_instances(
            "glyphs3/WghtVar_Instances.glyphs",
            vec![
                ("Regular".to_string(), vec![("Weight", 400.0)]),
                ("Bold".to_string(), vec![("Weight", 700.0)]),
            ],
        );
    }

    #[test]
    fn compile_named_instances_from_designspace() {
        assert_named_instances(
            "wght_var.designspace",
            vec![
                ("Regular".to_string(), vec![("Weight", 400.0)]),
                ("Bold".to_string(), vec![("Weight", 700.0)]),
            ],
        );
    }

    #[test]
    fn compile_named_instances_from_designspace_with_stylename_attrs() {
        assert_named_instances(
            "wght_var_instance_stylename.designspace",
            vec![
                ("Foo".to_string(), vec![("Weight", 400.0)]),
                ("Bar".to_string(), vec![("Weight", 700.0)]),
            ],
        );
    }

    fn assert_fs_selection(source: &str, expected: SelectionFlags) {
        let result = TestCompile::compile_source(source);
        let font = result.font();
        let os2 = font.os2().unwrap();
        assert_eq!(expected, os2.fs_selection());
    }

    #[test]
    fn wght_var_fs_selection() {
        assert_fs_selection(
            "glyphs3/WghtVar.glyphs",
            SelectionFlags::REGULAR | SelectionFlags::USE_TYPO_METRICS | SelectionFlags::WWS,
        );
    }

    #[test]
    fn static_bold_italic_fs_selection() {
        assert_fs_selection(
            "glyphs3/StaticBoldItalic.glyphs",
            SelectionFlags::BOLD | SelectionFlags::ITALIC,
        );
    }

    #[test]
    fn populates_unicode_range() {
        let result = TestCompile::compile_source("static.designspace");
        let font = result.font();
        let os2 = font.os2().unwrap();

        assert_eq!(
            (1, 1 << 6, 0, 0),
            (
                os2.ul_unicode_range_1(),
                os2.ul_unicode_range_2(),
                os2.ul_unicode_range_3(),
                os2.ul_unicode_range_4()
            ),
            "Should set bit 1, Latin-1 Supplement and bit 38, Math Operators",
        );
    }

    #[test]
    fn prefers_explicit_os_x_class() {
        let result = TestCompile::compile_source("fontinfo.designspace");
        let font = result.font();
        let os2 = font.os2().unwrap();

        assert_eq!((800, 8), (os2.us_weight_class(), os2.us_width_class(),),);
    }

    #[test]
    fn glyphs_app_ir_categories() {
        let result = TestCompile::compile_source("glyphs3/Oswald-glyph-categories.glyphs");
        let staticmeta = result.fe_context.static_metadata.get();
        let categories = &staticmeta.gdef_categories.categories;
        assert_eq!(categories.get("a"), Some(&GlyphClassDef::Base));
        assert_eq!(categories.get("acutecomb"), Some(&GlyphClassDef::Mark));
        assert_eq!(categories.get("brevecomb"), Some(&GlyphClassDef::Mark));
        assert_eq!(
            categories.get("brevecomb_acutecomb"),
            Some(&GlyphClassDef::Mark)
        );
        assert_eq!(categories.get("f_a"), Some(&GlyphClassDef::Ligature));
        assert_eq!(categories.get(".notdef"), None);
    }

    #[test]
    fn glyphs_app_bracket_glyph_categories() {
        let result = TestCompile::compile_source("glyphs3/PropagateAnchorsTest.glyphs");
        let staticmeta = result.fe_context.static_metadata.get();
        let categories = &staticmeta.gdef_categories.categories;
        assert_eq!(
            categories.get("A.BRACKET.varAlt01"),
            Some(&GlyphClassDef::Base)
        );
        assert_eq!(
            categories.get("Aacute.BRACKET.varAlt01"),
            Some(&GlyphClassDef::Base)
        );
        assert_eq!(categories.get(".notdef"), None);
    }

    #[test]
    fn ufo_app_ir_categories_matches_glyphs() {
        let glyphs = TestCompile::compile_source("glyphs3/Oswald-glyph-categories.glyphs");
        let ufo = TestCompile::compile_source("Oswald-glyph-categories/Oswald-Regular.designspace");

        let gmeta = glyphs.fe_context.static_metadata.get();
        let ufometa = ufo.fe_context.static_metadata.get();

        assert_eq!(
            gmeta.gdef_categories.categories,
            ufometa.gdef_categories.categories
        )
    }

    #[test]
    fn ufo_and_glyphs_set_prefer_fea_flag() {
        let glyphs = TestCompile::compile_source("glyphs3/Oswald-glyph-categories.glyphs");
        let ufo = TestCompile::compile_source("Oswald-glyph-categories/Oswald-Regular.designspace");

        let gmeta = glyphs.fe_context.static_metadata.get();
        let ufometa = ufo.fe_context.static_metadata.get();
        assert!(!gmeta.gdef_categories.prefer_gdef_categories_in_fea);
        assert!(ufometa.gdef_categories.prefer_gdef_categories_in_fea);
    }

    #[test]
    fn captures_vendor_id() {
        let result = TestCompile::compile_source("glyphs3/TheBestNames.glyphs");
        let font = result.font();

        let os2 = font.os2().unwrap();
        assert_eq!(Tag::new(b"RODS"), os2.ach_vend_id());
    }

    fn assert_maxp_composite(
        src: &str,
        max_component_depth: u16,
        max_composite_points: u16,
        max_composite_contours: u16,
    ) {
        let result = TestCompile::compile_source(src);
        let font = result.font();
        let maxp = font.maxp().unwrap();

        assert_eq!(
            (
                max_component_depth,
                max_composite_points,
                max_composite_contours
            ),
            (
                maxp.max_component_depth().unwrap(),
                maxp.max_composite_points().unwrap(),
                maxp.max_composite_contours().unwrap()
            )
        );
    }

    #[test]
    fn maxp_with_no_components() {
        assert_maxp_composite("glyphs3/StaticBoldItalic.glyphs", 0, 0, 0);
    }

    #[test]
    fn maxp_with_shallow_components() {
        assert_maxp_composite("glyphs2/Component.glyphs", 1, 8, 2);
    }

    #[test]
    fn maxp_with_deep_components() {
        assert_maxp_composite("glyphs3/NestedComponent.glyphs", 3, 12, 3);
    }

    fn assert_created_set(source: &str) {
        let result = TestCompile::compile_source(source);
        let font = result.font();
        let head = font.head().unwrap();

        // TIL Mac has an epoch of it's own
        let mac_epoch = Utc.with_ymd_and_hms(1904, 1, 1, 0, 0, 0).unwrap();
        let created = mac_epoch
            .checked_add_signed(Duration::try_seconds(head.created().as_secs()).unwrap())
            .unwrap();

        // We should match 2023-05-05 15:11:55 +0000
        assert_eq!(
            Utc.with_ymd_and_hms(2023, 5, 5, 15, 11, 55).unwrap(),
            created
        );
    }

    #[test]
    fn captures_created_from_glyphs() {
        assert_created_set("glyphs3/Dated.glyphs");
    }

    #[test]
    fn captures_created_from_designspace() {
        assert_created_set("wght_var.designspace");
    }

    #[test]
    fn generates_stat() {
        let result = TestCompile::compile_source("wght_var.designspace");
        let font = result.font();

        let name = font.name().unwrap();
        let stat = font.stat().unwrap();
        assert_eq!(
            ("Regular", vec![Tag::new(b"wght"),]),
            (
                resolve_name(&name, stat.elided_fallback_name_id().unwrap())
                    .unwrap()
                    .as_str(),
                stat.design_axes()
                    .unwrap()
                    .iter()
                    .map(|ar| ar.axis_tag())
                    .collect::<Vec<_>>(),
            )
        );
    }

    fn assert_simple_kerning(source: &str) {
        let result = TestCompile::compile_source(source);

        let kerning_groups = result.fe_context.kerning_groups.get();

        let mut groups: Vec<_> = kerning_groups
            .groups
            .iter()
            .map(|(name, entries)| {
                let mut entries: Vec<_> = entries.iter().map(|e| e.as_str()).collect();
                entries.sort();
                (name.to_owned(), entries)
            })
            .collect();
        groups.sort();

        let wght = Tag::new(b"wght");
        let mut kerns: HashMap<KernPair, Vec<(String, f64)>> = HashMap::new();
        for kern_loc in kerning_groups.locations.iter() {
            assert_eq!(
                vec![wght],
                kern_loc.axis_tags().cloned().collect::<Vec<_>>()
            );

            let kerns_at = result
                .fe_context
                .kerning_at
                .get(&FeWorkIdentifier::KernInstance(kern_loc.clone()));
            for (pair, adjustment) in kerns_at.kerns.iter() {
                kerns.entry(pair.clone()).or_default().push((
                    format!(
                        "wght {}",
                        kern_loc.iter().map(|(_, v)| *v).next().unwrap().to_f64()
                    ),
                    adjustment.0,
                ));
            }
        }
        let mut kerns: Vec<_> = kerns
            .into_iter()
            .map(|((left, right), mut values)| {
                values.sort_by_key(|(s, _)| s.clone());
                (left, right, values)
            })
            .collect();
        kerns.sort_by_key(|(left, right, _)| (left.clone(), right.clone()));

        assert_eq!(
            (groups, kerns),
            (
                vec![
                    (
                        KernGroup::Side1("bracketleft_R".into()),
                        vec!["bracketleft"],
                    ),
                    (
                        KernGroup::Side1("bracketright_R".into()),
                        vec!["bracketright"],
                    ),
                    (
                        KernGroup::Side2("bracketleft_L".into()),
                        vec!["bracketleft"],
                    ),
                    (
                        KernGroup::Side2("bracketright_L".into()),
                        vec!["bracketright"],
                    ),
                ],
                vec![
                    (
                        KernSide::Glyph("bracketleft".into()),
                        KernSide::Glyph("bracketright".into()),
                        vec![
                            ("wght 0".to_string(), -300.0),
                            ("wght 1".to_string(), -150.0)
                        ],
                    ),
                    (
                        KernSide::Glyph("exclam".into()),
                        KernSide::Glyph("exclam".into()),
                        vec![
                            ("wght 0".to_string(), -360.0),
                            ("wght 1".to_string(), -100.0)
                        ],
                    ),
                    (
                        KernSide::Glyph("exclam".into()),
                        KernSide::Glyph("hyphen".into()),
                        vec![("wght 0".to_string(), 20.0),],
                    ),
                    (
                        KernSide::Glyph("exclam".into()),
                        KernSide::Group(KernGroup::Side2("bracketright_L".into())),
                        vec![("wght 0".to_string(), -160.0),],
                    ),
                    (
                        KernSide::Glyph("hyphen".into()),
                        KernSide::Glyph("hyphen".into()),
                        vec![
                            ("wght 0".to_string(), -150.0),
                            ("wght 1".to_string(), -50.0)
                        ],
                    ),
                    (
                        KernSide::Group(KernGroup::Side1("bracketleft_R".into())),
                        KernSide::Glyph("exclam".into()),
                        vec![("wght 0".to_string(), -165.0),],
                    ),
                ],
            )
        );
    }

    #[test]
    fn kerning_from_glyphs() {
        assert_simple_kerning("glyphs3/WghtVar.glyphs");
    }

    #[test]
    fn kerning_from_ufo() {
        assert_simple_kerning("designspace_from_glyphs/WghtVar.designspace");
    }

    fn assert_intermediate_layer(src: &str) {
        let result = TestCompile::compile_source(src);
        let font = result.font();

        assert_eq!(
            vec!["CPHT", "wght"],
            font.fvar()
                .unwrap()
                .axes()
                .unwrap()
                .iter()
                .map(|a| a.axis_tag().to_string())
                .collect::<Vec<_>>()
        );

        // Confirm movement on the wght axis is as expected:
        // glyph "i" (char 0x69) defines three masters at wght=400, 700 and 900
        assert_eq!(
            vec![
                Rect::new(239.0, 0.0, 364.0, 705.0),
                Rect::new(213.0, 0.0, 388.0, 737.0),
                Rect::new(191.0, 0.0, 410.0, 761.0),
            ],
            vec![
                cbox_of_char(0x69, &font, vec![0.0, 0.0]), // CPHT=700, wght=400
                cbox_of_char(0x69, &font, vec![0.0, 0.6]), // CPHT=700, wght=700
                cbox_of_char(0x69, &font, vec![0.0, 1.0]), // CPHT=700, wght=900
            ]
        );

        // glyph "I" (char 0x49) only defines two masters along wght at 400 and 900
        // (and default CPHT=700) so wght=700 should be interpolated between them.
        assert_eq!(
            vec![
                Rect::new(231.0, 0.0, 364.0, 700.0),
                Rect::new(195.0, 0.0, 400.0, 700.0),
                Rect::new(171.0, 0.0, 424.0, 700.0),
            ],
            vec![
                cbox_of_char(0x49, &font, vec![0.0, 0.0]), // CPHT=700, wght=400
                cbox_of_char(0x49, &font, vec![0.0, 0.6]), // CPHT=700, wght=700
                cbox_of_char(0x49, &font, vec![0.0, 1.0]), // CPHT=700, wght=900
            ]
        );

        // glyph "I" also defines two additional ('virtual') masters along CPHT at 600 and 800
        // (and default wght=400), check that the cap-height (bbox.yMax) matches that throughout
        // the wght axis.
        assert_eq!(
            vec![
                Rect::new(231.0, 0.0, 364.0, 600.0),
                Rect::new(171.0, 0.0, 424.0, 600.0),
                Rect::new(231.0, 0.0, 364.0, 800.0),
                Rect::new(171.0, 0.0, 424.0, 800.0),
            ],
            vec![
                cbox_of_char(0x49, &font, vec![-1.0, 0.0]), // CPHT=600, wght=400
                cbox_of_char(0x49, &font, vec![-1.0, 1.0]), // CPHT=600, wght=900
                cbox_of_char(0x49, &font, vec![1.0, 0.0]),  // CPHT=800, wght=400
                cbox_of_char(0x49, &font, vec![1.0, 1.0]),  // CPHT=800, wght=900
            ]
        );

        // interediate glyph layers are expected to be ignored when building variations
        // for global font metrics or OpenType Layout features; the "IntermediateLayer" test
        // font contains a kerning pair between 'i' and 'i', defined at Regular (wght=400 or 0.0)
        // and Black (wght=900 or 1.0 normalized); the fact that 'i' or other glyphs define
        // additional intermediate layers should not influence the GDEF variation regions used
        // for GPOS kerning:
        // https://github.com/googlefonts/fontc/issues/407
        let gdef = font.gdef().unwrap();
        let varstore = gdef.item_var_store().unwrap().unwrap();
        let regions = varstore
            .variation_region_list()
            .unwrap()
            .variation_regions();
        assert_eq!(regions.len(), 1);
        let region_coords = regions
            .get(0)
            .unwrap()
            .region_axes()
            .iter()
            .map(|coords| {
                [
                    coords.start_coord().to_f32(),
                    coords.peak_coord().to_f32(),
                    coords.end_coord().to_f32(),
                ]
            })
            .collect::<Vec<_>>();
        assert_eq!(region_coords, vec![[0.0, 0.0, 0.0], [0.0, 1.0, 1.0]]);
        assert_eq!(varstore.item_variation_data_count(), 1, "{varstore:#?}");
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(vec![vec![-350]], delta_sets(&vardata));
    }

    #[test]
    fn intermediate_layer_in_glyphs2() {
        // https://github.com/googlefonts/fontc/issues/62
        assert_intermediate_layer("glyphs2/IntermediateLayer.glyphs");
    }

    #[test]
    fn intermediate_layer_in_glyphs3() {
        // https://github.com/googlefonts/fontc/issues/62
        assert_intermediate_layer("glyphs3/IntermediateLayer.glyphs");
    }

    #[test]
    fn intermediate_layer_in_designspace() {
        // https://github.com/googlefonts/fontc/issues/400
        assert_intermediate_layer("designspace_from_glyphs/IntermediateLayer.designspace");
    }

    #[test]
    fn empty_glyph_is_zero_length() {
        let result = TestCompile::compile_source("glyphs2/WghtVar.glyphs");
        let space_idx = result.get_glyph_index("space").unwrap() as usize;
        let font = result.font();

        let is_long = match font.head().unwrap().index_to_loc_format() {
            0 => false,
            1 => true,
            _ => panic!("Unrecognized loca format"),
        };
        let loca = font.loca(is_long).unwrap();
        assert_eq!(
            loca.get_raw(space_idx).unwrap(),
            loca.get_raw(space_idx + 1).unwrap(),
            "space has no outline and should take 0 bytes of glyf"
        );
    }

    fn assert_no_compile_features(source: &str, table_tags: &[&[u8; 4]]) {
        // default is to skip_features=false
        let default_result = TestCompile::compile_source(source);

        let nofea_result = TestCompile::compile(source, |mut args| {
            args.skip_features = true;
            args
        });

        let font_with_fea = default_result.font();
        let font_without_fea = nofea_result.font();

        for table_tag in table_tags {
            let tag = Tag::new(table_tag);
            assert!(
                font_with_fea.data_for_tag(tag).is_some(),
                "Expected font built from {source} with fea to have {tag}"
            );
            assert!(
                font_without_fea.data_for_tag(tag).is_none(),
                "Expected font build from {source} without fea to not have {tag}"
            );
        }
    }

    #[test]
    fn no_compile_features_from_glyphs_2() {
        assert_no_compile_features("glyphs2/WghtVar.glyphs", &[b"GDEF", b"GPOS"]);
    }

    #[test]
    fn no_compile_features_from_glyphs_3() {
        assert_no_compile_features("glyphs3/WghtVar.glyphs", &[b"GDEF", b"GPOS"]);
    }

    #[test]
    fn no_compile_features_from_ufo() {
        assert_no_compile_features(
            "designspace_from_glyphs/WghtVar.designspace",
            &[b"GDEF", b"GPOS"],
        );
        assert_no_compile_features("static.designspace", &[b"GPOS", b"GSUB"]);
    }

    #[test]
    fn compile_simple_glyph_keep_direction() {
        let source = "glyphs3/WghtVar.glyphs";

        // first compile with default args (keep_direction=false)
        let default_result = TestCompile::compile_source(source);

        let ir_glyph = read_ir_glyph(&default_result.build_dir, "hyphen");
        let ir_default_instance = ir_glyph.default_instance();
        assert_eq!(
            &ir_default_instance.contours[0].to_svg(),
            "M131,330 L131,250 L470,250 L470,330 L131,330 Z"
        );

        let RawGlyph::Simple(glyph) = read_be_glyph(&default_result.build_dir, "hyphen") else {
            panic!("Expected a simple glyph");
        };

        // point order in the compiled glyph is reversed compared to the IR glyph
        // while the starting point is the same
        assert_eq!(
            vec![
                CurvePoint::on_curve(131, 330),
                CurvePoint::on_curve(470, 330),
                CurvePoint::on_curve(470, 250),
                CurvePoint::on_curve(131, 250),
            ],
            glyph
                .contours
                .iter()
                .flat_map(|c| c.iter())
                .copied()
                .collect::<Vec<_>>()
        );
        drop(default_result); // let's not use that by mistake

        // recompile with --keep-direction
        let keep_direction_result = TestCompile::compile(source, |mut args| {
            args.keep_direction = true;
            args
        });

        let RawGlyph::Simple(glyph) = read_be_glyph(&keep_direction_result.build_dir, "hyphen")
        else {
            panic!("Expected a simple glyph");
        };

        // order/starting point in the compiled glyph are the same as in the IR glyph
        assert_eq!(
            vec![
                CurvePoint::on_curve(131, 330),
                CurvePoint::on_curve(131, 250),
                CurvePoint::on_curve(470, 250),
                CurvePoint::on_curve(470, 330),
            ],
            glyph
                .contours
                .iter()
                .flat_map(|c| c.iter())
                .copied()
                .collect::<Vec<_>>()
        );
    }

    fn delta_sets(var_data: &ItemVariationData) -> Vec<Vec<i32>> {
        (0..var_data.item_count())
            .map(|i| var_data.delta_set(i).collect::<Vec<_>>())
            .collect()
    }

    #[test]
    fn compile_hvar_single_model_direct_varstore() {
        // All glyphs define the same locations (single model is ok) so a direct
        // VarStore (without a mapping) can be built. In this particular case, this
        // turns out to be more compact than the equivalent indirect VarStore
        // so we check it gets preferred over the latter.
        let result = TestCompile::compile_source(
            "HVAR/SingleModel_Direct/HVARSingleModelDirect.designspace",
        );
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 14);
        let hvar = font.hvar().unwrap();
        assert!(hvar.advance_width_mapping().is_none());
        let varstore = hvar.item_variation_store().unwrap();
        let regions = varstore
            .variation_region_list()
            .unwrap()
            .variation_regions();
        // this simple test font only has two masters (Regular [default] and Bold)
        // so we expect a single region with a single wght axis
        assert_eq!(regions.len(), 1);
        let region_coords = regions
            .get(0)
            .unwrap()
            .region_axes()
            .iter()
            .map(|coords| {
                [
                    coords.start_coord().to_f32(),
                    coords.peak_coord().to_f32(),
                    coords.end_coord().to_f32(),
                ]
            })
            .collect::<Vec<_>>();
        assert_eq!(region_coords, vec![[0.0, 1.0, 1.0]]);
        // we expect one ItemVariationData and one delta set per glyph
        assert_eq!(varstore.item_variation_data_count(), 1, "{varstore:#?}");
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(
            vec![
                vec![0],
                vec![100],
                vec![120],
                vec![70],
                vec![100],
                vec![120],
                vec![120],
                vec![70],
                vec![70],
                vec![120],
                vec![70],
                vec![0],
                vec![0],
                vec![0],
            ],
            delta_sets(&vardata)
        );
    }

    #[test]
    fn compile_hvar_single_model_direct_varstore_from_glyphs2_with_implicit_axes() {
        // This test font is a Glyphs 2 source without an 'Axes' custom parameter, so
        // the axes default to "Weight", "Width", and "Custom". However, the two masters
        // only define coordinates along Weight, so the other two are so-called 'point'
        // axes which don't vary and are discarded by the VariationModel. The IR Glyph
        // instances' locations still contain these static 'point' axes. When building
        // HVAR, we must prune these out lest their presence would trigger the creation
        // of a multi-model indirect VarStore where a single-model direct one would suffice.
        // https://github.com/googlefonts/fontc/issues/1256
        let result = TestCompile::compile_source("glyphs2/WghtVar_ImplicitAxes.glyphs");
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 4);
        let hvar = font.hvar().unwrap();
        // we expect a 'direct' VarStore with implicit variation indices and no mapping
        assert!(hvar.advance_width_mapping().is_none());
        let varstore = hvar.item_variation_store().unwrap();
        let regions = varstore
            .variation_region_list()
            .unwrap()
            .variation_regions();
        // this simple test font only has two masters (Regular [default] and Bold)
        // so we expect a single region with a single wght axis
        assert_eq!(regions.len(), 1);
        let region_coords = regions
            .get(0)
            .unwrap()
            .region_axes()
            .iter()
            .map(|coords| {
                [
                    coords.start_coord().to_f32(),
                    coords.peak_coord().to_f32(),
                    coords.end_coord().to_f32(),
                ]
            })
            .collect::<Vec<_>>();
        assert_eq!(region_coords, vec![[0.0, 1.0, 1.0]]);
        // we expect one ItemVariationData and 4 delta sets, one per glyph
        assert_eq!(varstore.item_variation_data_count(), 1, "{varstore:#?}");
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(
            vec![vec![0], vec![400], vec![200], vec![200],],
            delta_sets(&vardata)
        );
    }

    #[test]
    fn compile_hvar_single_model_indirect_varstore() {
        // This is the same as the previous font but with additional 10 glyphs that
        // share the same deltas (the number was found empirically), enough to tip
        // the balance and make an indirect store more compact despite the overhead
        // of the additional DeltaSetIndexMap.
        let result = TestCompile::compile_source(
            "HVAR/SingleModel_Indirect/HVARSingleModelIndirect.designspace",
        );
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 24);
        let hvar = font.hvar().unwrap();
        let varstore = hvar.item_variation_store().unwrap();
        assert_eq!(
            varstore
                .variation_region_list()
                .unwrap()
                .variation_regions()
                .len(),
            1
        );
        assert_eq!(varstore.item_variation_data_count(), 1);
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(
            vec![vec![0], vec![70], vec![100], vec![120],],
            delta_sets(&vardata)
        );
        let Some(Ok(DeltaSetIndexMap::Format0(varidx_map))) = hvar.advance_width_mapping() else {
            panic!("Expected advance width mapping with DeltaSetIndexMap::Format0");
        };
        // since the last 10 glyphs have the same advance width deltas and share the same varidx,
        // the mapping omits the trailing duplicate entries
        assert_eq!(varidx_map.map_count(), num_glyphs - 10 + 1);
    }

    struct HvarReader<'a> {
        hvar: write_fonts::read::tables::hvar::Hvar<'a>,
        glyph_order: Arc<GlyphOrder>,
    }

    impl<'a> HvarReader<'a> {
        fn new(
            hvar: write_fonts::read::tables::hvar::Hvar<'a>,
            glyph_order: Arc<GlyphOrder>,
        ) -> Self {
            Self { hvar, glyph_order }
        }

        fn width_delta(&self, name: &str, coords: &[NormalizedCoord]) -> f64 {
            let name = GlyphName::from(name);
            let gid = self.glyph_order.glyph_id(&name).unwrap();
            let coords: Vec<F2Dot14> = coords
                .iter()
                .map(|coord| F2Dot14::from_f32(coord.to_f64() as _))
                .collect();
            self.hvar
                .advance_width_delta(gid.into(), &coords)
                .unwrap()
                .to_f64()
        }
    }

    #[test]
    fn compile_hvar_multi_model_indirect_varstore() {
        // Some glyphs are 'sparse' and define different sets of locations, so multiple
        // sub-models are required to compute the advance width deltas.
        // FontTools always builds an indirect VarStore in this case and we do the same.
        let result = TestCompile::compile_source(
            "HVAR/MultiModel_Indirect/HVARMultiModelIndirect.designspace",
        );
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 5);
        let hvar = font.hvar().unwrap();
        let varstore = hvar.item_variation_store().unwrap();
        let region_coords = varstore
            .variation_region_list()
            .unwrap()
            .variation_regions()
            .iter()
            .map(|region| {
                region
                    .unwrap()
                    .region_axes()
                    .iter()
                    .map(|coords| {
                        [
                            coords.start_coord().to_f32(),
                            coords.peak_coord().to_f32(),
                            coords.end_coord().to_f32(),
                        ]
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        // Glyph "A" defines three masters (Regular, Medium and Bold) so uses two
        // regions (the first two in the list); the rest of the glyphs only have
        // two masters so use only one region (the last one).
        assert_eq!(
            region_coords,
            vec![
                vec![[0.0, 0.333313, 1.0]],
                vec![[0.333313, 1.0, 1.0]],
                vec![[0.0, 1.0, 1.0]],
            ]
        );
        // in this particular test font, despite being multi-model, we still end up with
        // a single VarData subtable, filled with zeros for regions that don't apply
        assert_eq!(varstore.item_variation_data_count(), 1);
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0, 1, 2]);
        // 4 rows insted of 5 because gid1 and gid4 happen to share the same varidx
        assert_eq!(vardata.item_count(), 4);
        let Some(Ok(varidx_map)) = hvar.advance_width_mapping() else {
            panic!("Expected advance width mapping");
        };
        assert_eq!(
            (0..num_glyphs)
                .map(|gid| {
                    let varidx = varidx_map.get(gid as u32).unwrap();
                    assert_eq!(varidx.outer, 0);
                    varidx.inner
                })
                .collect::<Vec<_>>(),
            vec![0, 2, 3, 1, 2],
        );
        let hvar = HvarReader::new(hvar, result.fe_context.glyph_order.get());
        // .notdef is not variable
        assert_eq!(
            hvar.width_delta(".notdef", &[NormalizedCoord::new(0.0)]),
            0.0
        );
        // 'space' has two masters, with Bold 100 units wider than Regular
        assert_eq!(
            hvar.width_delta("space", &[NormalizedCoord::new(1.0)]),
            100.0
        );
        // ... so it will be exactly 50 units wider half-way in between
        assert_eq!(
            hvar.width_delta("space", &[NormalizedCoord::new(0.5)]),
            50.0
        );
        // 'A' is 120 units wider than Regular in the Bold master
        assert_eq!(hvar.width_delta("A", &[NormalizedCoord::new(1.0)]), 120.0);
        // ... but also has an additional Medium (wght=500, normalized 0.3333) master with
        // a delta of 70; so at normalized 0.5 (wght=550), its delta will *not* be 60
        assert_eq!(hvar.width_delta("A", &[NormalizedCoord::new(0.5)]), 83.0);
    }

    fn anchor_coords(at: AnchorTable) -> (i32, i32) {
        match at {
            AnchorTable::Format1(at) => (at.x_coordinate() as i32, at.y_coordinate() as i32),
            AnchorTable::Format2(at) => (at.x_coordinate() as i32, at.y_coordinate() as i32),
            AnchorTable::Format3(at) => (at.x_coordinate() as i32, at.y_coordinate() as i32),
        }
    }

    fn mark_base_lookups<'a>(gpos: &'a Gpos) -> Vec<TableRef<'a, MarkBasePosFormat1Marker>> {
        // If only we had more indirections
        gpos.lookup_list()
            .iter()
            .flat_map(|l| l.lookups().iter().map(|l| l.unwrap()))
            .filter_map(|l| match l {
                PositionLookup::MarkToBase(mark_base) => Some(mark_base),
                _ => None,
            })
            .flat_map(|mb| mb.subtables().iter().map(|s| s.unwrap()))
            .collect()
    }

    #[test]
    fn compile_basic_gpos_mark_base() {
        let result = TestCompile::compile_source("glyphs3/WghtVar_Anchors.glyphs");
        let font = result.font();
        let gpos = font.gpos().unwrap();

        let base_gid = result.get_gid("A");
        let macroncomb_gid = result.get_gid("macroncomb");
        let brevecomb_gid = result.get_gid("brevecomb");

        // If only we had more indirections
        let mark_base_lookups = mark_base_lookups(&gpos);

        let bases = mark_base_lookups
            .iter()
            .flat_map(|mb| mb.base_coverage().unwrap().iter().collect::<Vec<_>>())
            .zip(
                mark_base_lookups
                    .iter()
                    .flat_map(|mb| {
                        let base_array = mb.base_array().unwrap();
                        let data = base_array.offset_data();
                        base_array
                            .base_records()
                            .iter()
                            .map(move |r| (data, r.unwrap()))
                    })
                    .flat_map(|(data, b)| b.base_anchors(data).iter().map(|b| b.unwrap().unwrap()))
                    .map(anchor_coords),
            )
            .collect::<Vec<_>>();

        let marks = mark_base_lookups
            .iter()
            .flat_map(|mb| mb.mark_coverage().unwrap().iter().collect::<Vec<_>>())
            .zip(
                mark_base_lookups
                    .iter()
                    .flat_map(|mb| {
                        let mark_array = mb.mark_array().unwrap();
                        let data = mark_array.offset_data();
                        mb.mark_array()
                            .unwrap()
                            .mark_records()
                            .iter()
                            .map(move |mr| (data, mr))
                    })
                    .map(|(data, mr)| {
                        (
                            mr.mark_class(),
                            anchor_coords(mr.mark_anchor(data).unwrap()),
                        )
                    }),
            )
            .collect::<Vec<_>>();

        assert_eq!(
            (
                vec![(base_gid, (300, 700))],
                vec![
                    // (glyph id, (mark class, (anchor x, anchor y)))
                    (macroncomb_gid, (0, (300, 600))),
                    (brevecomb_gid, (0, (200, 500)))
                ]
            ),
            (bases, marks)
        );
    }

    fn assert_noexport(source: &str) {
        let result = TestCompile::compile_source(source);

        assert!(!result.contains_glyph("hyphen"));
        let fe_hyphen_consumer = result
            .fe_context
            .glyphs
            .get(&FeWorkIdentifier::Glyph("manual-component".into()));
        assert!(
            !fe_hyphen_consumer
                .default_instance()
                .components
                .iter()
                .any(|c| c.base == "hyphen"),
            "IR glyph should not reference hyphen {fe_hyphen_consumer:?}"
        );
        let be_hyphen_consumer = result
            .be_context
            .glyphs
            .get(&BeWorkIdentifier::GlyfFragment("manual-component".into()).into());
        assert!(
            be_hyphen_consumer.is_simple(),
            "{be_hyphen_consumer:?} should be a simple glyph"
        );
    }

    #[test]
    fn compile_obeys_no_export_glyphs() {
        assert_noexport("glyphs3/WghtVar_NoExport.glyphs");
    }

    #[test]
    fn compile_obeys_no_export_designspace() {
        assert_noexport("designspace_from_glyphs/WghtVar_NoExport.designspace");
    }

    #[test]
    fn compile_do_not_decompose_nested_no_export_glyphs() {
        let result = TestCompile::compile("glyphs3/NestedNoExportComponent.glyphs", |mut args| {
            args.flatten_components = true;
            args
        });

        assert!(!result.contains_glyph("_glyph"));
        for name in ["glyph2", "glyph3"] {
            let be_glyph = result
                .be_context
                .glyphs
                .get(&BeWorkIdentifier::GlyfFragment(name.into()).into());
            assert!(
                be_glyph.is_composite(),
                "{be_glyph:?} should be a composite glyph"
            );

            let fe_glyph = result
                .fe_context
                .glyphs
                .get(&FeWorkIdentifier::Glyph(name.into()));
            assert!(
                fe_glyph
                    .default_instance()
                    .components
                    .iter()
                    .all(|c| c.base == "glyph1"),
                "IR glyph should reference glyph1 {fe_glyph:?}"
            );
        }
    }

    fn assert_fs_type(source: &str, expected_fs_type: u16) {
        let compile = TestCompile::compile_source(source);
        let os2 = compile.font().os2().unwrap();

        assert_eq!(expected_fs_type, os2.fs_type());
    }

    #[test]
    fn default_fs_type_glyphs() {
        assert_fs_type("glyphs3/WghtVar.glyphs", 1 << 3);
    }

    #[test]
    fn default_fs_type_designspace() {
        assert_fs_type("wght_var.designspace", 1 << 2);
    }

    #[test]
    fn os2_weight_class_matches_default_wght() {
        let compile = TestCompile::compile_source("glyphs3/WghtVar_3master_CustomOrigin.glyphs");
        let font = compile.font();

        // the default value for 'wght' is 700 (Bold) in this test font
        assert_eq!(vec![(Tag::new(b"wght"), 200.0, 700.0, 700.0)], axes(&font),);
        // ... which is reflected in the OS/2 table usWeightClass
        assert_eq!(700, font.os2().unwrap().us_weight_class());
    }

    #[test]
    fn os2_width_class_matches_default_wdth_glyphs2() {
        let compile = TestCompile::compile_source("glyphs2/WdthVar.glyphs");
        let font = compile.font();

        // the default value for 'wdth' is 50 (UltraCondensed) in this test font
        assert_eq!(vec![(Tag::new(b"wdth"), 50.0, 50.0, 200.0)], axes(&font),);
        // ... which is reflected in the OS/2 table usWidthClass (UltraCondensed = 1)
        assert_eq!(
            WidthClass::UltraCondensed as u16,
            font.os2().unwrap().us_width_class()
        );
    }

    #[test]
    fn os2_width_class_matches_default_wdth_glyphs3() {
        let compile = TestCompile::compile_source("glyphs3/WdthVar.glyphs");
        let font = compile.font();

        // the default value for 'wdth' is 50 (UltraCondensed) in this test font
        assert_eq!(vec![(Tag::new(b"wdth"), 50.0, 50.0, 200.0)], axes(&font),);
        // ... which is reflected in the OS/2 table usWidthClass (UltraCondensed = 1)
        assert_eq!(
            WidthClass::UltraCondensed as u16,
            font.os2().unwrap().us_width_class()
        );
    }

    #[test]
    fn one_lookup_per_group() {
        let compile = TestCompile::compile_source("glyphs3/Oswald-AE-comb.glyphs");
        let gpos = compile.font().gpos().unwrap();

        // We had a bug where it was 2
        assert_eq!(1, mark_base_lookups(&gpos).len());
    }

    fn assert_post_italic_angle(source: &str, expected_angle: f32) {
        let compile = TestCompile::compile_source(source);
        let post = compile.font().post().unwrap();

        assert_eq!(expected_angle, post.italic_angle().to_f32());
    }

    #[test]
    fn italic_angle_in_glyphs2() {
        assert_post_italic_angle("glyphs2/SlantedFont.glyphs", -12.0);
    }

    #[test]
    fn italic_angle_in_glyphs3() {
        assert_post_italic_angle("glyphs3/SlantedFont.glyphs", -12.0);
    }

    #[test]
    fn italic_angle_in_designspace() {
        assert_post_italic_angle("designspace_from_glyphs/SlantedFont.designspace", -12.0);
    }

    fn assert_hhea_caret_slope(source: &str, expected_slope_rise: i16, expected_slope_run: i16) {
        let compile = TestCompile::compile_source(source);
        let hhea = compile.font().hhea().unwrap();

        assert_eq!(expected_slope_rise, hhea.caret_slope_rise());
        assert_eq!(expected_slope_run, hhea.caret_slope_run());
        assert_eq!(0, hhea.caret_offset());
    }

    #[test]
    fn default_caret_slope_in_glyphs2() {
        assert_hhea_caret_slope("glyphs2/SlantedFont.glyphs", 1000, 213);
    }

    #[test]
    fn default_caret_slope_in_glyphs3() {
        assert_hhea_caret_slope("glyphs3/SlantedFont.glyphs", 1000, 213);
    }

    #[test]
    fn default_caret_slope_in_designspace() {
        assert_hhea_caret_slope("designspace_from_glyphs/SlantedFont.designspace", 1000, 213);
    }

    fn assert_mvar(
        mvar: write_fonts::read::tables::mvar::Mvar,
        expected_value_records: Vec<(Tag, u16)>,
        expected_delta_sets: Vec<Vec<i32>>,
    ) {
        let actual_records = mvar
            .value_records()
            .iter()
            .map(|rec| {
                assert_eq!(rec.delta_set_outer_index(), 0);
                (rec.value_tag(), rec.delta_set_inner_index())
            })
            .collect::<Vec<_>>();
        assert_eq!(actual_records, expected_value_records);

        let Some(Ok(varstore)) = mvar.item_variation_store() else {
            panic!("MVAR has no ItemVariationStore?!");
        };

        assert!(!expected_delta_sets.is_empty());
        let expected_num_regions = expected_delta_sets[0].len() as u16;
        assert!(expected_num_regions > 0);
        assert!(expected_delta_sets
            .iter()
            .all(|row| row.len() as u16 == expected_num_regions));

        assert_eq!(
            varstore.variation_region_list().unwrap().region_count(),
            expected_num_regions
        );

        assert_eq!(varstore.item_variation_data_count(), 1);
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(
            vardata.region_indexes(),
            (0..expected_num_regions).collect::<Vec<_>>()
        );
        assert_eq!(delta_sets(&vardata), expected_delta_sets);
    }

    #[test]
    fn mvar_from_designspace() {
        // This test font contains 3 masters defined over the 'slnt' axis, the middle
        // one is upright, the other two are slanted backwards and forwards +/-5 degrees.
        // For the sake of simplicity, all variable global metrics in the back-slanted
        // master where offset by -5 units, and in the forward-slanted master by +5 units,
        // so their MVAR records will all point to the same delta set.
        let compile = TestCompile::compile_source("MVAR.designspace");

        assert_mvar(
            compile.font().mvar().unwrap(),
            vec![
                (Tag::new(b"cpht"), 0),
                (Tag::new(b"hasc"), 0),
                (Tag::new(b"hcla"), 0),
                (Tag::new(b"hcld"), 0),
                (Tag::new(b"hcof"), 0),
                (Tag::new(b"hcrn"), 0),
                (Tag::new(b"hcrs"), 0),
                (Tag::new(b"hdsc"), 0),
                (Tag::new(b"hlgp"), 0),
                (Tag::new(b"sbxo"), 0),
                (Tag::new(b"sbxs"), 0),
                (Tag::new(b"sbyo"), 0),
                (Tag::new(b"sbys"), 0),
                (Tag::new(b"spxo"), 0),
                (Tag::new(b"spxs"), 0),
                (Tag::new(b"spyo"), 0),
                (Tag::new(b"spys"), 0),
                (Tag::new(b"stro"), 0),
                (Tag::new(b"strs"), 0),
                (Tag::new(b"undo"), 0),
                (Tag::new(b"unds"), 0),
                (Tag::new(b"xhgt"), 0),
            ],
            vec![vec![5, -5]],
        );
    }

    fn assert_mvar_from_glyphs(mvar: write_fonts::read::tables::mvar::Mvar) {
        // This is almost the same font as MVAR.designspace, with the only difference
        // that the hhea caret slope rise and offset cannot be tested because Glyphs.app
        // has no custom parameters for them; only the hhea caret slope run 'hcrn' can
        // be indirectly tweaked via the italic angle.
        assert_mvar(
            mvar,
            vec![
                (Tag::new(b"cpht"), 0),
                (Tag::new(b"hasc"), 0),
                (Tag::new(b"hcla"), 0),
                (Tag::new(b"hcld"), 0),
                // (Tag::new(b"hcof"), ?),
                // (Tag::new(b"hcrs"), ?),
                (Tag::new(b"hcrn"), 1),
                (Tag::new(b"hdsc"), 0),
                (Tag::new(b"hlgp"), 0),
                (Tag::new(b"sbxo"), 0),
                (Tag::new(b"sbxs"), 0),
                (Tag::new(b"sbyo"), 0),
                (Tag::new(b"sbys"), 0),
                (Tag::new(b"spxo"), 0),
                (Tag::new(b"spxs"), 0),
                (Tag::new(b"spyo"), 0),
                (Tag::new(b"spys"), 0),
                (Tag::new(b"stro"), 0),
                (Tag::new(b"strs"), 0),
                (Tag::new(b"undo"), 0),
                (Tag::new(b"unds"), 0),
                (Tag::new(b"xhgt"), 0),
            ],
            vec![
                vec![5, -5],   // deltas for everything except 'hcrn'
                vec![87, -87], // 'hcrn' deltas, derived from the italic angle
            ],
        );
    }

    #[test]
    fn mvar_from_glyphs2() {
        let compile = TestCompile::compile_source("glyphs2/MVAR.glyphs");
        assert_mvar_from_glyphs(compile.font().mvar().unwrap());
    }

    #[test]
    fn mvar_from_glyphs3() {
        let compile = TestCompile::compile_source("glyphs3/MVAR.glyphs");
        assert_mvar_from_glyphs(compile.font().mvar().unwrap());
    }

    #[test]
    fn strikeout_size_fallback() {
        let compile =
            TestCompile::compile_source("glyphs3/GlobalMetrics_font_customParameters.glyphs");
        let strikeout_sizes = compile
            .fe_context
            .global_metrics
            .get()
            .iter()
            .filter_map(|(metric, values)| {
                if *metric == GlobalMetric::StrikeoutSize {
                    Some(values)
                } else {
                    None
                }
            })
            .flat_map(|v| v.values().copied())
            .collect::<HashSet<_>>();
        // Light has an explicit 40, everything else uses 42
        assert_eq!(
            HashSet::from([OrderedFloat(40.0), OrderedFloat(42.0)]),
            strikeout_sizes
        );
    }

    #[test]
    fn compile_ufo() {
        let result = TestCompile::compile_source("WghtVar-Bold.ufo");
        let font = result.font();
        let name = font.name().unwrap();

        // Expected values based on
        //  fontmake resources/testdata/WghtVar-Bold.ufo
        //  ttx -o - -t name master_ttf/WghtVar-Bold.ttf
        assert_eq!(
            vec![
                (NameId::FAMILY_NAME, "Wght Var".to_string()),
                (NameId::SUBFAMILY_NAME, "Regular".to_string()),
                (NameId::UNIQUE_ID, "0.000;NONE;WghtVar-Regular".to_string()),
                (NameId::FULL_NAME, "Wght Var Regular".to_string()),
                (NameId::VERSION_STRING, "Version 0.000".to_string()),
                (NameId::POSTSCRIPT_NAME, "WghtVar-Regular".to_string()),
            ],
            name.name_record()
                .iter()
                .map(|nr| (nr.name_id.get(), resolve_name(&name, nr.name_id()).unwrap()))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_empty_gvar_with_correct_axis_count() {
        // The test font contains a 'wght' axis and only 1 UFO source with a variable
        // FEA that varies the position of glyph 'A' based on the wght axis; the gvar
        // actually contains no glyph variations. Taken from
        // https://github.com/googlefonts/fontc/issues/815
        let result = TestCompile::compile_source("varpos.designspace");
        let font = result.font();

        let fvar = font.fvar().unwrap();
        let gvar = font.gvar().unwrap();

        // We assert that axis_count matches fvar's despite the gvar being no-op
        // (i.e. all glyph variations are empty)
        assert_eq!(fvar.axis_count(), 1);
        assert_eq!(fvar.axis_count(), gvar.axis_count());
        let glyph_count = font.maxp().unwrap().num_glyphs();
        assert_eq!(gvar.glyph_count(), glyph_count);
        assert!((0..glyph_count).all(|gid| gvar
            .glyph_variation_data(GlyphId16::new(gid).into())
            .unwrap()
            .is_none()));
    }

    // if a font has custom gdef categories defined, create the gdef table and
    // with the appropriate GlyphClassDef subtable, even if there's no GPOS/GSUB
    // or no other stuff in GDEF (... to match fontmake, see
    // <https://github.com/googlefonts/fontmake/issues/1120>)
    #[test]
    fn gdef_glyph_categories_without_any_layout() {
        let compile = TestCompile::compile_source("glyphs3/gdef_categories_no_layout.glyphs");
        let font = compile.font();
        let gdef = font.gdef().unwrap();
        let classdef = gdef.glyph_class_def().unwrap().unwrap();
        assert_eq!(classdef.iter().count(), 1)
    }

    // <https://github.com/googlefonts/fontc/issues/1008>
    #[test]
    fn no_names_for_instances_in_a_static_font() {
        let compile = TestCompile::compile_source("glyphs3/StaticWithInstance.glyphs");
        let font = compile.font();

        // <https://learn.microsoft.com/en-us/typography/opentype/spec/name#name-ids>
        let name = font.name().unwrap();
        let font_specific_names = name
            .name_record()
            .iter()
            .filter_map(|nr| {
                if nr.name_id().to_u16() > 255 {
                    Some((nr.name_id(), nr.string(name.string_data()).unwrap()))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        assert!(
            font_specific_names.is_empty(),
            "Should have no font specific names, got {font_specific_names:?}"
        );
    }

    // <https://github.com/googlefonts/fontc/issues/1022>
    // We used to mistakenly only count repeat use of the same component once
    #[test]
    fn duplicate_components_count_in_maxp() {
        let compile = TestCompile::compile_source("glyphs3/PixelRef.glyphs");
        let font = compile.font();
        let maxp = font.maxp().unwrap();
        // 3 uses of the "pixel" component which has 4 points
        assert_eq!(
            (12, 3),
            (
                maxp.max_composite_points().unwrap(),
                maxp.max_composite_contours().unwrap()
            )
        );
    }

    fn assert_expected_unicode_ranges(source: &str) {
        let compile = TestCompile::compile_source(source);
        let font = compile.font();
        let os2 = font.os2().unwrap();

        assert_eq!(
            [1 | (1 << 28), (1 << 20) | (1 << 24), 0, 1 << 26],
            [
                os2.ul_unicode_range_1(),
                os2.ul_unicode_range_2(),
                os2.ul_unicode_range_3(),
                os2.ul_unicode_range_4()
            ]
        );
    }

    #[test]
    fn obeys_source_unicode_ranges_glyphs() {
        assert_expected_unicode_ranges("glyphs3/WghtVar_OS2.glyphs");
    }

    #[test]
    fn obeys_source_unicode_ranges_designspace() {
        assert_expected_unicode_ranges("designspace_from_glyphs/WghtVarOS2.designspace");
    }

    fn assert_expected_codepage_ranges(source: &str) {
        let compile = TestCompile::compile_source(source);
        let font = compile.font();
        let os2 = font.os2().unwrap();

        assert_eq!(
            [Some(1 | (1 << 1) | (1 << 19)), Some(1 << 31)],
            [os2.ul_code_page_range_1(), os2.ul_code_page_range_2(),]
        );
    }

    #[test]
    fn obeys_source_codepage_ranges_glyphs() {
        assert_expected_codepage_ranges("glyphs3/WghtVar_OS2.glyphs");
    }

    #[test]
    fn obeys_source_codepage_ranges_designspace() {
        assert_expected_codepage_ranges("designspace_from_glyphs/WghtVarOS2.designspace");
    }

    #[test]
    fn bbox_of_nested_components() {
        let compile = TestCompile::compile_source("glyphs2/MatrixComponent.glyphs");
        let font = compile.font();
        let glyf = font.glyf().unwrap();
        let loca = font.loca(false).unwrap();

        // original glyph and transformed derivatives sketched in https://codepen.io/rs42/pen/wvVqVPL?editors=1000
        // "correct" values taken from fontmake compilation

        let expected_rot30_bbox = Rect::new(75.0, 107.0, 359.0, 300.0);
        let expected_rot60more_bbox = Rect::new(50.0, 149.0, 150.0, 449.0);

        let gids = ["rot30", "rot60more"]
            .into_iter()
            .map(|gn| {
                compile
                    .fe_context
                    .glyph_order
                    .get()
                    .glyph_id(gn)
                    .map(|gid16| GlyphId::new(gid16.to_u32()))
                    .unwrap()
            })
            .collect::<Vec<_>>();

        let boxes = gids
            .iter()
            .map(|gid| loca.get_glyf(*gid, &glyf).unwrap().unwrap())
            .map(|glyf| {
                Rect::new(
                    glyf.x_min() as f64,
                    glyf.y_min() as f64,
                    glyf.x_max() as f64,
                    glyf.y_max() as f64,
                )
            })
            .collect::<Vec<_>>();

        assert_eq!(vec![expected_rot30_bbox, expected_rot60more_bbox], boxes);
    }

    #[test]
    fn mostly_unique_names() {
        let compile = TestCompile::compile_source("glyphs3/DuplicateNames.glyphs");
        let font = compile.font();
        let name = font.name().unwrap();
        let fvar = font.fvar().unwrap();
        let stat = font.stat().unwrap();

        let axes = fvar.axes().unwrap().iter().collect::<Vec<_>>();
        let default_location = axes.iter().map(|a| a.default_value()).collect::<Vec<_>>();
        let named_instances = fvar
            .instances()
            .unwrap()
            .iter()
            .map(|ni| ni.unwrap())
            .collect::<Vec<_>>();

        // Default instance's subfamily name matches subfamily (nameID=2); it can/should share
        let default_instance_name_id = named_instances
            .iter()
            .find_map(|ni| (ni.coordinates == default_location).then_some(ni.subfamily_name_id))
            .unwrap();

        assert_eq!(NameId::SUBFAMILY_NAME, default_instance_name_id);
        assert_eq!(
            "Regular".to_string(),
            resolve_name(&name, default_instance_name_id).unwrap()
        );

        // Non-default instance's subfamily name is only allowed to reuse names from
        // the font-specific nameID range >= 256
        let min_font_specific_name_id = NameId::new(256);
        let non_default_instance_name_id = named_instances
            .iter()
            .find_map(|ni| (ni.coordinates != default_location).then_some(ni.subfamily_name_id))
            .unwrap();

        assert!(non_default_instance_name_id >= min_font_specific_name_id);
        assert_eq!(
            "Bold".to_string(),
            resolve_name(&name, non_default_instance_name_id).unwrap()
        );

        // Similarly fvar/STAT axis names aren't allowed to share spec-reserved nameIDs
        // (despite this particular axis was artificially named "Regular" to show this)
        let axis_name_id = axes.first().map(|a| a.axis_name_id()).unwrap();

        assert!(axis_name_id >= min_font_specific_name_id);
        assert_eq!(
            "Regular".to_string(),
            resolve_name(&name, axis_name_id).unwrap()
        );

        let design_axis_name_id = stat
            .design_axes()
            .unwrap()
            .first()
            .map(|a| a.axis_name_id())
            .unwrap();

        assert_eq!(design_axis_name_id, axis_name_id);
    }

    #[test]
    fn allow_duplicate_components() {
        // This used to crash us, <https://github.com/googlefonts/fontc/issues/1115>
        TestCompile::compile_source("DoubleComponentError/OuterInner.designspace");
    }

    #[test]
    fn merge_stylistic_set_names() {
        let result = TestCompile::compile_source("glyphs3/WghtVarWithStylisticSet.glyphs");
        let font = result.font();
        let name = font.name().unwrap();

        let names = name
            .name_record()
            .iter()
            .map(|rec| (rec.name_id(), resolve_name(&name, rec.name_id()).unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(
            names,
            [
                (NameId::COPYRIGHT_NOTICE, "Copy!".to_owned()),
                (NameId::FAMILY_NAME, "WghtVar".to_owned()),
                (NameId::SUBFAMILY_NAME, "Regular".to_owned()),
                (NameId::UNIQUE_ID, "1.234;NONE;WghtVar-Regular".to_owned()),
                (NameId::FULL_NAME, "WghtVar Regular".to_owned()),
                (NameId::VERSION_STRING, "Version 1.234".to_owned()),
                (NameId::POSTSCRIPT_NAME, "WghtVar-Regular".to_owned()),
                (NameId::DESCRIPTION, "The greatest weight var".to_owned()),
                (
                    NameId::LICENSE_URL,
                    "https://example.com/my/font/license".to_owned()
                ),
                // axis & instance names
                (NameId::new(256), "Weight".to_owned()),
                // from FEA, merged after names of axes and named instances
                (NameId::new(257), "my fun feature".to_owned()),
            ]
        );

        let gsub = font.gsub().unwrap();
        let feature_list = gsub.feature_list().unwrap();
        assert_eq!(feature_list.feature_records().len(), 2);
        let feature = feature_list.feature_records()[0]
            .feature(feature_list.offset_data())
            .unwrap();
        let params = feature.feature_params().unwrap().unwrap();
        let FeatureParams::StylisticSet(params) = params else {
            panic!("wrong feature params type");
        };

        // matches what is in the name table
        assert_eq!(params.ui_name_id().to_u16(), 257);
    }

    #[test]
    fn skip_empty_stylistic_set_names() {
        let result = TestCompile::compile_source("glyphs3/WghtVarWithStylisticSet.glyphs");
        let font = result.font();
        let gsub = font.gsub().unwrap();
        let feature_list = gsub.feature_list().unwrap();
        assert_eq!(feature_list.feature_records().len(), 2);
        let ss02 = feature_list.feature_records()[1]
            .feature(feature_list.offset_data())
            .unwrap();
        assert!(ss02.feature_params().is_none());
    }

    #[test]
    fn merge_name_table_from_fea() {
        let result = TestCompile::compile_source("CustomNameTableInFea.ufo");
        let font = result.font();
        let name = font.name().unwrap();

        let names = name
            .name_record()
            .iter()
            .map(|rec| (rec.name_id(), resolve_name(&name, rec.name_id()).unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(
            names,
            [
                (NameId::FAMILY_NAME, "Wght Var".to_string()),
                (NameId::SUBFAMILY_NAME, "Regular".to_string()),
                (NameId::UNIQUE_ID, "0.000;NONE;WghtVar-Regular".to_string()),
                (NameId::FULL_NAME, "set from FEA name table".to_string()),
                (NameId::VERSION_STRING, "Version 0.000".to_string()),
                (NameId::POSTSCRIPT_NAME, "WghtVar-Regular".to_string()),
                (NameId::DESIGNER, "Joachim Müller-Lancé".to_string()),
            ]
        );
    }

    #[test]
    fn use_base_table_from_fea() {
        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("CustomBaseTableInFea.ufo");
        let font = result.font();
        let base = font.base().unwrap();
        let horiz = base.horiz_axis().unwrap().unwrap();
        let horiz_tags = horiz.base_tag_list().unwrap().unwrap().baseline_tags();
        assert_eq!(horiz_tags, [Tag::new(b"ideo")]);
        let horiz_scripts = horiz.base_script_list().unwrap();
        assert_eq!(horiz_scripts.base_script_count(), 1);

        let vert = base.vert_axis().unwrap().unwrap();
        let vert_tags = vert.base_tag_list().unwrap().unwrap().baseline_tags();
        assert_eq!(vert_tags, [Tag::new(b"romn")]);
        let vert_scripts = vert.base_script_list().unwrap();
        assert_eq!(vert_scripts.base_script_count(), 2);
    }

    #[test]
    fn use_stat_table_from_fea() {
        let result = TestCompile::compile_source("CustomStatInFea.ufo");
        let font = result.font();
        let name = font.name().unwrap();

        let names = name
            .name_record()
            .iter()
            .map(|rec| (rec.name_id(), resolve_name(&name, rec.name_id()).unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(
            names,
            [
                (NameId::FAMILY_NAME, "New Font".to_string()),
                (NameId::SUBFAMILY_NAME, "Regular".to_string()),
                (NameId::UNIQUE_ID, "0.000;NONE;NewFont-Regular".to_string()),
                (NameId::FULL_NAME, "New Font Regular".to_string()),
                (NameId::VERSION_STRING, "Version 0.000".to_string()),
                (NameId::POSTSCRIPT_NAME, "NewFont-Regular".to_string()),
                // these names come from STAT
                (NameId::new(256), "Regular".to_string()),
                (NameId::new(257), "Italic".to_string()),
                (NameId::new(258), "Upright".to_string()),
                (NameId::new(259), "Italic".to_string()),
            ]
        );

        let stat = font.stat().unwrap();
        let design_axes = stat.design_axes().unwrap();
        assert_eq!(
            design_axes
                .iter()
                .map(|axis| axis.axis_tag())
                .collect::<Vec<_>>(),
            [Tag::new(b"ital")]
        );

        let axis_values = stat.offset_to_axis_values().unwrap().unwrap();
        assert_eq!(axis_values.axis_values().len(), 2);
    }

    #[test]
    fn generate_meta_table() {
        let result = TestCompile::compile_source("glyphs3/MetaTable.glyphs");
        let meta = result.be_context.meta.get();
        assert_eq!(
            meta.data_maps,
            [
                DataMapRecord::new(
                    Tag::new(b"dlng"),
                    Metadata::ScriptLangTags(vec![ScriptLangTag::new("latn-en".into()).unwrap()]),
                ),
                DataMapRecord::new(
                    Tag::new(b"slng"),
                    Metadata::ScriptLangTags(vec![
                        ScriptLangTag::new("latn".into()).unwrap(),
                        ScriptLangTag::new("derp".into()).unwrap()
                    ]),
                )
            ]
        );
    }

    #[test]
    fn dont_generate_meta_table_if_no_glyphs_param() {
        let result = TestCompile::compile_source("glyphs3/NoMetaTable.glyphs");
        assert!(result.be_context.meta.try_get().is_none())
    }

    #[test]
    fn dont_generate_meta_table_if_empty_glyphs_param() {
        let result = TestCompile::compile_source("glyphs3/EmptyMetaTable.glyphs");
        assert!(result.be_context.meta.try_get().is_none())
    }

    #[test]
    fn cpal_grayscale() {
        let result = TestCompile::compile_source("glyphs3/COLRv1-grayscale.glyphs");
        let cpal = result.font().cpal().unwrap();
        assert_eq!(
            (
                1,
                2,
                [
                    ColorRecord {
                        red: 0,
                        green: 0,
                        blue: 0,
                        alpha: 255
                    },
                    ColorRecord {
                        red: 64,
                        green: 64,
                        blue: 64,
                        alpha: 255
                    }
                ]
                .as_slice()
            ),
            (
                cpal.num_palettes(),
                cpal.num_palette_entries(),
                cpal.color_records_array().unwrap().unwrap()
            )
        );
    }

    #[test]
    fn cpal_color() {
        let result = TestCompile::compile_source("glyphs3/COLRv1-simple.glyphs");
        let cpal = result.font().cpal().unwrap();
        assert_eq!(
            (
                1,
                2,
                [
                    ColorRecord {
                        red: 0,
                        green: 0,
                        blue: 255,
                        alpha: 255
                    },
                    ColorRecord {
                        red: 255,
                        green: 0,
                        blue: 0,
                        alpha: 255
                    }
                ]
                .as_slice()
            ),
            (
                cpal.num_palettes(),
                cpal.num_palette_entries(),
                cpal.color_records_array().unwrap().unwrap()
            )
        );
    }

    #[test]
    fn color_me_not() {
        let compile = TestCompile::compile_source("glyphs3/WghtVar.glyphs");
        assert!(compile.font().cpal().is_err());
        assert!(compile.font().colr().is_err());
    }

    #[test]
    fn colr_grayscale() {
        let result = TestCompile::compile_source("glyphs3/COLRv1-grayscale.glyphs");
        result.font().colr().unwrap(); // for now just check the table exists
    }

    #[rstest]
    #[case("glyphs2/IncompatibleUnexported.glyphs")]
    #[case("glyphs3/IncompatibleUnexported.glyphs")]
    #[case("designspace_from_glyphs/IncompatibleUnexported.designspace")]
    fn skip_unexported_glyph_anchors(#[case] source: &str) {
        let result = TestCompile::compile_source(source);
        assert!(result.contains_glyph("A"));
        assert!(result.contains_glyph("space"));
        assert!(!result.contains_glyph("doesnt_matter"));
    }

    #[test]
    fn feature_variation_lookup_order_like_fontmake() {
        // when we generate lookups from designspace rules, we want the lookups
        // (in the lookup list) to be ordered like fontmake, which means sorted
        // by the glyphnames of the rules, not the GIDs.

        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("dspace_rules/Basic.designspace");
        let gsub = result.font().gsub().unwrap();
        let features = gsub.feature_list().unwrap();
        let all_referenced_lookups = features
            .feature_records()
            .iter()
            .flat_map(|rec| {
                rec.feature(features.offset_data())
                    .unwrap()
                    .lookup_list_indices()
            })
            .map(|x| x.get())
            .collect::<HashSet<_>>();
        let lookups = gsub.lookup_list().unwrap();

        // these are the lookups referenced via the feature substitution
        assert!(!all_referenced_lookups.contains(&0));
        assert!(!all_referenced_lookups.contains(&1));

        let one = lookups.lookups().get(0).unwrap();
        let two = lookups.lookups().get(1).unwrap();

        fn get_first_gid_or_die_trying(lookup: SubstitutionLookup) -> GlyphId16 {
            match lookup {
                SubstitutionLookup::Single(sub) => match sub.subtables().get(0).unwrap() {
                    SingleSubst::Format1(sub) => sub.coverage().unwrap().iter().next().unwrap(),
                    SingleSubst::Format2(sub) => sub.coverage().unwrap().iter().next().unwrap(),
                },
                _ => panic!("oops"),
            }
        }

        let one_gid = get_first_gid_or_die_trying(one);
        let two_gid = get_first_gid_or_die_trying(two);

        assert_eq!(result.get_gid("bar"), one_gid);
        assert_eq!(result.get_gid("plus"), two_gid);
    }

    #[test]
    fn rvrn_aalt_lookup_order() {
        // do we put lookups in the right order?
        // - aalt goes at the front
        // - except for rvrn, which goes at the front-front
        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("dspace_rules/Basic.designspace");
        let gsub = result.font().gsub().unwrap();
        let feature_list = gsub.feature_list().unwrap();
        assert_eq!(feature_list.feature_records().len(), 3);
        assert_eq!(
            feature_list
                .feature_records()
                .iter()
                .map(|rec| rec.feature_tag())
                .collect::<Vec<_>>(),
            ["aalt", "rvrn", "salt"]
        );
        let salt = feature_list.feature_records()[2];
        let aalt = feature_list.feature_records()[0];
        // rvrn is 0 & 1
        // aalt is 2
        // salt is 3
        assert_eq!(
            aalt.feature(feature_list.offset_data())
                .unwrap()
                .lookup_list_indices(),
            [2]
        );
        assert_eq!(
            salt.feature(feature_list.offset_data())
                .unwrap()
                .lookup_list_indices(),
            [3]
        );
    }

    fn get_first_feature_substitution(
        gsub: write_fonts::read::tables::gsub::Gsub,
        expected_sub_idx: usize,
    ) -> write_fonts::read::tables::layout::Feature {
        let featvars = gsub.feature_variations().unwrap().unwrap();
        let var_rec = featvars.feature_variation_records()[0];
        let feature_substitution = var_rec
            .feature_table_substitution(featvars.offset_data())
            .unwrap()
            .unwrap();

        let sub_record = feature_substitution.substitutions()[0];

        assert_eq!(sub_record.feature_index() as usize, expected_sub_idx);
        sub_record
            .alternate_feature(feature_substitution.offset_data())
            .unwrap()
    }

    #[test]
    fn designspace_rvrn_feature_variation() {
        // do we create a feature variation record, pointing at the expected values?
        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("dspace_rules/Basic.designspace");
        let gsub = result.font().gsub().unwrap();
        let feature_list = gsub.feature_list().unwrap();
        let rvrn_idx = feature_list
            .feature_records()
            .iter()
            .position(|rec| rec.feature_tag() == "rvrn")
            .unwrap();
        let rvrn = feature_list.feature_records()[rvrn_idx];
        // base rvrn feature has no lookups
        assert!(rvrn
            .feature(feature_list.offset_data())
            .unwrap()
            .lookup_list_indices()
            .is_empty());

        let rvrn_replacement = get_first_feature_substitution(gsub, rvrn_idx);
        // rvrn at the front
        assert_eq!(rvrn_replacement.lookup_list_indices(), [0, 1]);
    }

    #[test]
    fn glyphs_feature_variations() {
        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("glyphs3/LibreFranklin-bracketlayer.glyphs");
        let gsub = result.font().gsub().unwrap();
        let lookup_list = gsub.lookup_list().unwrap();
        assert_eq!(lookup_list.lookup_count(), 1);
        let feature_list = gsub.feature_list().unwrap();
        // by default, glyphsLib puts feature variations in rvrn
        let rvrn_idx = feature_list
            .feature_records()
            .iter()
            .position(|rec| rec.feature_tag() == "rvrn")
            .unwrap();
        let rvrn = feature_list.feature_records()[rvrn_idx];
        // base rvrn feature has no lookups
        assert!(rvrn
            .feature(feature_list.offset_data())
            .unwrap()
            .lookup_list_indices()
            .is_empty());

        let rvrn_replacement = get_first_feature_substitution(gsub, rvrn_idx);
        assert_eq!(rvrn_replacement.lookup_list_indices(), [0]);
    }

    #[test]
    fn glyphs_feature_variations_custom_feature() {
        // honor the 'Feature for Feature Variations' key
        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("glyphs2/WorkSans-minimal-bracketlayer.glyphs");
        let gsub = result.font().gsub().unwrap();
        let lookup_list = gsub.lookup_list().unwrap();
        assert_eq!(lookup_list.lookup_count(), 1);
        let feature_list = gsub.feature_list().unwrap();
        assert_eq!(feature_list.feature_records().len(), 1);
        assert_eq!(feature_list.feature_records()[0].feature_tag(), "rclt");

        let replacement = get_first_feature_substitution(gsub, 0);
        assert_eq!(replacement.lookup_list_indices(), [0]);
    }

    #[test]
    fn glyphs_fea_include_file() {
        // ensure that we resolve inclue statements in glyphs sources
        let _ = env_logger::builder().is_test(true).try_init();
        let result = TestCompile::compile_source("glyphs_fea_include/glyphs_include.glyphs");
        let gsub = result.font().gsub().unwrap();
        let feature_list = gsub.feature_list().unwrap();
        assert_eq!(feature_list.feature_records().len(), 1);
        assert_eq!(feature_list.feature_records()[0].feature_tag(), "test");
    }

    #[rstest]
    #[case("glyphs3/InstancePostscript.glyphs")]
    #[case("glyphs2/InstancePostscript.glyphs")]
    fn compile_instance_postscript_names(#[case] source: &str) {
        let result = TestCompile::compile_source(source);
        let font = result.font();

        let fvar = font.fvar().unwrap();
        let name = font.name().unwrap();

        let actual = fvar
            .instances()
            .unwrap()
            .iter()
            .map(|instance| {
                instance
                    .unwrap()
                    .post_script_name_id
                    .map(|id| resolve_name(&name, id).unwrap())
            })
            .collect::<Vec<_>>();

        assert_eq!(
            actual,
            vec![
                None,
                Some("PostMedium".to_string()),
                Some("PostBold".to_string())
            ]
        );
    }

    #[test]
    fn dont_compile_instance_postscript_names_if_none() {
        let result = TestCompile::compile_source("glyphs3/InstanceNoPostscript.glyphs");
        let font = result.font();

        let fvar = font.fvar().unwrap();

        assert!(fvar
            .instances()
            .unwrap()
            .iter()
            .all(|instance| instance.unwrap().post_script_name_id.is_none()));
    }

    #[rstest]
    #[case::yes(true)]
    #[case::no(false)]
    fn rename_glyphs_to_production_names(#[case] use_adobe_style_post_names: bool) {
        let result = TestCompile::compile("glyphs3/ProductionNames.glyphs", |mut args| {
            args.no_production_names = !use_adobe_style_post_names;
            args
        });

        let raw_post = dump_table(result.be_context.post.get().as_ref()).unwrap();
        let post = Post::read(FontData::new(&raw_post)).unwrap();

        if use_adobe_style_post_names {
            assert_eq!(post.glyph_name(result.get_gid("A")), Some("A"));
            assert_eq!(post.glyph_name(result.get_gid("idotless")), Some("uni0131"));
            assert_eq!(post.glyph_name(result.get_gid("nbspace")), Some("uni00A0"));
            // We don't check if user-defined prod names actually follow AGL rules;
            // e.g. in this case 'uni' prefix is NOT followed by groups of 4 hex digits
            assert_eq!(
                post.glyph_name(result.get_gid("invalid_prod_name")),
                Some("uni99")
            );
            // We just strip invalid PS characters (e.g. hyphens)
            assert_eq!(
                post.glyph_name(result.get_gid("foobar-cy")),
                Some("foobarcy")
            );
            // number suffixes are added to avoid duplicates
            assert_eq!(
                post.glyph_name(result.get_gid("foobarcy")),
                Some("foobarcy.1")
            );
            assert_eq!(
                post.glyph_name(result.get_gid("foobarcy.1")),
                Some("foobarcy.1.1")
            );
            assert_eq!(
                post.glyph_name(result.get_gid("duplicate.1")),
                Some("uniF8FF")
            );
            assert_eq!(
                post.glyph_name(result.get_gid("duplicate.2")),
                Some("uniF8FF.1")
            );
            assert_eq!(
                post.glyph_name(result.get_gid("A_nbspace_idotless")),
                Some("uni004100A00131")
            );
            assert_eq!(
                post.glyph_name(result.get_gid("A_nbspace_idotless.ss01")),
                Some("uni004100A00131.ss01")
            );
        } else {
            // original names are preserved
            let glyph_order = result.fe_context.glyph_order.get();
            for name in glyph_order.iter().map(|(_, name)| name.as_str()) {
                assert_eq!(post.glyph_name(result.get_gid(name)), Some(name));
            }
        }
    }

    #[rstest]
    #[case("glyphs3/DontUseProductionNames.glyphs")]
    #[case("glyphs2/DontUseProductionNames.glyphs")]
    #[case("designspace_from_glyphs/DontuseProductionNames.designspace")]
    fn dont_use_production_names_custom_parameter(#[case] source: &str) {
        // these sources have the "Don't use Production Names" custom parameter set to true
        // (or the ufo2ft equivalent "useProductionNames" lib key set to false).
        let result = TestCompile::compile_source(source);
        let font = result.font();
        let post = font.post().unwrap();

        // this would have been 'uni00A0' if glyphs had been renamed to production names
        assert_eq!(post.glyph_name(result.get_gid("nbspace")), Some("nbspace"));
    }

    /// End-to-end test that `vhea` is built correctly.
    #[rstest]
    #[case("glyphs2/Vertical.glyphs")]
    #[case("Vertical.ufo")]
    fn compile_vertical_typesetting_header(#[case] source: &str) {
        let result = TestCompile::compile_source(source);
        let font = result.font();

        let maxp = font.maxp().unwrap();
        let vhea = font.vhea().expect("should include vertical table");

        // Explicit global metrics in sources.
        assert_eq!(vhea.ascender().to_i16(), 3456);
        assert_eq!(vhea.descender().to_i16(), -789);
        assert_eq!(vhea.line_gap().to_i16(), 12);

        assert_eq!(vhea.caret_slope_rise(), 500);
        assert_eq!(vhea.caret_slope_run(), 1000);
        assert_eq!(vhea.caret_offset(), 250);

        // Derived from glyph metrics; all glyphs need full vertical metrics
        // except the last one, as it shares it predecessor's advance.
        assert_eq!(vhea.number_of_long_ver_metrics(), maxp.num_glyphs() - 1);

        // Derived from glyph metrics and outlines. To make the calculations
        // below more obvious, 'a' has been given extreme values to take the
        // crown for all of them.
        let big_advance = 3333;
        let big_v_origin = 2500;
        let (big_y_max, big_y_min) = (8000, -4000);

        // 'a' has the largest advance height.
        assert_eq!(vhea.advance_height_max().to_u16(), big_advance);

        // 'a' has outlines highest above its vertical origin.
        assert_eq!(
            vhea.min_top_side_bearing().to_i16(),
            big_v_origin - big_y_max
        );

        // 'a' has outlines lowest below its advance-offset origin.
        assert_eq!(
            vhea.min_bottom_side_bearing().to_i16(),
            big_y_min - (big_v_origin - big_advance as i16)
        );

        // 'a' has outlines lowest below its vertical origin.
        assert_eq!(vhea.y_max_extent().to_i16(), big_v_origin - big_y_min);
    }

    /// End-to-end test that `vmtx` is built correctly.
    #[rstest]
    #[case("glyphs2/Vertical.glyphs")]
    #[case("Vertical.ufo")]
    fn compile_vertical_typesetting_metrics(#[case] source: &str) {
        let result = TestCompile::compile_source(source);
        let font = result.font();
        let vmtx = font.vmtx().expect("should include vertical table");

        // Global *horizontal* metrics of test case.
        let (ascender, descender) = (1234, -567);
        let typo_asc = 2345;

        // Derived default values.
        let notdef_height = (ascender - descender) as u16;
        let notdef_y_max = ascender;

        let default_vert_origin = typo_asc;
        let empty_y_max = 0;

        // Assert that long metrics are correct.
        assert_eq!(
            vmtx.v_metrics()
                .iter()
                .map(|long_metric| (long_metric.advance(), long_metric.side_bearing()))
                .collect::<Vec<_>>(),
            vec![
                // '.notdef', auto-generated: explicit height, default vertical origin.
                (notdef_height, default_vert_origin - notdef_y_max),
                // 'a': explicit height, explicit vertical origin and yMax of 8000.
                (3333, 2500 - 8000),
                // 'b': explicit height, default vertical origin with no outlines.
                (0, default_vert_origin - empty_y_max),
                // 'c': explicit height, default vertical origin with no outlines.
                (2000, default_vert_origin - empty_y_max)
            ]
        );

        // Assert that accompanying bearing for implicit long metric is correct.
        assert_eq!(
            vmtx.top_side_bearings()
                .iter()
                .map(|value| value.get())
                .collect::<Vec<_>>(),
            vec![
                // 'd': same explicit height as 'c', default vertical origin with no outlines
                default_vert_origin - empty_y_max
            ]
        );
    }

    #[test]
    fn bracket_glyphs() {
        let result = TestCompile::compile_source("glyphs3/LibreFranklin-bracketlayer.glyphs");
        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        assert_eq!(glyphs.len(), 5); // includes NOTDEF

        let yen_gid = result.get_gid("yen");
        let yen_bracket_gid = result.get_gid("yen.BRACKET.varAlt01");
        let peso_gid = result.get_gid("peso");
        let peso_bracket_gid = result.get_gid("peso.BRACKET.varAlt01");

        assert!([yen_gid, yen_bracket_gid, peso_gid, peso_bracket_gid]
            .iter()
            .all(|gid| *gid > GlyphId16::NOTDEF));

        fn get_component_gids(glyph: &glyf::Glyph) -> Vec<GlyphId16> {
            match glyph {
                glyf::Glyph::Simple(_) => panic!("not a composite"),
                glyf::Glyph::Composite(g) => g.components().map(|comp| comp.glyph).collect(),
            }
        }

        let yen = glyphs[yen_gid.to_u32() as usize].as_ref().unwrap();
        let yen_bracket = glyphs[yen_bracket_gid.to_u32() as usize].as_ref().unwrap();
        assert_eq!(get_component_gids(yen), [peso_gid]);
        assert_eq!(get_component_gids(yen_bracket), [peso_bracket_gid]);
    }
}
